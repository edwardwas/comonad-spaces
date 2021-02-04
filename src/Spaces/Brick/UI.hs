module Spaces.Brick.UI where

import           Spaces
import           Spaces.CovT

import           Brick
import           Brick.BChan
import           Control.Comonad
import           Control.Concurrent.Async
import           Control.Lens             hiding ((<.>))
import           Data.Functor.Apply
import           Data.Void
import           Graphics.Vty             (Vty (..), mkVty, userConfig)
import           Graphics.Vty.Attributes

mapBrickEvent :: (a -> b) -> BrickEvent n a -> BrickEvent n b
mapBrickEvent f (AppEvent a)         = AppEvent $ f a
mapBrickEvent _ (VtyEvent e)         = VtyEvent e
mapBrickEvent f (MouseDown n b ms l) = MouseDown n b ms l
mapBrickEvent f (MouseUp n b l)      = MouseUp n b l

splitBrickEvent :: BrickEvent n e -> Either e (BrickEvent n Void)
splitBrickEvent (AppEvent e)         = Left e
splitBrickEvent (VtyEvent v)         = Right $ VtyEvent v
splitBrickEvent (MouseDown n b ms l) = Right $ MouseDown n b ms l
splitBrickEvent (MouseUp n b l)      = Right $ MouseUp n b l

-- | The view type for brick.
data BrickUI a =
  BrickUI
    { brickUIDisplay    :: Widget ()
    -- ^ The widget to display
    , brickUIStep       :: BrickEvent () Void -> a
    -- ^ How to handle a single event
    , brickUIShouldStop :: Bool
    -- ^ If the program should exit
    } deriving (Functor)

displayBrickUI :: Applicative m => Widget () -> BrickUI (m ())
displayBrickUI widget =
  BrickUI
    { brickUIDisplay = widget
    , brickUIStep = \_ -> pure ()
    , brickUIShouldStop = False
    }

combineBrickUI ::
     (Widget () -> Widget () -> Widget ())
  -> BrickUI (a -> b)
  -> BrickUI a
  -> BrickUI b
combineBrickUI f b1 b2 =
  BrickUI
    { brickUIDisplay = f (brickUIDisplay b1) (brickUIDisplay b2)
    , brickUIStep = brickUIStep b1 <*> brickUIStep b2
    , brickUIShouldStop = brickUIShouldStop b1 || brickUIShouldStop b2
    }

instance Applicative BrickUI where
  pure a =
    BrickUI
      { brickUIDisplay = emptyWidget
      , brickUIStep = \_ -> a
      , brickUIShouldStop = False
      }
  f <*> a = combineBrickUI (<=>) f a

newtype HorizontalBrickUI a = HorizontalBrickUI (BrickUI a)
  deriving (Functor)
makeWrapped ''HorizontalBrickUI

instance Applicative HorizontalBrickUI  where
  pure = HorizontalBrickUI . pure
  HorizontalBrickUI f <*> HorizontalBrickUI a =
    HorizontalBrickUI (combineBrickUI (<+>) f a)

type BrickSpace w = Space (EventM ()) w BrickUI
type BrickAction w = CovT w (EventM ()) ()

spaceUIApp :: Comonad w => App (BrickSpace w) (BrickAction w) ()
spaceUIApp =
  let step w e = do
        w' <-
          case splitBrickEvent e of
            Right event -> moveCovT ((brickUIStep $ extract w) event) w
            Left action -> moveCovT action w
        if brickUIShouldStop (extract w')
          then halt w'
          else continue w'
   in App
        { appDraw = pure . brickUIDisplay . extract
        , appChooseCursor = \_ _ -> Nothing
        , appStartEvent = pure
        , appAttrMap = \_ -> attrMap defAttr []
        , appHandleEvent = step
        }

-- | Run a brikc UI space. The first action is a callback allowing effects
runBrickSpaceUI ::
     Comonad w
  => ((BrickAction w -> IO ()) -> IO a)
  -> BrickSpace w
  -> IO (BrickSpace w)
runBrickSpaceUI withChan space = do
  let createVty = userConfig >>= mkVty
  startVty <- createVty
  chan <- newBChan 1
  eventFiller <- async $ withChan (writeBChan chan)
  customMain startVty createVty (Just chan) spaceUIApp space <*
    cancel eventFiller
