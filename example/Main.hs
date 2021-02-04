module Main where

import           Spaces
import           Spaces.Brick.UI
import           Spaces.CovT
import           Spaces.Day
import           Spaces.Optional
import           Spaces.Tagged

import           Brick
import           Control.Comonad.Env
import           Control.Comonad.Store
import           Control.Comonad.Traced
import           Control.Concurrent     (threadDelay)
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Functor.Day
import           Data.Monoid            (Any (..), Sum (..))
import qualified Data.Text              as T
import           Data.Thyme             (UTCTime (..), getCurrentTime)
import           Data.Void
import           Graphics.Vty           (Event (..), Key (..))

data TimeInfo =
  TimeInfo
    { currentTime :: UTCTime
    , startTime   :: UTCTime
    }
  deriving (Eq, Show)

beginTimeInfo :: IO TimeInfo
beginTimeInfo = (\t -> TimeInfo t t) <$> getCurrentTime

updateTimeInfo ::
     (InProduct "time" f ks fs, MonadState TimeInfo (CovT f m))
  => UTCTime
  -> CovT (TaggedProduct ks fs) m ()
updateTimeInfo cT = taggedCovT #time (modify (\ti -> ti {currentTime = cT}))

displayTime :: TimeInfo -> BrickSpace (Store TimeInfo)
displayTime =
  store $ \ti ->
    displayBrickUI
      (str ("Current Time: " <> show (currentTime ti)) <=>
       str ("Start time: " <> show (startTime ti)))

quitOnQ :: Comonad w => BrickSpace w -> BrickSpace (TracedT Any w)
quitOnQ inner = TracedT (helper <$> inner)
  where
    helper disp (Any b) =
      disp
        { brickUIShouldStop = b || brickUIShouldStop disp
        , brickUIStep =
            \e ->
              covHoistW lower (brickUIStep disp e) *>
              when (e == VtyEvent (EvKey (KChar 'q') [])) (tell $ Any True)
        }

countChar :: Char -> BrickSpace (Traced (Sum Int))
countChar c =
  traced $ \(Sum n) ->
    BrickUI
      { brickUIDisplay =
          str ("You have pressed " <> show c <> " " <> show n <> " times.")
      , brickUIStep = \case
          VtyEvent (EvKey (KChar c') []) | c == c' -> tell $ Sum 1
          _ -> return ()
      , brickUIShouldStop = False
      }

foo ti =
  withTaggedSpace #time (displayTime ti) $
  withTaggedSpace #countB (countChar 'b') $
  withTaggedSpace #countA (countChar 'a') $ emptyTaggedSpace

main = do
  let helper f =
        forever $ do
          ti <- getCurrentTime
          f $ covHoistW lower $ updateTimeInfo ti
          threadDelay $ floor 1e6
  startTi <- beginTimeInfo
  runBrickSpaceUI helper (quitOnQ $ foo startTi)
