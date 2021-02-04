module Main where

import           Spaces
import           Spaces.Brick.UI
import           Spaces.CovT
import           Spaces.Day

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

foo = composeSpace (countChar 'a') (countChar 'b')

main :: IO ()
main = do
  runBrickSpaceUI (\_ -> return ()) (quitOnQ foo )
  return ()
