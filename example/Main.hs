module Main where

import           Spaces
import           Spaces.CovT
import           Spaces.Day

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
import           Data.Void

countCharUI :: Char -> Space IO (Traced (Sum Int)) SimpleUI
countCharUI c =
  traced $ \(Sum n) ->
    SimpleUI ["Count of " <> [c] <> " = " <> show n] $ \input ->
      when ([c] == input) (tell $ Sum 1)

masterUI =
  composeSpace (countCharUI 'a') $
  composeSpace (countCharUI 'b') (countCharUI 'c')

main = do
  runSimpleUI masterUI
  return ()
