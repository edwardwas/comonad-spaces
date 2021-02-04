module Spaces where

import           Spaces.CovT

import           Control.Comonad
import           Data.Void

type Space m w ui = w (ui (CovT w m ()))

stepSpace ::
     (Monad m, Comonad w)
  => (forall x. ui x -> m x)
  -> Space m w ui
  -> m (Space m w ui)
stepSpace runner space = do
  action <- runner (extract space)
  moveCovT action space

runSpace ::
     (Monad m, Comonad w) => (forall x. ui x -> m x) -> Space m w ui -> m Void
runSpace runner space = stepSpace runner space >>= runSpace runner
