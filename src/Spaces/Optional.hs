module Spaces.Optional where

import           Spaces.CovT

import           Control.Comonad
import           Control.Comonad.Trans.Class

data Optional w a = Optional Bool a (w a)
  deriving (Functor)

instance Comonad w => Comonad (Optional w) where
  extract (Optional True a _)   = a
  extract (Optional False _ wa) = extract wa
  extend f (Optional b a wa) =
    Optional b (f $ Optional b a wa) (extend (f . Optional False a) wa)

instance ComonadTrans Optional where
  lower (Optional _ _ w) = w
