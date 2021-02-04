module Spaces.Focus where

import           Spaces
import           Spaces.CovT

import           Control.Comonad
import           Control.Comonad.Trans.Class
import           Data.Functor.Identity
import           Data.List.NonEmpty          (NonEmpty (..))
import           Data.Maybe

data HasFocus = HasFocus | DoesNotHaveFocus
  deriving (Eq,Show)

data FocusedT w a =
  FocusedT [HasFocus -> w a] (HasFocus -> w a) [HasFocus -> w a]
  deriving (Functor)

focusedTToList :: FocusedT w a -> [w a]
focusedTToList (FocusedT p c f) =
  map ($ DoesNotHaveFocus) (reverse p) <>
  ((c HasFocus) : (map ($ DoesNotHaveFocus) f))

createFocused :: NonEmpty (HasFocus -> w a) -> FocusedT w a
createFocused (a :| as) = FocusedT [] a as

iterateWhileTrue :: (a -> Maybe a) -> a -> [a]
iterateWhileTrue f a =
  let next =
        case f a of
          Just b  -> iterateWhileTrue f b
          Nothing -> []
   in a : next

moveFocusedBack :: FocusedT w a -> Maybe (FocusedT w a)
moveFocusedBack (FocusedT (p:ps) c fs) = Just $ FocusedT ps p (c : fs)
moveFocusedBack _                      = Nothing

moveFocusedForewared :: FocusedT w a -> Maybe (FocusedT w a)
moveFocusedForewared (FocusedT ps c (f:fs)) = Just $ FocusedT (c : ps) f fs
moveFocusedForewared _                      = Nothing

instance Comonad w => Comonad (FocusedT w) where
  extract (FocusedT _ w _) = extract (w HasFocus)
  duplicate focus@(FocusedT p c f) =
    let tailHelper []     = []
        tailHelper (a:as) = as
        moveHelper m =
          zipWith
            (\focus func hf -> focus <$ func hf)
            (iterateWhileTrue m focus)
     in FocusedT
          (tailHelper $ moveHelper moveFocusedBack p)
          (\h -> focus <$ c h)
          (tailHelper $ moveHelper moveFocusedForewared f)

instance ComonadTrans FocusedT where
  lower (FocusedT _ c _) = c HasFocus

advanceFocus :: (Comonad w, Applicative m) => CovT (FocusedT w) m ()
advanceFocus = CovT $ \w -> pure (extract $ fromMaybe w (moveFocusedForewared w), ())

reverseFocus :: (Comonad w, Applicative m) => CovT (FocusedT w) m ()
reverseFocus = CovT $ \w -> pure (extract $ fromMaybe w (moveFocusedBack w), ())

foo :: FocusedT Identity Int
foo = FocusedT [const $ pure 1] (const $ pure 2) [const $ pure 5, const $ pure 6]

