module Spaces.Day where

import           Spaces
import           Spaces.CovT

import           Control.Applicative
import           Control.Comonad
import           Data.Functor.Apply
import           Data.Functor.Day
import           GHC.Exts

dayCovT :: Monad m => CovT w m (a -> b) -> CovT v m a -> CovT (Day w v) m b
dayCovT coW coV =
  CovT $ \(Day f g func) -> do
    (z, a2b) <- runCovT coW f
    (y, a) <- runCovT coV g
    return (func z y, a2b a)

dayCovT_ :: Monad m => CovT w m () -> CovT v m () -> CovT (Day w v) m ()
dayCovT_ coW coV  = dayCovT (const <$> coW) coV

composeSpace ::
     (Monad m, Functor w, Applicative ui)
  => Space m w ui
  -> Space m v ui
  -> Space m (Day w v) ui
composeSpace space1 space2 = day (liftA2 dayCovT_ <$> space1) space2

dayLeft :: (Applicative g, Functor f) => f a -> Day f g a
dayLeft f = day (const <$> f) (pure ())

dayRight :: (Applicative f) => g a -> Day f g a
dayRight g = day (pure id) g

extractDayLeft :: (Functor f, Comonad g) => Day f g a -> f a
extractDayLeft (Day f g func) = (\a -> func a (extract g)) <$> f

extractDayRight :: (Comonad f, Functor g) => Day f g a -> g a
extractDayRight (Day f g func) = (func (extract f)) <$> g

type family AllC (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  AllC _ '[] = ()
  AllC c (x ': xs) = (c x, AllC c xs)

data ManyDay (fs :: [* -> *]) a where
  BaseManyDay :: a -> ManyDay '[] a
  ManyDay :: f a -> ManyDay fs b -> (a -> b -> c) -> ManyDay (f ': fs) c

instance Functor (ManyDay fs) where
  fmap func (BaseManyDay a)  = BaseManyDay $ func a
  fmap func (ManyDay d ds f) = ManyDay d ds (\a b -> func $ f a b)

instance (AllC Comonad fs) => Comonad (ManyDay fs) where
  extract (BaseManyDay a)     = a
  extract (ManyDay d ds func) = func (extract d) (extract ds)
  duplicate (BaseManyDay a) = BaseManyDay $ BaseManyDay a
  duplicate (ManyDay d ds func) =
    ManyDay (duplicate d) (duplicate ds) $ \d' ds' -> ManyDay d' ds' func

instance Applicative (ManyDay '[]) where
  pure = BaseManyDay
  BaseManyDay f <*> BaseManyDay a = BaseManyDay (f a)

instance (Applicative f, Applicative (ManyDay fs)) =>
         Applicative (ManyDay (f ': fs)) where
  pure a = ManyDay (pure ()) (pure ()) $ \_ _ -> a
  ManyDay df dfs funcf <*> ManyDay da das funca =
    ManyDay ((,) <$> df <*> da) ((,) <$> dfs <*> das) $ \(fa, aa) (fb, ab) ->
      (funcf fa fb) (funca aa ab)

class ExtractManyDay f fs where
  extractManyDay :: ManyDay fs a -> f a

instance (AllC Comonad fs, Functor f) => ExtractManyDay f (f ': fs) where
  extractManyDay (ManyDay d ds func) = (\a -> func a (extract ds)) <$> d

instance {-# OVERLAPS #-} (Functor f, ExtractManyDay f fs, Comonad g) =>
         ExtractManyDay f (g ': fs) where
  extractManyDay (ManyDay d ds func) = func (extract d) <$> extractManyDay ds

manyDayCovT :: Monad m => CovT w m (a -> b) -> CovT (ManyDay ws) m a -> CovT (ManyDay (w ': ws)) m b
manyDayCovT coA coAs = CovT $ \(ManyDay d ds func) -> do
  (x1, f) <- runCovT coA d
  (x2, a) <- runCovT coAs ds
  return (func x1 x2, f a)

consManySpace ::
     (Monad m, Applicative ui)
  => Space m w ui
  -> Space m (ManyDay ws) ui
  -> Space m (ManyDay (w ': ws)) ui
consManySpace space1 space2 =
  ManyDay space1 space2 $ liftA2 $ \w v -> manyDayCovT (const <$> w) v

(<:>) ::
     (Monad m, Applicative ui)
  => Space m w ui
  -> Space m (ManyDay ws) ui
  -> Space m (ManyDay (w ': ws)) ui
(<:>) = consManySpace

infixr 5 <:>
