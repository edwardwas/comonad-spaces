module Spaces.Tagged where

import           Spaces
import           Spaces.CovT

import           Control.Comonad
import           Control.Lens
import           Data.Functor.Day
import           Data.Proxy
import           GHC.OverloadedLabels
import           GHC.TypeLits

data Tag (n :: Symbol) where
   Tag :: KnownSymbol n => Tag n

instance (KnownSymbol n, k ~ n) => IsLabel n (Tag k) where
  fromLabel = Tag

data TaggedProduct (ks :: [Symbol]) fs a where
  EmptyTaggedProduct :: a -> TaggedProduct '[] '[] a
  TaggedProduct
    :: Tag k
    -> f a
    -> TaggedProduct ks fs b
    -> (a -> b -> c)
    -> TaggedProduct (k ': ks) (f ': fs) c

instance Functor (TaggedProduct ks fs) where
  fmap f (EmptyTaggedProduct a) = EmptyTaggedProduct $ f a
  fmap f (TaggedProduct t here there combFunc) =
    TaggedProduct t here there (\a b -> f $ combFunc a b)

instance Applicative (TaggedProduct '[] '[]) where
  pure = EmptyTaggedProduct
  EmptyTaggedProduct f <*> EmptyTaggedProduct a = EmptyTaggedProduct (f a)

instance (Applicative f, KnownSymbol k, Applicative (TaggedProduct ks fs)) =>
         Applicative (TaggedProduct (k ': ks) (f ': fs)) where
  pure a = TaggedProduct Tag (pure ()) (pure ()) (\_ _ -> a)
  TaggedProduct _ hereA thereA combFuncA <*> TaggedProduct _ hereB thereB combFuncB =
    TaggedProduct
      Tag
      ((,) <$> hereA <*> hereB)
      ((,) <$> thereA <*> thereB)
      (\(aa, ab) (ba, bb) -> (combFuncA aa ba) (combFuncB ab bb))

instance Comonad (TaggedProduct '[] '[]) where
  extract (EmptyTaggedProduct a) = a
  duplicate = EmptyTaggedProduct

instance (Comonad (TaggedProduct ks fs), Comonad f, KnownSymbol k) =>
         Comonad (TaggedProduct (k ': ks) (f ': fs)) where
  extract (TaggedProduct _ here there combFunc) =
    combFunc (extract here) (extract there)
  duplicate (TaggedProduct _ here there combFunc) =
    TaggedProduct
      Tag
      (duplicate here)
      (duplicate there)
      (\fHere fThere -> TaggedProduct Tag fHere fThere combFunc)

class InProduct k f ks fs | fs -> f , ks -> k where
  extractViaTag :: Tag k -> TaggedProduct ks fs a -> f a

instance (Functor f, Comonad (TaggedProduct ks fs)) =>
         InProduct k f (k ': ks) (f ': fs) where
  extractViaTag _ (TaggedProduct _ here there combFunc) =
    (\a -> combFunc a $ extract there) <$> here

instance {-# OVERLAPS #-} (InProduct k f ks fs, Comonad f1) =>
                          InProduct k f (k1 ': ks) (f1 ': fs) where
  extractViaTag tag (TaggedProduct _ here there combFunc) =
    extractViaTag tag (combFunc (extract here) <$> there)

popTaggedProduct ::
     Comonad f => TaggedProduct (k ': ks) (f ': fs) a -> TaggedProduct ks fs a
popTaggedProduct (TaggedProduct _ here there combFunc) =
  combFunc (extract here) <$> there

taggedCovT ::
     InProduct k f ks fs
  => Tag k
  -> CovT f m a
  -> CovT (TaggedProduct ks fs) m a
taggedCovT tag co = CovT (runCovT co . extractViaTag tag)

withTaggedSpace ::
     forall k f ks fs m ui.
     ( Comonad f
     , Applicative ui
     , Comonad (TaggedProduct ks fs)
     , Monad m
     , KnownSymbol k
     )
  => Tag k
  -> Space m f ui
  -> Space m (TaggedProduct ks fs) ui
  -> TaggedProduct (k ': ks) (f ': fs) (ui (CovT (TaggedProduct (k ': ks) (f ': fs)) m ()))
withTaggedSpace tag newSpace oldSpace =
  TaggedProduct
    tag
    newSpace
    oldSpace
    (\new old ->
       (*>) <$> (taggedCovT tag <$> new) <*>
       (covHoistW popTaggedProduct <$> old))

emptyTaggedSpace :: (Monad m, Applicative ui) => Space m (TaggedProduct '[] '[]) ui
emptyTaggedSpace = EmptyTaggedProduct $ pure $ pure ()
