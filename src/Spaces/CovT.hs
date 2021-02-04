module Spaces.CovT where

import           Control.Applicative
import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Comonad.Env    (Env, EnvT (..))
import qualified Control.Comonad.Env    as Env
import           Control.Comonad.Store
import           Control.Comonad.Traced
import           Control.Lens           hiding ((:<))
import           Control.Monad.Free
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

newtype CovT w m a =
  CovT
    { runCovT :: forall x. w x -> m (x, a)
    }

type Cov w = CovT w Identity

collapseCovT :: Functor m => CovT Identity m a -> m a
collapseCovT co = snd <$> runCovT co (Identity ())

runCov :: Cov w a -> w x -> (x, a)
runCov co = runIdentity . runCovT co

covHoistW :: (forall x . w x -> v x) -> CovT v m a -> CovT w m a
covHoistW f co = CovT (runCovT co . f)

covHoistM :: (forall x . m x -> n x) -> CovT w m a -> CovT w n a
covHoistM f co = CovT $ \w -> f $ runCovT co w

raiseCovT :: (Comonad w, Functor m) => m a -> CovT w m a
raiseCovT action = CovT $ \w -> (extract w,) <$> action

instance Functor m => Functor (CovT w m) where
  fmap f (CovT cov) = CovT $ \w -> (\(x, a) -> (x, f a)) <$> cov w

instance (Monad m, Comonad w) => Applicative (CovT w m) where
  pure a = CovT $ \w -> pure (extract w, a)
  (<*>) = ap

instance (Comonad w, Monad m) => Monad (CovT w m) where
  m >>= f = CovT $ \w -> do
    (w', a) <- runCovT m (duplicate w)
    runCovT (f a) w'

movingCovT :: (Functor m, Comonad w) => CovT w m (a -> b) -> w a -> m (w b)
movingCovT covt w = (\(w', f) -> f <$> w') <$> runCovT covt (duplicate w)

moveCovT :: (Functor m, Comonad w) => CovT w m () -> w a -> m (w a)
moveCovT co = movingCovT (id <$ co)

-- = MTL instances
-- == Reader

covToReader :: CovT (EnvT r w) m a -> ReaderT r (CovT w m) a
covToReader cov = ReaderT $ \r -> CovT (runCovT cov . EnvT r)

readerToCov :: ReaderT r (CovT w m) a -> CovT (EnvT r w) m a
readerToCov (ReaderT act) = CovT $ \(EnvT e w) -> runCovT (act e) w

instance (Comonad w, Monad m) => MonadReader r (CovT (EnvT r w) m) where
  ask = readerToCov ask
  local f = readerToCov . local f . covToReader

instance (Monad m, Comonad w, MonadState s (CovT w m)) =>
         MonadState s (CovT (EnvT r w) m) where
  state = readerToCov . state

instance (Monad m, Comonad w, MonadWriter x (CovT w m)) =>
         MonadWriter x (CovT (EnvT r w) m) where
  tell = readerToCov . tell
  listen = readerToCov . listen . covToReader
  pass = readerToCov . pass . covToReader

type instance Zoomed (CovT (EnvT r w) m) = Zoomed (CovT w m)

instance (Comonad w, Comonad v, Monad m, Zoom (CovT w m) (CovT v m) s t) =>
         Zoom (CovT (EnvT r w) m) (CovT (EnvT r v) m) s t where
  zoom l = readerToCov . zoom l . covToReader

-- == Writer

writerToCov :: Functor m => WriterT r (CovT w m) a -> CovT (TracedT r w) m a
writerToCov (WriterT coMW) =
  CovT $ \(TracedT w) -> (\(func, (a, r)) -> (func r, a)) <$> runCovT coMW w

covToWriter ::
     (Monad m, Comonad w, Monoid x)
  => CovT (TracedT x w) m a
  -> WriterT x (CovT w m) a
covToWriter cov = do
  (_, current) <- listen $ return ()
  WriterT $
    CovT $ \w -> fmap (, current) <$> runCovT cov (TracedT (const <$> w))

instance (Monoid x, Comonad w, Monad m) =>
         MonadWriter x (CovT (TracedT x w) m) where
  tell = writerToCov . tell
  listen = writerToCov . listen . covToWriter
  pass = writerToCov . pass . covToWriter

instance (Monoid x, Monad m, Comonad w, MonadReader r (CovT w m)) =>
         MonadReader r (CovT (TracedT x w) m) where
  ask = writerToCov ask
  local f = writerToCov . local f . covToWriter

instance (Monoid x, Monad m, Comonad w, MonadState s (CovT w m)) =>
         MonadState s (CovT (TracedT x w) m) where
  state = writerToCov . state

-- == State

stateToCov :: Functor m => StateT s (CovT w m) a -> CovT (StoreT s w) m a
stateToCov (StateT act) =
  CovT $ \(StoreT w s) -> (\(f, (a, s)) -> (f s, a)) <$> runCovT (act s) w

covToState ::
     (Comonad w, Monad m) => CovT (StoreT s w) m a -> StateT s (CovT w m) a
covToState cov =
  StateT $ \current ->
    CovT $ \w -> fmap (, current) <$> runCovT cov (StoreT (const <$> w) current)

instance (Monad m, Comonad w) => MonadState s (CovT (StoreT s w) m) where
  state = stateToCov . state

instance (Monad m, Comonad w, MonadReader r (CovT w m)) =>
         MonadReader r (CovT (StoreT s w) m) where
  ask = stateToCov ask
  local f = stateToCov . local f . covToState

instance (Monad m, Comonad w, MonadWriter x (CovT w m)) =>
         MonadWriter x (CovT (StoreT s w) m) where
  tell = stateToCov . tell
  listen = stateToCov . listen . covToState
  pass = stateToCov . pass . covToState

type instance Zoomed (CovT (StoreT s w) m) = Zoomed (StateT s (CovT w m))

instance (Comonad w, Monad m) =>
         Zoom (CovT (StoreT s w) m) (CovT (StoreT s' w) m) s s' where
  zoom l = stateToCov . zoom l . covToState

-- == Free
