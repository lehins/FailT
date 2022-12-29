{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Control.Monad.Trans.Fail
-- Copyright   : (c) Alexey Kuleshevich 2022-2023
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Control.Monad.Trans.Fail (
  Fail,
  runFail,
  runFailLast,
  runFailAgg,
  errorFail,
  FailT (..),
  runFailT,
  runFailLastT,
  runFailAggT,
  hoistFailT,
  mapFailT,
  exceptFailT,
  throwFailT,

  -- * Helpers
  liftCatch,
  liftListen,
  liftPass,
) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Cont
import Control.Monad.Except
import qualified Control.Monad.Fail as Fail
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Zip
import Data.Functor.Classes
import Data.Functor.Identity
import Data.List (intersperse)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup
import GHC.Stack
#if MIN_VERSION_base(4,12,0)
import Data.Functor.Contravariant
#endif

type Fail = FailT Identity

runFail :: Fail a -> Either String a
runFail = runIdentity . runFailT
{-# INLINE runFail #-}

runFailLast :: Fail a -> Either String a
runFailLast = runIdentity . runFailLastT
{-# INLINE runFailLast #-}

runFailAgg :: Fail a -> Either [String] a
runFailAgg = runIdentity . runFailAggT
{-# INLINE runFailAgg #-}

errorFail :: HasCallStack => Fail a -> a
errorFail = either error id . runFail

newtype FailT m a = FailT (m (Either [String] a))

runFailT :: Functor m => FailT m a -> m (Either String a)
runFailT (FailT f) = either (Left . sconcat . NE.intersperse "\n" . toFailureNonEmpty) Right <$> f
{-# INLINE runFailT #-}

runFailLastT :: Functor m => FailT m a -> m (Either String a)
runFailLastT (FailT f) = either (Left . NE.head . toFailureNonEmpty) Right <$> f
{-# INLINE runFailLastT #-}

runFailAggT :: FailT m a -> m (Either [String] a)
runFailAggT (FailT f) = f
{-# INLINE runFailAggT #-}

-- | Change the underlying monad with the hoisting function
hoistFailT :: (forall a. m a -> n a) -> FailT m b -> FailT n b
hoistFailT f = FailT . f . runFailAggT
{-# INLINE hoistFailT #-}

-- | Map a function over the underlying representation of the FailT monad.
mapFailT :: (m (Either [String] a) -> n (Either [String] b)) -> FailT m a -> FailT n b
mapFailT f = FailT . f . runFailAggT
{-# INLINE mapFailT #-}

exceptFailT :: (HasCallStack, Monad m) => FailT m a -> ExceptT FailException m a
exceptFailT m =
  ExceptT $
    runFailAggT m >>= \case
      Right x -> pure $ Right x
      Left errMsgs ->
        pure $
          Left $
            FailException
              { failCallStack = ?callStack
              , failMessages = errMsgs
              }
{-# INLINE exceptFailT #-}

data FailException = FailException
  { failCallStack :: CallStack
  , failMessages :: [String]
  }

instance Show FailException where
  show FailException{failCallStack, failMessages} =
    mconcat $
      intersperse "\n" $
        "FailException"
          : NE.toList (toFailureNonEmpty failMessages)
          ++ [prettyCallStack failCallStack]

instance Exception FailException

toFailureNonEmpty :: [String] -> NE.NonEmpty String
toFailureNonEmpty xs =
  case NE.nonEmpty xs of
    Nothing -> "No failure reason given" NE.:| []
    Just ne -> ne

throwFailT :: (HasCallStack, MonadThrow m) => FailT m a -> m a
throwFailT f = do
  runFailAggT f >>= \case
    Right x -> pure x
    Left errMsgs ->
      throwM $
        FailException
          { failCallStack = ?callStack
          , failMessages = errMsgs
          }
{-# INLINEABLE throwFailT #-}

instance Functor m => Functor (FailT m) where
  fmap f (FailT m) = FailT (fmap (fmap f) m)
  {-# INLINE fmap #-}

instance Monad m => Applicative (FailT m) where
  pure = FailT . pure . Right
  {-# INLINE pure #-}

  FailT m <*> FailT k =
    FailT $
      m >>= \case
        Left merr -> pure $ Left merr
        Right f ->
          k >>= \case
            Left kerr -> pure $ Left kerr
            Right a -> pure $ Right (f a)
  {-# INLINE (<*>) #-}

instance Monad m => Monad (FailT m) where
  FailT m >>= k =
    FailT $
      m >>= \case
        Left merr -> return $ Left merr
        Right a -> runFailAggT $ k a
  {-# INLINE (>>=) #-}

#if !(MIN_VERSION_base(4,13,0))
  fail = FailT . return . Left
  {-# INLINE fail #-}
#endif

instance Monad m => Fail.MonadFail (FailT m) where
  fail = FailT . return . Left . pure
  {-# INLINE fail #-}

instance Foldable f => Foldable (FailT f) where
  foldMap f (FailT m) = foldMap (either (const mempty) f) m
  {-# INLINE foldMap #-}

instance Traversable f => Traversable (FailT f) where
  traverse f (FailT m) = FailT <$> traverse (either (pure . Left) (fmap Right . f)) m
  {-# INLINE traverse #-}

plus :: Monad m => FailT m a -> FailT m a -> FailT m a
plus (FailT m) (FailT k) = FailT $ do
  m >>= \case
    Left merr ->
      k >>= \case
        Left kerr -> pure $ Left $ merr ++ kerr
        Right result -> pure $ Right $ result
    Right x -> pure (Right x)
{-# INLINEABLE plus #-}

instance Monad m => Alternative (FailT m) where
  empty = FailT $ pure (Left [])
  {-# INLINE empty #-}
  (<|>) = plus
  {-# INLINE (<|>) #-}

instance Monad m => MonadPlus (FailT m) where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = plus
  {-# INLINE mplus #-}

instance (Monad m, Semigroup a) => Semigroup (FailT m a) where
  (<>) (FailT m) (FailT k) = FailT $ do
    mres <- m
    kres <- k
    case mres of
      Left merr ->
        case kres of
          Left kerr -> pure $ Left $ merr ++ kerr
          Right y -> pure $ Right y
      Right x ->
        case kres of
          Left _kerr -> pure $ Right x
          Right y -> pure $ Right (x <> y)
  {-# INLINEABLE (<>) #-}

instance (Monad m, Semigroup a) => Monoid (FailT m a) where
  mempty = empty
  {-# INLINE mempty #-}

instance MonadIO m => MonadIO (FailT m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadTrans FailT where
  lift = FailT . fmap Right
  {-# INLINE lift #-}

instance MonadZip m => MonadZip (FailT m) where
  mzipWith f (FailT a) (FailT b) = FailT $ mzipWith (liftA2 f) a b
  {-# INLINE mzipWith #-}

#if MIN_VERSION_base(4,12,0)
instance Contravariant f => Contravariant (FailT f) where
  contramap f = FailT . contramap (fmap f) . runFailAggT
  {-# INLINE contramap #-}
#endif

instance Eq1 m => Eq1 (FailT m) where
  liftEq eq (FailT x) (FailT y) = liftEq (liftEq eq) x y
  {-# INLINE liftEq #-}

instance Ord1 m => Ord1 (FailT m) where
  liftCompare comp (FailT x) (FailT y) =
    liftCompare (liftCompare comp) x y
  {-# INLINE liftCompare #-}

instance Read1 m => Read1 (FailT m) where
  liftReadsPrec rp rl =
    readsData $
      readsUnaryWith (liftReadsPrec rp' rl') "FailT" FailT
    where
      rp' = liftReadsPrec rp rl
      rl' = liftReadList rp rl

instance Show1 m => Show1 (FailT m) where
  liftShowsPrec sp sl d (FailT m) =
    showsUnaryWith (liftShowsPrec sp' sl') "FailT" d m
    where
      sp' = liftShowsPrec sp sl
      sl' = liftShowList sp sl

instance (Eq1 m, Eq a) => Eq (FailT m a) where
  (==) = eq1
  {-# INLINE (==) #-}

instance (Ord1 m, Ord a) => Ord (FailT m a) where
  compare = compare1
  {-# INLINE compare #-}

instance (Read1 m, Read a) => Read (FailT m a) where
  readsPrec = readsPrec1

instance (Show1 m, Show a) => Show (FailT m a) where
  showsPrec = showsPrec1

instance MonadReader r m => MonadReader r (FailT m) where
  ask = lift ask
  {-# INLINE ask #-}
  local = mapFailT . local
  {-# INLINE local #-}
  reader = lift . reader
  {-# INLINE reader #-}

instance MonadState s m => MonadState s (FailT m) where
  get = lift get
  {-# INLINE get #-}
  put = lift . put
  {-# INLINE put #-}
  state = lift . state
  {-# INLINE state #-}

instance MonadError e m => MonadError e (FailT m) where
  throwError = lift . throwError
  {-# INLINE throwError #-}
  catchError = liftCatch catchError
  {-# INLINE catchError #-}

instance MonadWriter w m => MonadWriter w (FailT m) where
  writer = lift . writer
  {-# INLINE writer #-}
  tell = lift . tell
  {-# INLINE tell #-}
  listen = liftListen listen
  {-# INLINE listen #-}
  pass = liftPass pass
  {-# INLINE pass #-}

instance MonadCont m => MonadCont (FailT m) where
  callCC = liftCallCC callCC
  {-# INLINE callCC #-}

-- | Lift a @callCC@ operation to the new monad.
liftCallCC
  :: (((Either [String] a -> m (Either [String] b)) -> m (Either [String] a)) -> m (Either [String] a))
  -> ((a -> FailT m b) -> FailT m a)
  -> FailT m a
liftCallCC ccc f = FailT $ ccc $ \c ->
  runFailAggT (f (\a -> FailT $ c (Right a)))
{-# INLINE liftCallCC #-}

-- | Lift a @`catchE`@ operation to the new monad.
liftCatch
  :: (m (Either [String] a) -> (e -> m (Either [String] a)) -> m (Either [String] a))
  -> FailT m a
  -> (e -> FailT m a)
  -> FailT m a
liftCatch f m h = FailT $ f (runFailAggT m) (runFailAggT . h)
{-# INLINE liftCatch #-}

-- | Lift a @`listen`@ operation to the new monad.
liftListen
  :: Monad m
  => (m (Either [String] a) -> m (Either [String] a, w))
  -> (FailT m) a
  -> (FailT m) (a, w)
liftListen l = mapFailT $ \m -> do
  (a, w) <- l m
  return $! fmap (\r -> (r, w)) a
{-# INLINE liftListen #-}

-- | Lift a @`pass`@ operation to the new monad.
liftPass
  :: Monad m
  => (m (Either [String] a, w -> w) -> m (Either [String] a))
  -> (FailT m) (a, w -> w)
  -> (FailT m) a
liftPass p = mapFailT $ \m -> p $ do
  a <- m
  return $! case a of
    Left errs -> (Left errs, id)
    Right (v, f) -> (Right v, f)
{-# INLINE liftPass #-}
