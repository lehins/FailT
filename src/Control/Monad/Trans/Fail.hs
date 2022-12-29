{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

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
  throwFailT,
) where

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Monad.Trans
import Control.Monad.Zip
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

throwFailT :: (HasCallStack, MonadIO m) => FailT m a -> m a
throwFailT f = do
  runFailAggT f >>= \case
    Right x -> pure x
    Left errMsgs ->
      liftIO $
        throwIO $
          FailException
            { failCallStack = ?callStack
            , failMessages = errMsgs
            }

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
