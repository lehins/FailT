{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Control.Monad.Trans.Fail
-- Copyright   : (c) Alexey Kuleshevich 2022-2023
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Control.Monad.Trans.Fail (
  Fail (..),
  runFail,
  errorFail,
  FailT (..),
  runFailT,
) where

import Control.Applicative
import Control.Exception
import qualified Control.Monad.Fail as Fail
import Control.Monad.Trans
import Data.Functor.Identity
#if !(MIN_VERSION_base(4,9,0))
import Data.Stack
#else
import GHC.Stack
#endif

type Fail = FailT Identity

runFail :: Fail a -> Either String a
runFail = runIdentity . runFailT
{-# INLINE runFail #-}

errorFail :: HasCallStack => Fail a -> a
errorFail = either error id . runFail

newtype FailT m a = FailT (m (Either String a))

runFailT :: FailT m a -> m (Either String a)
runFailT (FailT f) = f
{-# INLINE runFailT #-}

data FailException = FailException
  { failCallStack :: CallStack
  , failMessage :: String
  }
  deriving (Show)

instance Exception FailException

-- instance MonadTrans m => MonadTrans (FailT m) where
--   lift f

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
        Right a -> runFailT $ k a
  {-# INLINE (>>=) #-}

#if !(MIN_VERSION_base(4,8,0))
  return a = FailT $ return $ Right a
  {-# INLINE return #-}
#endif

#if !(MIN_VERSION_base(4,13,0))
  fail = FailT . return . Left
  {-# INLINE fail #-}
#endif

instance Monad m => Fail.MonadFail (FailT m) where
  fail = FailT . return . Left
  {-# INLINE fail #-}

instance Foldable f => Foldable (FailT f) where
  foldMap f (FailT m) = foldMap (either (const mempty) f) m
  {-# INLINE foldMap #-}

instance Traversable f => Traversable (FailT f) where
  traverse f (FailT m) = FailT <$> traverse (either (pure . Left) (fmap Right . f)) m
  {-# INLINE traverse #-}

instance Monad m => Alternative (FailT m) where
  empty = FailT $ pure (Left "")
  {-# INLINE empty #-}
  FailT m <|> FailT k = FailT $ do
    m >>= \case
      Left merr ->
        k >>= \case
          Left kerr -> pure $ Left $ merr ++ kerr
          Right result -> pure $ Right $ result
      Right x -> pure (Right x)
  {-# INLINEABLE (<|>) #-}
