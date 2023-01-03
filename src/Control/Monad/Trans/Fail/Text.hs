{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module      : Control.Monad.Trans.Fail.Text
-- Copyright   : (c) Alexey Kuleshevich 2022-2023
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable

module Control.Monad.Trans.Fail.Text (
  Fail,
  runFail,
  runFailLast,
  runFailAgg,
  errorFail,
  errorFailWithoutStackTrace,
  FailT,
  FailException,
  failT,
  runFailT,
  runFailLastT,
  runFailAggT,
  hoistFailT,
  mapFailT,
  mapErrorFailT,
  exceptFailT,
  throwFailT,
) where

import qualified Control.Monad.Trans.Fail as F
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except
import GHC.Stack
import Data.Text (Text)

type FailException = F.FailException Text

type Fail = F.Fail Text

runFail :: Fail a -> Either Text a
runFail = F.runFail
{-# INLINE runFail #-}

runFailLast :: Fail a -> Either Text a
runFailLast = F.runFailLast
{-# INLINE runFailLast #-}

runFailAgg :: Fail a -> Either [Text] a
runFailAgg = F.runFailAgg
{-# INLINE runFailAgg #-}

-- | Throw an error if there was a failure, otherwise return the result of monadic
-- computation. Use `throwFailT` in case you'd like to handle an actual exception.
errorFail :: HasCallStack => Fail a -> a
errorFail = F.errorFail

-- | Same as `errorFail`, but without the stack trace:
--
-- >>> errorFailWithoutStackTrace (fail "This didn't work" :: Fail ())
-- *** Exception: "This didn't work"
-- >>> import Control.Applicative
-- >>> errorFailWithoutStackTrace (fail "This didn't work" <|> pure "That Worked" :: Fail String)
-- "That Worked"
errorFailWithoutStackTrace :: Fail a -> a
errorFailWithoutStackTrace = F.errorFailWithoutStackTrace

-- | Fail monad transformer that plays well with `MonadFail`
type FailT = F.FailT Text

-- | Monomorphic synonym for `fail`
failT :: Applicative m => Text -> FailT m a
failT = F.failT
{-# INLINE failT #-}

runFailT :: Functor m => FailT m a -> m (Either Text a)
runFailT = F.runFailT
{-# INLINE runFailT #-}

runFailLastT :: Functor m => FailT m a -> m (Either Text a)
runFailLastT = F.runFailLastT
{-# INLINE runFailLastT #-}

runFailAggT :: FailT m a -> m (Either [Text] a)
runFailAggT = F.runFailAggT
{-# INLINE runFailAggT #-}

-- | Change the underlying monad with the hoisting function
hoistFailT :: (forall a. m a -> n a) -> FailT m b -> FailT n b
hoistFailT = F.hoistFailT
{-# INLINE hoistFailT #-}

-- | Map a function over the underlying representation of the `FailT` monad.
mapFailT :: (m (Either [Text] a) -> n (Either [Text] b)) -> FailT m a -> FailT n b
mapFailT = F.mapFailT
{-# INLINE mapFailT #-}

-- | Map a function over the error type in the `FailT` monad.
mapErrorFailT :: Functor m => (e -> Text) -> F.FailT e m a -> FailT m a
mapErrorFailT = F.mapErrorFailT
{-# INLINE mapErrorFailT #-}

exceptFailT :: (HasCallStack, Monad m) => FailT m a -> ExceptT FailException m a
exceptFailT = F.exceptFailT
{-# INLINE exceptFailT #-}

throwFailT :: (HasCallStack, MonadThrow m) => FailT m a -> m a
throwFailT = F.throwFailT
{-# INLINE throwFailT #-}
