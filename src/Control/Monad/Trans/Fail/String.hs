{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Control.Monad.Trans.Fail.String
-- Copyright   : (c) Alexey Kuleshevich 2022-2023
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Control.Monad.Trans.Fail.String (
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
  mapErrorsFailT,
  exceptFailT,
  throwFailT,
) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except
import qualified Control.Monad.Trans.Fail as F
import GHC.Stack

-- | Version of `F.Fail` restricted to `String`
type Fail = F.Fail String

-- | Version of `F.runFail` restricted to `String`
runFail :: Fail a -> Either String a
runFail = F.runFail
{-# INLINE runFail #-}

-- | Version of `F.runFailLast` restricted to `String`
runFailLast :: Fail a -> Either String a
runFailLast = F.runFailLast
{-# INLINE runFailLast #-}

-- | Version of `F.runFailAgg` restricted to `String`
runFailAgg :: Fail a -> Either [String] a
runFailAgg = F.runFailAgg
{-# INLINE runFailAgg #-}

-- | Version of `F.errorFail` restricted to `String`
--
-- Throw an error if there was a failure, otherwise return the result of monadic
-- computation. Use `throwFailT` in case you'd like to handle an actual exception.
errorFail :: HasCallStack => Fail a -> a
errorFail = F.errorFail

-- | Version of `F.runFail` restricted to `String`
--
-- Same as `errorFail`, but without the stack trace:
--
-- >>> errorFailWithoutStackTrace (fail "This didn't work" :: Fail ())
-- *** Exception: "This didn't work"
-- >>> import Control.Applicative
-- >>> errorFailWithoutStackTrace (fail "This didn't work" <|> pure "That Worked" :: Fail String)
-- "That Worked"
errorFailWithoutStackTrace :: Fail a -> a
errorFailWithoutStackTrace = F.errorFailWithoutStackTrace

-- | Version of `F.FailT` restricted to `String`
--
-- Fail monad transformer that plays well with `MonadFail`
type FailT = F.FailT String

-- | Version of `F.FailException` restricted to `String`
type FailException = F.FailException String

-- | Version of `F.failT` restricted to `String`
--
-- Monomorphic synonym for `fail`
failT :: Applicative m => String -> FailT m a
failT = F.failT
{-# INLINE failT #-}

-- | Version of `F.runFailT` restricted to `String`
runFailT :: Functor m => FailT m a -> m (Either String a)
runFailT = F.runFailT
{-# INLINE runFailT #-}

-- | Version of `F.runFailLastT` restricted to `String`
runFailLastT :: Functor m => FailT m a -> m (Either String a)
runFailLastT = F.runFailLastT
{-# INLINE runFailLastT #-}

-- | Version of `F.runFailAggT` restricted to `String`
runFailAggT :: FailT m a -> m (Either [String] a)
runFailAggT = F.runFailAggT
{-# INLINE runFailAggT #-}

-- | Version of `F.hoistFailT` restricted to `String`
--
-- Change the underlying monad with the hoisting function
hoistFailT :: (forall a. m a -> n a) -> FailT m b -> FailT n b
hoistFailT = F.hoistFailT
{-# INLINE hoistFailT #-}

-- | Version of `F.mapFailT` restricted to `String`
--
-- Map a function over the underlying representation of the `FailT` monad.
mapFailT :: (m (Either [String] a) -> n (Either [String] b)) -> FailT m a -> FailT n b
mapFailT = F.mapFailT
{-# INLINE mapFailT #-}

-- | Version of `F.mapErrorFail`, where resulting type is restricted to `String`
--
-- Map a function over the error type in the `FailT` monad.
mapErrorFailT :: Functor m => (e -> String) -> F.FailT e m a -> FailT m a
mapErrorFailT = F.mapErrorFailT
{-# INLINE mapErrorFailT #-}

-- | Version of `F.mapErrorsFail`, where resulting type is restricted to `String`
--
-- Map a function over the aggregation of errors in the `FailT` monad. Could be used for
-- example for clearing our all of the aggregated error messages:
--
-- >>> runFail (mapErrorsFailT (const []) $ failT "Something went wrong") :: Either String ()
-- Left "No failure reason given"
mapErrorsFailT :: Functor m => ([e] -> [String]) -> F.FailT e m a -> FailT m a
mapErrorsFailT = F.mapErrorsFailT
{-# INLINE mapErrorsFailT #-}

-- | Version of `F.exceptFailT` restricted to `String`
exceptFailT :: (HasCallStack, Monad m) => FailT m a -> ExceptT FailException m a
exceptFailT = F.exceptFailT
{-# INLINE exceptFailT #-}

-- | Version of `F.throwFailT` restricted to `String`
throwFailT :: (HasCallStack, MonadThrow m) => FailT m a -> m a
throwFailT = F.throwFailT
{-# INLINE throwFailT #-}
