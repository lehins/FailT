{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Control.Monad.Trans.Fail
-- Copyright   : (c) Alexey Kuleshevich 2022-2023
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
module Control.Monad.Trans.Fail (
  -- * Fail
  Fail,
  runFail,
  runFailLast,
  runFailAgg,
  errorFail,
  errorFailWithoutStackTrace,

  -- * FailT
  FailT (..),
  FailException (..),
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

  -- * Helpers
  liftCatch,
  liftListen,
  liftPass,
) where

import Control.Applicative
import Control.Exception
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.Cont
import Control.Monad.Except
import qualified Control.Monad.Fail as F
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Zip
import Data.Bifunctor (first)
import Data.Functor.Classes
import Data.Functor.Identity
import Data.List (intersperse)
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup as Semi
import Data.Typeable
import GHC.Exts
import GHC.Stack
#if MIN_VERSION_base(4,12,0)
import Data.Functor.Contravariant
#endif

-- | `FailT` transformer with `Identity` as the base monad.
type Fail e = FailT e Identity

-- | Unwrap the pure `Fail` monad and reveal the underlying result of monadic
-- computation.
--
-- >>> runFail (fail "Something went wrong") :: Either String ()
-- Left "Something went wrong"
-- >>> runFail (failT "Something went wrong" >> pure ())
-- Left "Something went wrong"
-- >>> import Control.Applicative
-- >>> runFail (failT "Something could have gone wrong" <|> pure ())
-- Right ()
--
-- All errors accrued during the monadic computation will be combined using the
-- `Semi.Semigroup` instance and delimited by a comma:
--
-- >>> runFail (fail "One thing went wrong" <|> fail "Another thing went wrong") :: Either String ()
-- Left "One thing went wrong, Another thing went wrong"
--
-- Failing with one of instances functions `mempty` or `empty` will yield a no-reason
-- error report:
--
-- >>> runFail mempty :: Either String ()
-- Left "No failure reason given"
runFail :: (IsString e, Semi.Semigroup e) => Fail e a -> Either e a
runFail = runIdentity . runFailT
{-# INLINE runFail #-}

-- | This is a variant of `runFailAgg` where only the error reported for the very last
-- failed computation will be produced and others discarded. This is useful when it is not
-- relevant to retain information about all the attempts and only the last one matters,
-- eg. parsing with backtracking.
runFailLast :: IsString e => Fail e a -> Either e a
runFailLast = runIdentity . runFailLastT
{-# INLINE runFailLast #-}

-- | Convert a `Fail` monad computation in an `Either`, where the `Left` will contain all
-- failures in the same order they where received, or `Right` upon a successful computation.
--
-- >>> runFailAgg (fail "One bad thing" <|> fail "Another bad thing") :: Either [String] ()
-- Left ["One bad thing","Another bad thing"]
-- >>> runFailAgg (fail "A bad thing" <|> pure "A good thing") :: Either [String] String
-- Right "A good thing"
runFailAgg :: Fail e a -> Either [e] a
runFailAgg = runIdentity . runFailAggT
{-# INLINE runFailAgg #-}

-- | Throw an error if there was a failure, otherwise return the result of
-- computation. Use `throwFailT` in case you'd like to handle an actual exception in some
-- other underlying monad.
--
-- >>> errorFail (fail "This didn't work" :: Fail String ())
-- *** Exception: "This didn't work"
-- CallStack (from HasCallStack):
-- ...
-- >>> errorFail (fail "This didn't work" <|> pure "That Worked" :: Fail String String)
-- "That Worked"
errorFail :: (Show e, HasCallStack) => Fail e a -> a
errorFail = either (error . toFailureDelimited . fmap show) id . runFailAgg

-- | Same as `errorFail`, but without the stack trace:
--
-- >>> errorFailWithoutStackTrace (fail "This didn't work" :: Fail String ())
-- *** Exception: "This didn't work"
-- >>> errorFailWithoutStackTrace (fail "This didn't work" <|> pure "That Worked" :: Fail String String)
-- "That Worked"
errorFailWithoutStackTrace :: Show e => Fail e a -> a
errorFailWithoutStackTrace =
  either (errorWithoutStackTrace . toFailureDelimited . fmap show) id . runFailAgg

-- | Fail monad transformer that plays well with `F.MonadFail` type class.
newtype FailT e m a = FailT (m (Either [e] a))

-- | Similar to `fail`, but it is not restricted to `String`.
failT :: Applicative m => e -> FailT e m a
failT = FailT . pure . Left . pure

-- | Similar to `runFail`, except underlying monad is not restricted to `Identity`.
--
-- Unwrap the `FailT` monad transformer and produce an action that can be executed in
-- the underlying monad and, which will produce either a comma delimited error message
-- upon a failure or the result otherwise.
--
-- >>> runFailT (failT "Could have failed" <|> liftIO (putStrLn "Nothing went wrong"))
-- Nothing went wrong
-- Right ()
runFailT :: (IsString e, Semi.Semigroup e, Functor m) => FailT e m a -> m (Either e a)
runFailT (FailT f) = either (Left . toFailureDelimited) Right <$> f
{-# INLINE runFailT #-}

-- | Similar to `runFailLast`, except underlying monad is not restricted to `Identity`.
runFailLastT :: (IsString e, Functor m) => FailT e m a -> m (Either e a)
runFailLastT (FailT f) = either (Left . NE.last . toFailureNonEmpty) Right <$> f
{-# INLINE runFailLastT #-}

-- | Similar to `runFailAgg`, except underlying monad is not restricted to `Identity`.
runFailAggT :: FailT e m a -> m (Either [e] a)
runFailAggT (FailT f) = f
{-# INLINE runFailAggT #-}

-- | Change the underlying monad with the hoisting function.
hoistFailT :: (forall a. m a -> n a) -> FailT e m b -> FailT e n b
hoistFailT f = FailT . f . runFailAggT
{-# INLINE hoistFailT #-}

-- | Map a function over the underlying representation of the `FailT` monad.
mapFailT :: (m (Either [e] a) -> n (Either [e] b)) -> FailT e m a -> FailT e n b
mapFailT f = FailT . f . runFailAggT
{-# INLINE mapFailT #-}

-- | Map a function over the error type in the `FailT` monad.
mapErrorFailT :: Functor m => (e -> e') -> FailT e m a -> FailT e' m a
mapErrorFailT f = mapErrorsFailT (map f)
{-# INLINE mapErrorFailT #-}

-- | Map a function over the aggregation of errors in the `FailT` monad. Could be used for
-- example for clearing our all of the aggregated error messages:
--
-- >>> runFail (mapErrorsFailT (const []) $ failT "Something went wrong") :: Either String ()
-- Left "No failure reason given"
mapErrorsFailT :: Functor m => ([e] -> [e']) -> FailT e m a -> FailT e' m a
mapErrorsFailT f (FailT m) = FailT (fmap (first f) m)
{-# INLINE mapErrorsFailT #-}

-- | Convert a `FailT` computation into an `ExceptT`.
--
-- >>> exceptFailT (fail "A bad thing" >> pure () :: Fail String ())
-- ExceptT (Identity (Left FailException
-- "A bad thing"
-- CallStack (from HasCallStack):
-- ...
exceptFailT :: (HasCallStack, Typeable e, Show e, Monad m) => FailT e m a -> ExceptT FailException m a
exceptFailT m =
  ExceptT $
    runFailAggT m >>= \case
      Right x -> pure $ Right x
      Left errMsgs ->
        pure $
          Left $
            FailException
              { failMessages = errMsgs
              , failCallStack = ?callStack
              }
{-# INLINE exceptFailT #-}

-- | An exception that is produced by the `FailT` monad transformer.
data FailException where
  FailException
    :: (Typeable e, Show e)
    => { failMessages :: [e]
       , failCallStack :: CallStack
       }
    -> FailException

instance Show FailException where
  show FailException{failMessages, failCallStack} =
    mconcat $
      intersperse "\n" $
        "FailException"
          : NE.toList (toFailureNonEmpty (show <$> failMessages))
          ++ [prettyCallStack failCallStack]

instance Exception FailException

toFailureNonEmpty :: IsString e => [e] -> NE.NonEmpty e
toFailureNonEmpty xs =
  case NE.nonEmpty xs of
    Nothing -> "No failure reason given" NE.:| []
    Just ne -> ne

toFailureDelimited :: (IsString e, Semi.Semigroup e) => [e] -> e
toFailureDelimited = Semi.sconcat . NE.intersperse ", " . toFailureNonEmpty

-- | Use the `MonadThrow` instance to raise a `FailException` in the underlying monad.
--
-- >>> throwFailT (failT "One thing went wrong")
-- *** Exception: FailException
-- "One thing went wrong"
-- ...
-- >>> throwFailT (failT "One thing went wrong") :: Maybe ()
-- Nothing
throwFailT :: (HasCallStack, Typeable e, Show e, MonadThrow m) => FailT e m a -> m a
throwFailT f = do
  runFailAggT f >>= \case
    Right x -> pure x
    Left errMsgs ->
      throwM $
        FailException
          { failMessages = errMsgs
          , failCallStack = ?callStack
          }
{-# INLINEABLE throwFailT #-}

instance Functor m => Functor (FailT e m) where
  fmap f (FailT m) = FailT (fmap (fmap f) m)
  {-# INLINE fmap #-}

instance Monad m => Applicative (FailT e m) where
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
  FailT m *> FailT k = FailT $ m *> k
  {-# INLINE (*>) #-}

-- | Short-circuites on the first failing operation.
instance (IsString e, Monad m) => Monad (FailT e m) where
  FailT m >>= k =
    FailT $
      m >>= \case
        Left merr -> return $ Left merr
        Right a -> runFailAggT $ k a
  {-# INLINE (>>=) #-}

#if !(MIN_VERSION_base(4,13,0))
  fail = FailT . return . Left . pure . fromString
  {-# INLINE fail #-}
#endif

instance (IsString e, Monad m) => F.MonadFail (FailT e m) where
  fail = FailT . return . Left . pure . fromString
  {-# INLINE fail #-}

instance Foldable f => Foldable (FailT e f) where
  foldMap f (FailT m) = foldMap (either (const mempty) f) m
  {-# INLINE foldMap #-}

instance Traversable f => Traversable (FailT e f) where
  traverse f (FailT m) = FailT <$> traverse (either (pure . Left) (fmap Right . f)) m
  {-# INLINE traverse #-}

-- | Short-circuits on the first successful operation, combines failures otherwise.
instance Monad m => Alternative (FailT e m) where
  empty = FailT $ pure (Left [])
  {-# INLINE empty #-}
  FailT m <|> FailT k = FailT $ do
    m >>= \case
      Left merr ->
        k >>= \case
          Left kerr -> pure $ Left $ merr ++ kerr
          Right result -> pure $ Right result
      Right x -> pure (Right x)
  {-# INLINEABLE (<|>) #-}

-- | Executes all monadic actions and combines all successful results using a `Semi.Semigroup`
-- instance. Combines together all failures as well, until a successful operation.
instance (Monad m, Semi.Semigroup a) => Semi.Semigroup (FailT e m a) where
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
          Right y -> pure $ Right (x Semi.<> y)
  {-# INLINEABLE (<>) #-}

instance (Monad m, Semi.Semigroup a) => Monoid (FailT e m a) where
  mempty = empty
  {-# INLINE mempty #-}
#if !(MIN_VERSION_base(4,11,0))
  mappend = (Semi.<>)
#endif

instance (IsString e, MonadIO m) => MonadIO (FailT e m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadTrans (FailT e) where
  lift = FailT . fmap Right
  {-# INLINE lift #-}

instance (IsString e, MonadZip m) => MonadZip (FailT e m) where
  mzipWith f (FailT a) (FailT b) = FailT $ mzipWith (liftA2 f) a b
  {-# INLINE mzipWith #-}

#if MIN_VERSION_base(4,12,0)
instance Contravariant f => Contravariant (FailT e f) where
  contramap f = FailT . contramap (fmap f) . runFailAggT
  {-# INLINE contramap #-}
#endif

instance (Eq e, Eq1 m) => Eq1 (FailT e m) where
  liftEq eq (FailT x) (FailT y) = liftEq (liftEq eq) x y
  {-# INLINE liftEq #-}

instance (Ord e, Ord1 m) => Ord1 (FailT e m) where
  liftCompare comp (FailT x) (FailT y) =
    liftCompare (liftCompare comp) x y
  {-# INLINE liftCompare #-}

instance (Read e, Read1 m) => Read1 (FailT e m) where
  liftReadsPrec rp rl =
    readsData $
      readsUnaryWith (liftReadsPrec rp' rl') "FailT" FailT
    where
      rp' = liftReadsPrec rp rl
      rl' = liftReadList rp rl

instance (Show e, Show1 m) => Show1 (FailT e m) where
  liftShowsPrec sp sl d (FailT m) =
    showsUnaryWith (liftShowsPrec sp' sl') "FailT" d m
    where
      sp' = liftShowsPrec sp sl
      sl' = liftShowList sp sl

instance (Eq e, Eq1 m, Eq a) => Eq (FailT e m a) where
  (==) = eq1
  {-# INLINE (==) #-}

instance (Ord e, Ord1 m, Ord a) => Ord (FailT e m a) where
  compare = compare1
  {-# INLINE compare #-}

instance (Read e, Read1 m, Read a) => Read (FailT e m a) where
  readsPrec = readsPrec1

instance (Show e, Show1 m, Show a) => Show (FailT e m a) where
  showsPrec = showsPrec1

instance (IsString e, MonadReader r m) => MonadReader r (FailT e m) where
  ask = lift ask
  {-# INLINE ask #-}
  local = mapFailT . local
  {-# INLINE local #-}
  reader = lift . reader
  {-# INLINE reader #-}

instance (IsString e, MonadState s m) => MonadState s (FailT e m) where
  get = lift get
  {-# INLINE get #-}
  put = lift . put
  {-# INLINE put #-}
  state = lift . state
  {-# INLINE state #-}

instance (IsString e, MonadError e m) => MonadError e (FailT e m) where
  throwError = lift . throwError
  {-# INLINE throwError #-}
  catchError = liftCatch catchError
  {-# INLINE catchError #-}

instance (IsString e, MonadWriter w m) => MonadWriter w (FailT e m) where
  writer = lift . writer
  {-# INLINE writer #-}
  tell = lift . tell
  {-# INLINE tell #-}
  listen = liftListen listen
  {-# INLINE listen #-}
  pass = liftPass pass
  {-# INLINE pass #-}

instance (IsString e, MonadCont m) => MonadCont (FailT e m) where
  callCC = liftCallCC callCC
  {-# INLINE callCC #-}

-- | Lift a @callCC@ operation to the new monad.
liftCallCC
  :: (((Either [e] a -> m (Either [e] b)) -> m (Either [e] a)) -> m (Either [e] a))
  -> ((a -> FailT e m b) -> FailT e m a)
  -> FailT e m a
liftCallCC ccc f = FailT $ ccc $ \c ->
  runFailAggT (f (FailT . c . Right))
{-# INLINE liftCallCC #-}

-- | Lift a @`catchE`@ operation to the new monad.
liftCatch
  :: (m (Either [e] a) -> (e -> m (Either [e] a)) -> m (Either [e] a))
  -> FailT e m a
  -> (e -> FailT e m a)
  -> FailT e m a
liftCatch f m h = FailT $ f (runFailAggT m) (runFailAggT . h)
{-# INLINE liftCatch #-}

-- | Lift a @`listen`@ operation to the new monad.
liftListen
  :: Monad m
  => (m (Either [e] a) -> m (Either [e] a, w))
  -> (FailT e m) a
  -> (FailT e m) (a, w)
liftListen l = mapFailT $ \m -> do
  (a, w) <- l m
  return $! fmap (\r -> (r, w)) a
{-# INLINE liftListen #-}

-- | Lift a @`pass`@ operation to the new monad.
liftPass
  :: Monad m
  => (m (Either [e] a, w -> w) -> m (Either [e] a))
  -> (FailT e m) (a, w -> w)
  -> (FailT e m) a
liftPass p = mapFailT $ \m -> p $ do
  a <- m
  return $! case a of
    Left errs -> (Left errs, id)
    Right (v, f) -> (Right v, f)
{-# INLINE liftPass #-}
