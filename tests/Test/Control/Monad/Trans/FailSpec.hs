{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Control.Monad.Trans.FailSpec (spec) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Fail
import Data.Proxy
import Data.Semigroup
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Classes

laws :: Laws -> Spec
laws Laws{lawsTypeclass, lawsProperties} =
  describe lawsTypeclass $ mapM_ (uncurry prop) lawsProperties

instance (Applicative m, Arbitrary e, Arbitrary a) => Arbitrary (FailT e m a) where
  arbitrary = FailT . pure <$> arbitrary

spec :: Spec
spec = do
  describe "FailT" $ do
    prop "runFail" $ \err -> runFail (fail err :: Fail String ()) `shouldBe` Left err
    describe "Instance Laws" $ do
      let px = Proxy :: Proxy (FailT String Maybe (Sum Int))
      let px1 = Proxy :: Proxy (FailT String Maybe)
      laws $ eqLaws px
      laws $ ordLaws px
      laws $ showLaws px
      laws $ showReadLaws px
      laws $ monoidLaws px
      laws $ semigroupLaws px
      laws $ semigroupMonoidLaws px
      laws $ alternativeLaws px1
      laws $ applicativeLaws px1
      laws $ foldableLaws px1
      laws $ functorLaws px1
      laws $ monadLaws px1
      laws $ monadZipLaws px1
      laws $ traversableLaws px1
    describe "mtl" $ do
      prop "MonadReader" $ \msg (x :: Int) -> do
        let t :: FailT String (ReaderT Int IO) Int
            t = do
              x' <- ask
              pure $ succ x'
        runReaderT (runFailT t) x `shouldReturn` Right (x + 1)
        runReaderT (runFailT (fail msg >> pure x)) x `shouldReturn` Left msg
      prop "MonadState" $ \msg (x :: Int) -> do
        let t :: FailT String (StateT Int IO) ()
            t = do
              x' <- get
              liftIO (x' `shouldBe` x)
              put (x' + 1)
              modify' succ
        execStateT (runFailT t) x `shouldReturn` (x + 2)
        runStateT (runFailT (fail msg >> modify' succ)) x `shouldReturn` (Left msg, x)
