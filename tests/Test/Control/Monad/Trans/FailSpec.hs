{-# LANGUAGE CPP #-}
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

instance (Arbitrary e, Arbitrary1 m, Applicative m) => Arbitrary1 (FailT e m) where
  liftArbitrary gen =
    oneof
      [ FailT . pure . Left <$> arbitrary
      , FailT . pure . Right <$> gen
      ]
  {-# INLINE liftArbitrary #-}

spec :: Spec
spec = do
  describe "FailT" $ do
    prop "runFail" $ \err -> runFail (fail err :: Fail String ()) `shouldBe` Left err
    describe "Instance Laws" $ do
      let px = Proxy :: Proxy (FailT String Maybe (Sum Int))
      let px1 = Proxy :: Proxy (FailT String Maybe)
      mapM_ laws $
        [ eqLaws px
        , ordLaws px
        , showLaws px
        , showReadLaws px
        , monoidLaws px
        , semigroupLaws px
        , alternativeLaws px1
        , applicativeLaws px1
        , foldableLaws px1
        , functorLaws px1
        , monadLaws px1
        , monadZipLaws px1
        , traversableLaws px1
        ]
          ++ extraLaws px
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

extraLaws :: (Semigroup a, Monoid a, Eq a, Arbitrary a, Show a) => Proxy a -> [Laws]
#if MIN_VERSION_quickcheck_classes(0,6,2)
extraLaws px = [semigroupMonoidLaws px]
#else
extraLaws _px = []
#endif
