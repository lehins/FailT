{-# LANGUAGE CPP #-}

module Main where

import Spec (spec)
import System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)
import Test.Hspec.Runner

config :: Config
#if !(MIN_VERSION_hspec(2,8,0))
config = defaultConfig
#else
config =
  defaultConfig
    { configTimes = True
    , configColorMode = ColorAlways
    }
#endif

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  hspecWith config spec
