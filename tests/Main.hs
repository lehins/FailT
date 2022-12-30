module Main where

import Spec
import System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)
import Test.Hspec.Runner

config :: Config
config =
  defaultConfig
    { configTimes = True
    , configColorMode = ColorAlways
    }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  hspecWith config spec
