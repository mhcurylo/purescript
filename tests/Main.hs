{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Prelude ()
import Prelude.Compat

import Test.Tasty

import qualified TestCompiler
import qualified TestCoreFn
import qualified TestDocs
import qualified TestHierarchy
import qualified TestPrimDocs
import qualified TestPsci
import qualified TestIde
import qualified TestPscPublish
import qualified TestPscBundle
import qualified TestUtils

import System.IO (hSetEncoding, stdout, stderr, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  heading "Updating support code"
  TestUtils.updateSupportCode
  heading "Prim documentation test suite"
  TestPrimDocs.main

  ideTests <- TestIde.main
  compilerTests <- TestCompiler.main
  psciTests <- TestPsci.main
  pscBundleTests <- TestPscBundle.main
  coreFnTests <- TestCoreFn.main
  docsTests <- TestDocs.main
  publishTests <- TestPscPublish.main
  hierarchyTests <- TestHierarchy.main

  defaultMain $
    testGroup
      "Tests"
      [ 
--        compilerTests
--      , psciTests
       pscBundleTests
--      , ideTests
--      , coreFnTests
--      , docsTests
--      , publishTests
--      , hierarchyTests
      ]

  where
  heading msg = do
    putStrLn ""
    putStrLn $ replicate 79 '#'
    putStrLn $ "# " ++ msg
    putStrLn $ replicate 79 '#'
    putStrLn ""
