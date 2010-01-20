#! /usr/bin/env runhaskell

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- base
import Control.Monad       ( (>>), return )
import Data.Bool           ( Bool )
import System.Cmd          ( system )
import System.FilePath     ( (</>) )
import System.IO           ( IO )

-- cabal
import Distribution.Simple ( defaultMainWithHooks
                           , simpleUserHooks
                           , UserHooks(runTests, haddockHook)
                           , Args
                           )

import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..) )
import Distribution.Simple.Program        ( userSpecifyArgs )
import Distribution.Simple.Setup          ( HaddockFlags )
import Distribution.PackageDescription    ( PackageDescription(..) )

-------------------------------------------------------------------------------

main ∷ IO ()
main = defaultMainWithHooks hooks
  where
    hooks = simpleUserHooks
            { runTests    = runTests'
            , haddockHook = haddockHook'
            }

-- Run a 'test' binary that gets built when configured with '-ftest'.
runTests' ∷ Args → Bool → PackageDescription → LocalBuildInfo → IO ()
runTests' _ _ _ _ = system testcmd >> return ()
  where testcmd = "." </> "dist" </> "build" </> "test" </> "test"

-- Define __HADDOCK__ for CPP when running haddock.
haddockHook' ∷ PackageDescription → LocalBuildInfo → UserHooks → HaddockFlags → IO ()
haddockHook' pkg lbi =
  haddockHook simpleUserHooks pkg (lbi { withPrograms = p })
  where
    p = userSpecifyArgs "haddock" ["--optghc=-D__HADDOCK__"] (withPrograms lbi)
