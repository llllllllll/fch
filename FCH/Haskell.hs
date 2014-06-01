-- |
-- Module      : FCH.Haskell
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- The language module for Haskell module.

module FCH.Haskell (haskell) where

import FCH.Data

-- | Module specs for the Haskell language.
haskell :: Language
haskell = Language { mkComment  = \cs    -> "-- " ++ cs
                   ,  mkString  = \fl cs -> let strName = fl ++ "Str"
                                            in strName ++ " :: String\n"
                                                   ++ strName ++ " = "
                                                   ++ mkStrLit cs
                   , mkLen      = \fl cs -> let intName = fl ++ "Len"
                                            in intName ++ " :: Int\n"
                                                   ++ intName ++ " = "
                                                   ++ show (length cs)
                   , mdSetup    = \fl m  -> "module " ++ m ++ " (" ++ fl
                                  ++ "Len, " ++ fl ++ "Str) where"
                   , mdCleanup  = \_ _   -> ""
                   , checkFile  = True
                   }
