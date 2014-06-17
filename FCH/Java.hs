-- |
-- Module      : FCH.Java
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- The language module for C headers.

module FCH.Java (java) where

import Data.Char (toUpper)

import FCH.Data

-- | The c programming language (and maybe c++, who cares).
java :: Language
java = Language { mkComment = (++) "// "
                , mkString  = \fl cs -> "final String " ++ ufl fl
                              ++ "_STR = " ++ mkStrLit cs ++ ";"
                , mkLen     = \fl cs -> "final int " ++ ufl fl ++ "_LEN = "
                              ++ show (length cs) ++ ";"
                , mdSetup   = \_ md  -> "public class " ++ md ++ "{"
                , mdCleanup = \_ _   -> "}"
                , checkFile = True
             }
  where
      ufl = map toUpper
