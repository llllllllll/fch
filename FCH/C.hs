-- |
-- Module      : FCH.C
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- The language module for C headers.

module FCH.C (c) where

import FCH.Data

-- | The c programming language (and maybe c++, who cares).
c :: Language
c = Language { mkComment = (++) "// "
             , mkString  = \fl cs -> "const char* const " ++ fl
                           ++ "_str = " ++ mkStrLit cs ++ ";"
             , mkLen     = \fl cs -> "const size_t " ++ fl ++ "_len = "
                           ++ show (length cs) ++ ";"
             , mdSetup   = \_ md  -> "#ifndef " ++ md ++ "\n#define " ++ md
             , mdCleanup = \_ _   -> "#endif"
             , checkFile = True
             }
