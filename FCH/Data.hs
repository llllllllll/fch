-- |
-- Module      : FCH.Data
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- The data module for fch.

module FCH.Data where

-- | A language spec for fch.
-- mkComment: Takes the string and generates a line comment for this language.
-- mkString: Takes the file name and a string and makes the string variable.
-- mkLen: Takes the file name and the string and makes the length variable.
-- mdSetup: Takes the file name and the module name and does the module setup.
-- mdCleanup: Takes the file name and the module name and cleans up the module.
-- checkFile: Should this language check the file name for the identifiers.
data Language = Language { mkComment :: String -> String
                         , mkString  :: FilePath -> String -> String
                         , mkLen     :: FilePath -> String -> String
                         , mdSetup   :: FilePath -> String -> String
                         , mdCleanup :: FilePath -> String -> String
                         , checkFile :: Bool
                         }

-- | Makes a string litteral.
mkStrLit :: String -> String
mkStrLit cs = '"' : (cs >>= checkSpecial) ++ "\""
  where
      checkSpecial '"'  = "\\\""
      checkSpecial '\n' = "\\n"
      checkSpecial '\t' = "\\t"
      checkSpecial c    = [c]
