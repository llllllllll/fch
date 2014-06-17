-- |
-- Module      : FCH.Python
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- The module for generating python modules.

module FCH.Python (python) where

import Data.Char (toUpper)

import FCH.Data

-- | The python program language.
python :: Language
python = Language { mkComment = (++) "# "
                  , mkString  = \fl cs -> ufl fl ++ "_STR = " ++ mkStrLit cs
                  , mkLen     = \fl cs -> ufl fl ++ "_LEN = "
                                ++ show (length cs)
                  , mdSetup   = const . const ""
                  , mdCleanup = const . const ""
                  , checkFile = True
                  }
  where
      ufl = map toUpper
