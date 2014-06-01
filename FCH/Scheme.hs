-- |
-- Module      : FCH.Scheme
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- The language module for scheme headers.

module FCH.Scheme (scheme) where

import FCH.Data

-- | The scheme programming languages, might work for other lisps.
scheme :: Language
scheme = Language { mkComment = \cs    -> "; " ++ cs
                  , mkString  = \fl cs -> "(define " ++ fl ++ "-string "
                                ++ mkStrLit cs ++ ")"
                  , mkLen     = \fl cs -> "(define " ++ fl ++ "-length "
                                ++ show (length cs) ++ ")"
                  , mdSetup   = \_ m   -> "(define-module (" ++ m ++ "))"
                  , mdCleanup = \fl _  -> "(export " ++ fl ++ "-string "
                                ++ fl ++ "-len)"
                  , checkFile = False
                  }
