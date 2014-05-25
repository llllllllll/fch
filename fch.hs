-- |
-- Module      : FCH
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- The main module for fch.

import Control.Monad         (when,mapM_,unless)
import Data.Char             (toLower)
import Data.List             (intercalate)
import Data.Maybe            (isNothing)
import System.Console.GetOpt (ArgOrder(..),OptDescr(..),ArgDescr(..)
                             ,getOpt,usageInfo)
import System.Environment    (getArgs)
import System.FilePath       (takeBaseName)

import FCH.Data    (Language,mkComment,mkString,mkLen,reqSetup,mdSetup,mdCleanup)
import FCH.C       (c)
import FCH.Haskell (haskell)
import FCH.Scheme  (scheme)


-- | The command line flags.
data Flag = Version            -- ^ Output the version information.
          | Help               -- ^ Output the help information.
          | LangOpt String     -- ^ Choose the language to output to.
          | ModuleSetup String -- ^ The name of the module to setup.
            deriving (Eq)

-- | The list of command line options.
options :: [OptDescr Flag]
options = [ Option "v" ["version"] (NoArg Version) "Prints version information."
          , Option "h" ["help"] (NoArg Help) "Outputs the help message"
          , Option "l" ["language"] (ReqArg LangOpt "LANGUAGE")
                       "The language to output as."
          , Option "m" ["module"] (ReqArg ModuleSetup "MODULE")
                       "The name of the module to setup."
          ]

-- | The string to print in case of the -h flag.
helpString :: String
helpString = "Usage:" ++ usageInfo "" options
             ++ "Execute `fch -l LANG FILE` to generate the file, FILE.ch.\n\
                \This file will contain the input file as a bound string \
                \and a \nconstant that holds the length of the file.\nThe \
                \rules defining how this process occurs are specific to the\n\
                \target language, so you might want try it on a test file \
                \first.\nPlease submit any bug reports to <joejev@gmail.com>."

-- | The string to print when an invalid language is selected.
invalidLangString :: String
invalidLangString = "error: Invalid language selection."

-- | The string to print in case of the -v flag.
versionString :: String
versionString = "fch: Files Compiled to Headers: version 0.0.0.1 (2014.05.25)\n\
                \Copyright (C) 2014 Joe Jevnik.\n\
                \This is free software; see the source for copying \
                \conditions.  There is NO\nwarranty; not even for \
                \MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."

-- | The string to print when no options are given.
noArgString :: String
noArgString = "fch: no input files\nFor usage help, try fch -h"

-- | The string to print when a language requires a module and none is given.
reqSetupString :: String
reqSetupString = "This language needs a module name, please pass -m MODULE."

-- | Converts the string representation of the language argument to a Langage
-- type. The string is toLower'ed first so case does not matter.
stringToLang :: String -> Maybe Language
stringToLang l
    | l `elem` cOpts       = Just c
    | l `elem` haskellOpts = Just haskell
    | l `elem` schemeOpts  = Just scheme
    | otherwise            = Nothing
  where
      cOpts       = ["c","c++","cpp"]
      haskellOpts = ["haskell","hask","hs"]
      schemeOpts  = ["scheme","scm","ss"]

-- | Gets the language argument out of the list of flags.
getLang :: [Flag] -> Maybe Language
getLang []             = Just haskell
getLang (LangOpt l:fs) = stringToLang $ map toLower l
getLang (_:fs)         = getLang fs

-- | Gets the module name argument out of the list of flags if it exists.
getModuleName :: [Flag] -> Maybe String
getModuleName []                 = Nothing
getModuleName (ModuleSetup m:fs) = Just m
getModuleName (_:fs)             = getModuleName fs

-- | Parses the options.
handleOpts :: ([Flag],[String],[String]) -> IO ()
handleOpts ([],[],_)  = putStrLn noArgString
handleOpts (fs,[],_)  = when (Help `elem` fs) (putStrLn helpString)
                        >> when (Version `elem` fs) (putStrLn versionString)
handleOpts (fs,a:_,_) = when (Help `elem` fs) (putStrLn helpString)
                        >> when (Version `elem` fs) (putStrLn versionString)
                        >> case getLang fs of
                               Nothing -> putStrLn invalidLangString
                               Just l  -> fch l a $ getModuleName fs

-- | Runs fch over the valid arguments.
fch :: Language -> FilePath -> Maybe String -> IO ()
fch l f mm
    | reqSetup l && isNothing mm = putStrLn reqSetupString
    | otherwise
        = readFile f
          >>= \src -> let f' = takeBaseName f
                          ss = mkString l f' src
                          ls = mkLen    l f' src
                          ms = case mm of
                                   Just m  -> mdSetup l f' m
                                   Nothing -> ""
                          mc = case mm of
                                 Just m  -> mdCleanup l f' m
                                 Nothing -> ""
                      in writeFile (f ++ ".ch") $ intercalate "\n"
                             (comments l ++ [ms,ss,ls,mc]) ++ "\n"
  where
      comments l = map (mkComment l)
                   [ "This file was generated by fch."
                   , "To submit a bug report or add a language module, go to:"
                   , "<https://github.com/llllllllll/fch>"
                   ]

-- | Grabs the args, parses them and operates on it.
main :: IO ()
main = getArgs >>= handleOpts . getOpt Permute options
