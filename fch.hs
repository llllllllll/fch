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

import Control.Applicative   ((<$>))
import Control.Monad         (when,mapM_,unless,join,liftM)
import Data.Char             (toLower)
import Data.List             (intercalate)
import Data.Maybe            (isNothing)
import System.Console.GetOpt ( ArgOrder(..),OptDescr(..),ArgDescr(..)
                             , getOpt,usageInfo)
import System.Environment    (getArgs)
import System.FilePath       (takeBaseName)
import System.IO             ( openFile,Handle(..),IOMode(..)
                             , hPutStrLn,stdout,hClose)

import FCH.Data    (Language,mkComment,mkString,mkLen
                   ,mdSetup,mdCleanup,checkFile)
import FCH.C       (c)
import FCH.Haskell (haskell)
import FCH.Java    (java)
import FCH.Scheme  (scheme)


-- | The command line flags.
data Flag = Version             -- ^ Output the version information.
          | Help                -- ^ Output the help information.
          | LangOpt String      -- ^ Choose the language to output to.
          | ModuleSetup String  -- ^ The name of the module to setup.
          | StdoutOpt           -- ^ Send the output to stdout.
          | OutputFile FilePath -- ^ Where are we writing the data to.
            deriving (Eq)

-- | The list of command line options.
options :: [OptDescr Flag]
options = [ Option "v" ["version"] (NoArg Version) "Prints version information."
          , Option "h" ["help"] (NoArg Help) "Outputs the help message"
          , Option "l" ["language"] (ReqArg LangOpt "LANGUAGE")
                       "The language to output as."
          , Option "m" ["module"] (ReqArg ModuleSetup "MODULE")
                       "The name of the module to setup."
          , Option "s" ["stdout"] (NoArg StdoutOpt) "Print the output to stdout"
          , Option "o" ["output"] (ReqArg OutputFile "FILE")
                   "The name of the output file (defaults to <INPUTFILE>.ch"
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

-- | Converts the string representation of the language argument to a Langage
-- type. The string is toLower'ed first so case does not matter.
stringToLang :: String -> Maybe Language
stringToLang l
    | l `elem` cOpts       = Just c
    | l `elem` haskellOpts = Just haskell
    | l `elem` schemeOpts  = Just scheme
    | l `elem` javaOpts    = Just java
    | otherwise            = Nothing
  where
      cOpts       = ["c","c++","cpp"]
      haskellOpts = ["haskell","hask","hs"]
      schemeOpts  = ["scheme","scm","ss"]
      javaOpts    = ["java"]  -- More may be added later.

-- | Gets the language argument out of the list of flags.
getLang :: [Flag] -> Maybe Language
getLang = foldr (\a b -> case a of
                             LangOpt l -> stringToLang l
                             _         -> b) (Just haskell)

-- | Gets the module name argument out of the list of flags if it exists.
getModuleName :: [Flag] -> Maybe String
getModuleName = foldr (\a b -> case a of
                                   ModuleSetup m -> Just m
                                   _             -> b) Nothing

-- | Gets the output stream,
getOutFl :: [Flag] -> Maybe FilePath -> IO Handle
getOutFl fs mf
    | StdoutOpt `elem` fs = return stdout
    | otherwise = case (foldr (\a b -> case a of
                                           OutputFile f -> Just f
                                           _            -> b) Nothing fs,mf) of
                       (Just f,_) -> openFile f WriteMode
                       (_,Just f) -> openFile (f ++ ".ch") WriteMode
                       (_,_)      -> openFile "fch.ch" WriteMode

-- | Handles the help and version commands.
helpAndVersion :: [Flag] -> IO Bool
helpAndVersion fs = let h = Help `elem` fs
                        v = Version `elem` fs
                    in when h (putStrLn helpString)
                    >> when v (putStrLn versionString)
                    >> return (h || v)

-- | Invokes fch with the flags and output destination.
invokeFch :: [Flag] -> Maybe FilePath -> Handle -> IO ()
invokeFch fs a = case getLang fs of
                     Nothing -> const $ putStrLn invalidLangString
                     Just l -> fch l a (getModuleName fs)

-- | Parses the options.
handleOpts :: ([Flag],[String],[String]) -> IO ()
handleOpts ([],[],_)  = putStrLn noArgString
handleOpts (fs,[],_)  = helpAndVersion fs
                        >>= \hv -> unless hv (getOutFl fs Nothing
                                              >>= invokeFch fs Nothing)
handleOpts (fs,a:_,_) = helpAndVersion fs
                        >> getOutFl fs (Just a)
                        >>= invokeFch fs (Just a)

-- | Runs fch over the valid arguments.
fch :: Language -> Maybe FilePath -> Maybe String -> Handle -> IO ()
fch l mf mm out = (case mf of
                       Nothing -> liftM ((,) "fch") getContents
                       Just f  -> liftM ((,) f) $ readFile f)
                  >>= \(f,src) -> let f' = (if checkFile l
                                              then safeIdentifier
                                              else id) $ takeBaseName f
                                      ss = mkString l f' src
                                      ls = mkLen    l f' src
                                      ms = case mm of
                                               Just m  -> mdSetup l f' m
                                               Nothing -> ""
                                      mc = case mm of
                                               Just m  -> mdCleanup l f' m
                                               Nothing -> ""
                                  in hPutStrLn out
                                         (intercalate "\n" (comments l
                                                            ++ [ms,ss,ls,mc]))
                  >> hClose out
  where
      comments l = map (mkComment l)
                   [ "This file was generated by fch."
                   , "To submit a bug report or add a language module, go to:"
                   , "<https://github.com/llllllllll/fch>"
                   ]
      safeIdentifier = map swapChar
      swapChar c
          | c `elem` badChars = '_'
          | otherwise         = c
        where
            badChars = "+-!@#$%^&*(){}[]`~<>,.?"

-- | Grabs the args, parses them and operates on it.
main :: IO ()
main = getArgs >>= handleOpts . getOpt Permute options
