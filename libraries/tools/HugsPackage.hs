module Main where

import Control.Monad (filterM, when)
import Data.Char
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Maybe (mapMaybe)
import Distribution.InstalledPackageInfo
import Distribution.ParseUtils (showError)
import Distribution.PreProcess
import Distribution.PreProcess.Unlit
import Distribution.Simple.Utils
import System.Directory
import System.Exit
import System.Environment
import System.FilePath
import System.IO

-- Convert a Haskell package for use by Hugs

-- Currently reads InstalledPackageInfo from package.conf
-- (or package.conf.in after preprocessing with cpp).

-- Missing features compared with the shell version:
--	* won't work on Windows, and assumes gcc
--	* hugs/exclude (use #if's in package.conf.in instead)
--	* gets ffihugs from PATH
--	* doesn't use *_hsc_make.c if present

usage :: String
usage = "hugs_package srcDir destDir"

-- Package description file.
-- If this isn't present, try to preprocess package.conf.in with cpp.
packageFile :: String
packageFile = "package.conf"

-- Currently getting build information from InstalledPackageInfo
type BuildParameters = InstalledPackageInfo

-- cpp defines
defHugs, defCallConv :: String
defHugs = "-D__HUGS__"
-- ccall on Unix, stdcall on Windows
defCallConv = "-DCALLCONV=ccall"

main :: IO ()
main = do
	args <- getArgs
	case args of
	    [srcDir, destDir] -> hugsPackage srcDir destDir
	    _ -> do
		name <- getProgName
		hPutStrLn stderr usage
		exitFailure

hugsPackage :: FilePath -> FilePath -> IO ()
hugsPackage srcDir destDir = do
	exists <- doesFileExist fpath
	inp <- if exists then readFile fpath else do
	    withTempFileDef $ \ tmpFile -> do
		maybeExit (ppCpp includes packageFileIn tmpFile)
		readFile tmpFile
	case parseInstalledPackageInfo inp of
	    Left err ->
		die (fpath ++ ": " ++ showError err)
	    Right pkgInfo -> do
		files <- prepPackage pkgInfo srcDir destDir
		ffiFiles <- filterM testFFIModule files
		mapM_ (compileFFI pkgInfo srcDir) ffiFiles
  where
	includes = ["-I" ++ (srcDir `joinFileName` "include")]
	fpath = srcDir `joinFileName` packageFile
	packageFileIn = fpath ++ ".in"

-- Pass 1: preprocess files

-- Preprocess a package, returning names of output files.
prepPackage :: BuildParameters -> FilePath -> FilePath -> IO [FilePath]
prepPackage pkgInfo srcDir destDir =
	sequence [prepModule (ppHandlers includes) useCpp (ppCpp includes)
			(stem srcDir mod) (stem destDir mod) |
		mod <- modules]
  where modules = exposedModules pkgInfo ++ hiddenModules pkgInfo
	stem dir mod = dir `joinFileName` dotToSep mod
	includes = map ("-I" ++) incDirs
	incDirs = (srcDir `joinFileName` "include") : includeDirs pkgInfo
	useCpp = True	-- TODO: look for CPP in extensions

-- Preprocess a file, returning name of output file.
prepModule :: [PPHandler] -> Bool -> PreProcessor ->
		FilePath -> FilePath -> IO FilePath
prepModule handlers useCpp cpp srcStem destStem = do
	createIfNotExists True (dirname destStem)
	chooseHandler handlers
  where
	dirname f = fst (splitFileName f)
	chooseHandler [] = die (srcStem ++ ".*: not found")
	chooseHandler ((suffix, pps):handlers) = do
		exists <- doesFileExist srcFile
		if exists then do
			opts <- getOptions srcFile
			let pps' = if useCpp || "-cpp" `elem` opts
					then pps ++ [cpp] else pps
			ppCompose pps' srcFile destFile
			return destFile
		    else chooseHandler handlers
	  where srcFile = srcStem ++ suffix
		destFile
		  | suffix == ".lhs" = destStem ++ ".lhs"
		  | otherwise        = destStem ++ ".hs"

-- Preprocessors

type PPHandler = (String, [PreProcessor])

ppHandlers :: [String] -> [PPHandler]
ppHandlers includes = [
	(".hs",  []),
	(".lhs", []),
	-- TODO: ("_hsc_make.c", ???),
	(".hsc", [standardPP "hsc2hs" (defHugs : includes)]),
	(".ly",  [ppHappy]),
	(".y",   [ppHappy])]

ppCompose :: [PreProcessor] -> PreProcessor
ppCompose [] = ppIdentity
ppCompose pps = foldr1 (>->) pps

ppCpp :: [String] -> PreProcessor
ppCpp flags inFile outFile =
	rawSystemPath "cpp"
		(["-traditional", "-P", defHugs, defCallConv] ++
		 flags ++ [inFile, outFile])

ppIdentity, ppHappy :: PreProcessor
ppIdentity inFile outFile = copyFile inFile outFile >> return ExitSuccess
ppHappy = standardPP "happy" []

standardPP :: String -> [String] -> PreProcessor
standardPP eName args inFile outFile
    = rawSystemPath eName (args ++ ["-o" ++ outFile, inFile])

(>->) :: PreProcessor -> PreProcessor -> PreProcessor
(p1 >-> p2) inFile outFile = withTempFileDef $ \ tmpFile ->
	p1 inFile tmpFile .&&. p2 tmpFile outFile

(.&&.) :: IO ExitCode -> IO ExitCode -> IO ExitCode
c1 .&&. c2 = do
	status <- c1
	case status of
	    ExitSuccess -> c2
	    _ -> return status

-- Pass 2: compile FFI modules

compileFFI :: BuildParameters -> FilePath -> FilePath -> IO ExitCode
compileFFI pkgInfo srcDir file = do
	options <- getOptions file
	let incs = uniq (sort (includeOpts options ++ pkg_incs))
	let hugsArgs = "-98" : map ("-i" ++) incs
	cfiles <- getCFiles file
	let cArgs =
		map ("-I" ++) incDirs ++
		map (joinFileName srcDir) cfiles ++
		extraCcOpts pkgInfo ++
		map ("-l" ++) libs ++
		extraLdOpts pkgInfo ++
		concat [["-framework", f] | f <- extraFrameworks pkgInfo]
	rawSystemPath "ffihugs" (hugsArgs ++ file : cArgs)
  where	pkg_incs = ["\"" ++ inc ++ "\"" | inc <- includes pkgInfo]
	incDirs = (srcDir `joinFileName` "include") : includeDirs pkgInfo
	libs = [lib | lib <- extraLibraries pkgInfo,
			not ("HS" `isPrefixOf` lib)]

includeOpts :: [String] -> [String]
includeOpts [] = []
includeOpts ("-#include" : arg : opts) = arg : includeOpts opts
includeOpts (_ : opts) = includeOpts opts

uniq :: Ord a => [a] -> [a]
uniq (x1:x2:xs) | x1 == x2 = uniq (x2:xs)
uniq xs = xs

-- get options from OPTIONS pragmas at the start of the source file
getOptions :: FilePath -> IO [String]
getOptions file = do
	inp <- readHaskellFile file
	return $ concat $ takeWhileJust $ map (getPragma "OPTIONS") $ lines inp

takeWhileJust :: [Maybe a] -> [a]
takeWhileJust (Just x:xs) = x : takeWhileJust xs
takeWhileJust _ = []

-- get C files from CFILES pragmas throughout the source file
getCFiles :: FilePath -> IO [String]
getCFiles file = do
	inp <- readHaskellFile file
	return $ concat $ mapMaybe (getPragma "CFILES") $ lines inp

getPragma :: String -> String -> Maybe [String]
getPragma name line = case words line of
	("{-#" : pname : rest)
	    | pname == name && last rest == "#-}" -> Just (init rest)
	_ -> Nothing

-- Does this module contain any FFI stuff?
testFFIModule :: FilePath -> IO Bool
testFFIModule file = do
	inp <- readHaskellFile file
	return ("foreign" `elem` identifiers (stripComments inp))

-- List of variable identifiers (and reserved words) in a source file.
identifiers :: String -> [String]
identifiers cs = case dropWhile (not . isStartChar) cs of
	[] -> []
	rest -> ident : identifiers cs'
	  where (ident, cs') = span isFollowChar rest
  where isStartChar c = c == '_' || isAlpha c
	isFollowChar c = c == '_' || c == '\'' || isAlphaNum c

-- Strip all comments from Haskell source.
stripComments :: String -> String
stripComments = stripCommentsLevel 0
  where stripCommentsLevel :: Int -> String -> String
	stripCommentsLevel 0 ('-':'-':cs) =	-- not quite right (e.g. -->)
		stripCommentsLevel 0 (dropWhile (/= '\n') cs)
	stripCommentsLevel n ('{':'-':cs) = stripCommentsLevel (n+1) cs
	stripCommentsLevel 0 (c:cs) = c : stripCommentsLevel 0 cs
	stripCommentsLevel n ('-':'}':cs) = stripCommentsLevel (n-1) cs
	stripCommentsLevel n (c:cs) = stripCommentsLevel n cs
	stripCommentsLevel _ [] = []

-- Get the non-literate source of a Haskell module.
readHaskellFile :: FilePath -> IO String
readHaskellFile file = do
	text <- readFile file
	return $ if literate then unlit file text else text
  where literate = ".lhs" `isSuffixOf` file

withTempFileDef :: (FilePath -> IO a) -> IO a
withTempFileDef = withTempFile "." ""
