{-
Prototype Cabal setup script for Hugs
	* added --builddir option to configure
	* configure runs ./configure if present
	* then reads build parameters from Setup.buildinfo, if present
	* user packages go in $HOME/hugs/packages/<pkg>
		(add {Home}/hugs/packages/* to your path)
	* probably doesn't work on Windows, and assumes gcc

Not yet implemented:
	* executables
	* installed package description stuff
	* source distributions

Missing features compared with hugs-package:
	* hugs/exclude
	* doesn't use *_hsc_make.c if present
-}

module Main where

import Distribution.Compat.ReadP (munch)
import Distribution.Extension	(Extension(..))
import qualified Distribution.InstalledPackageInfo as Inst
import Distribution.ParseUtils
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PreProcess	(PreProcessor)
import Distribution.PreProcess.Unlit
import Distribution.Setup
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.Version

import Control.Monad	(foldM, filterM, when)
import Data.Char	(isAlpha, isAlphaNum)
import Data.List	(isSuffixOf, sort, intersperse)
import Data.Maybe	(isNothing, mapMaybe)
import System.Cmd	(rawSystem)
import System.Console.GetOpt
import System.Directory
import System.Exit
import System.Environment
import System.FilePath
import System.IO
import Text.PrettyPrint.HughesPJ (fsep, text)

-- cpp defines
defHugs :: String
defHugs = "-D__HUGS__"

defaultPackageDesc :: FilePath
defaultPackageDesc = "Setup.description"

installSuffixes :: [String]
installSuffixes = ["hs", "lhs", drop 1 dllExtension]

-- main skeleton, copied from Distribution.Simple with minor changes

main :: IO ()
main
    = do let distPref = "dist"
         let srcPref   = distPref `joinFileName` "src"
	 pkg_descr <- readPackageDescription defaultPackageDesc
	 args <- getArgs
	 (action, args) <- parseGlobalArgs args
	 case action of
	    ConfigCmd flags -> do
		(flags@(_, _, _, mb_prefix), optFns, args) <-
			parseConfigureArgs flags args [buildDirOpt]
		let prefix_opt pref opts = ("--prefix=" ++ pref) : opts
		whenM (doesFileExist "configure") $
			rawSystem "./configure"
				(maybe id prefix_opt mb_prefix args)
		pkg_descr <- getBuildParams currentDir pkg_descr
		when (not (buildPackage pkg_descr))
			exitFailure
		localbuildinfo <- configure pkg_descr flags
		writePersistBuildConfig (foldr id localbuildinfo optFns)

	    BuildCmd -> do
		(_, args) <- parseBuildArgs args []
		no_extra_flags args
		pkg_descr <- getBuildParams currentDir pkg_descr
		localbuildinfo <- getPersistBuildConfig
		build pkg_descr localbuildinfo

	    CleanCmd -> do
		(_, args) <- parseCleanArgs args []
		no_extra_flags args
		pkg_descr <- getBuildParams currentDir pkg_descr
		localbuildinfo <- getPersistBuildConfig
		let buildPref = buildDir localbuildinfo
		try $ removeFileRecursive buildPref
		try $ removeFile localBuildInfoFile
		return ()

	    CopyCmd mprefix -> do
	        (mprefix, _, args) <- parseCopyArgs mprefix args []
		no_extra_flags args
		pkg_descr <- getBuildParams currentDir pkg_descr
		localbuildinfo <- getPersistBuildConfig
		install pkg_descr localbuildinfo mprefix False

	    InstallCmd mprefix uInst -> do
		((mprefix,uInst), _, args) <- parseInstallArgs (mprefix,uInst) args []
		no_extra_flags args
		pkg_descr <- getBuildParams currentDir pkg_descr
		localbuildinfo <- getPersistBuildConfig
		install pkg_descr localbuildinfo mprefix uInst
		when (isNothing mprefix && hasLibs pkg_descr)
			 (register pkg_descr localbuildinfo uInst)

	    SDistCmd -> do
		(_, args) <- parseSDistArgs args []
		no_extra_flags args
		pkg_descr <- getBuildParams currentDir pkg_descr
		sdist srcPref distPref pkg_descr

	    RegisterCmd uInst -> do
		(uInst, _, args) <- parseRegisterArgs uInst args []
		no_extra_flags args
		pkg_descr <- getBuildParams currentDir pkg_descr
		localbuildinfo <- getPersistBuildConfig
		when (hasLibs pkg_descr) (register pkg_descr localbuildinfo uInst)

	    UnregisterCmd -> do
		(_, args) <- parseUnregisterArgs args []
		no_extra_flags args
		pkg_descr <- getBuildParams currentDir pkg_descr
		localbuildinfo <- getPersistBuildConfig
		unregister pkg_descr localbuildinfo

no_extra_flags :: [String] -> IO ()
no_extra_flags [] = return ()
no_extra_flags extra_flags  = 
  die ("Unrecognised flags: " ++ concat (intersperse "," (extra_flags)))

buildDirOpt :: OptDescr (LocalBuildInfo -> LocalBuildInfo)
buildDirOpt = Option "b" ["builddir"] (ReqArg setBuildDir "DIR")
		"directory to receive the built package [dist/build]"
  where setBuildDir dir lbi = lbi { buildDir = dir }

-- actions for Hugs

build :: PackageDescription -> LocalBuildInfo -> IO ()
build pkg lbi = when (buildPackage pkg) $
	withLib pkg $ \ libInfo -> do
	-- Pass 1: preprocess (except cpp) files in source directory
	preprocessPackage pkg
	-- Pass 2: copy or cpp files from source directory to build directory
	files <- listSourceFiles libInfo
	copyPackage pkg buildPref files
	-- Pass 3: compile foreign stubs in build directory
	let destFiles = [buildPref `joinFileName` f | f <- files]
	ffiFiles <- filterM testFFIModule destFiles
	mapM_ (compileFFI pkg lbi) ffiFiles
  where buildPref = buildDir lbi

install :: PackageDescription -> LocalBuildInfo ->
	Maybe FilePath -> Bool -> IO ()
install pkg lbi mprefix uInst =
	when (buildPackage pkg) $
	withLib pkg $ \ libInfo -> do
	pkgDir <- hugsPackageDir pkg lbi uInst
	maybeRemoveFileRecursive pkgDir
	moveSources buildPref pkgDir (biModules libInfo) installSuffixes
  where buildPref = buildDir lbi

sdist :: FilePath -> FilePath -> PackageDescription -> IO ()
sdist srcPref distPref pkg_descr =
	return ()	-- TODO

register :: PackageDescription -> LocalBuildInfo -> Bool -> IO ()
register pkg lbi uInst =
	return ()	-- for Hugs, install means register

unregister :: PackageDescription -> LocalBuildInfo -> IO ()
unregister pkg lbi = do
	pkgDir <- hugsPackageDir pkg lbi False
	maybeRemoveFileRecursive pkgDir

hugsPackageDir :: PackageDescription -> LocalBuildInfo -> Bool -> IO FilePath
hugsPackageDir pkg lbi uInst = do
	dir <- if uInst then getHomeDirectory
	    else return (prefix lbi `joinFileName` "lib")
	return $ dir `joinFileName` "hugs" `joinFileName` "packages"
		`joinFileName` pkgName (package pkg)

-- like removeFileRecursive, but don't complain if directory absent
maybeRemoveFileRecursive dir =
	whenM (doesDirectoryExist dir) $ removeFileRecursive dir

-- Reading local build information from Setup.buildinfo (if present)

buildInfoFile :: FilePath
buildInfoFile = "Setup.buildinfo"

getBuildParams :: FilePath -> PackageDescription -> IO PackageDescription
getBuildParams srcDir pkg_descr = do
	exists <- doesFileExist fpath
	if exists then do
		inp <- readFile fpath
		case parseBuildParameters pkg_descr inp of
		    Left err -> die (fpath ++ ": " ++ showError err)
		    Right pkg_descr' -> return pkg_descr'
	    else
		return pkg_descr
  where
	fpath = srcDir `joinFileName` buildInfoFile

parseBuildParameters :: PackageDescription -> String ->
	Either PError PackageDescription
parseBuildParameters pkg_descr inp = do
	fieldLines <- singleStanza (stripComments inp)
	foldM (parseBasicStanza basicStanzaFields) pkg_descr fieldLines

-- stolen from Distribution.InstalledPackageInfo
parseBasicStanza ((StanzaField name _ _ set):fields) pkg (lineNo, f, val)
  | name == f = set lineNo val pkg
  | otherwise = parseBasicStanza fields pkg (lineNo, f, val)
parseBasicStanza [] pkg (lineNo, f, val) = return pkg

-- Building a package for Hugs

-- Pass 1: apply preprocessors (except cpp) in-place

-- Preprocess a package.
preprocessPackage :: PackageDescription -> IO ()
preprocessPackage pkg_descr =
	withLib pkg_descr $ \ libInfo -> do
	let srcDir = hsSourceDir libInfo
	let srcStem mod = srcDir `joinFileName` dotToSep mod
	sequence_ [preprocessModule handlers (srcStem mod) |
			mod <- biModules libInfo]
  where handlers = ppHandlers incls
	incls = ccOptions pkg_descr

-- Preprocess a file.
preprocessModule :: [PPHandler] -> FilePath -> IO ()
preprocessModule handlers fileStem = chooseHandler handlers
  where
	chooseHandler [] = die (fileStem ++ ".*: not found")
	chooseHandler ((suffix, maybe_pp):handlers) = do
		let srcFile = fileStem ++ suffix
		exists <- doesFileExist srcFile
		if exists then case maybe_pp of
			    Nothing -> return ()
			    Just pp -> do
				pp srcFile (fileStem ++ ".hs")
				return ()
		    else chooseHandler handlers

-- Preprocessors

type PPHandler = (String, Maybe PreProcessor)

ppHandlers :: [String] -> [PPHandler]
ppHandlers incls = [
	(".hs",  Nothing),
	(".lhs", Nothing),
	-- TODO: ("_hsc_make.c", ???),
	(".hsc", Just (standardPP "hsc2hs" (defHugs : incls))),
	(".ly",  Just (ppHappy)),
	(".y",   Just (ppHappy))]

ppHappy :: PreProcessor
ppHappy = standardPP "happy" []

standardPP :: String -> [String] -> PreProcessor
standardPP eName args inFile outFile
    = rawSystemPath eName (args ++ ["-o" ++ outFile, inFile])

-- List of Haskell source files relative to source directory.
listSourceFiles :: BuildInfo -> IO [FilePath]
listSourceFiles libInfo = mapM sourceFile (biModules libInfo)
  where sourceFile mod = do
		let stem = dotToSep mod
		let hsFile = stem ++ ".hs"
		let lhsFile = stem ++ ".lhs"
		hsExists <- doesFileExist (srcDir `joinFileName` hsFile)
		lhsExists <- doesFileExist (srcDir `joinFileName` lhsFile)
		if hsExists then return hsFile
		    else if lhsExists then return lhsFile
		    else die ((srcDir `joinFileName` stem) ++
				".{hs,lhs}: not found")
	srcDir = hsSourceDir libInfo

-- Pass 2: copy or cpp files to build directory

-- Copy or cpp a package from the source directory to the build directory.
copyPackage :: PackageDescription -> FilePath -> [FilePath] -> IO ()
copyPackage pkg_descr destDir files =
	withLib pkg_descr $ \ libInfo -> do
	let useCpp = CPP `elem` extensions libInfo
	let srcDir = hsSourceDir libInfo
	let copy_or_cpp f = copyModule useCpp cpp
				(srcDir `joinFileName` f)
				(destDir `joinFileName` f)
	sequence_ [copy_or_cpp f | f <- files]
  where	cpp = ppCpp (ccOptions pkg_descr)

ppCpp :: [String] -> PreProcessor
ppCpp flags inFile outFile =
	rawSystemPath "cpp"
		(["-traditional", "-P", defHugs] ++ flags ++ [inFile, outFile])

-- Copy or cpp a file from the source directory to the build directory.
copyModule :: Bool -> PreProcessor -> FilePath -> FilePath -> IO ()
copyModule cppAll cpp srcFile destFile = do
	createIfNotExists True (dirname destFile)
	opts <- getOptions srcFile
	if cppAll || "-cpp" `elem` opts then do
		cpp srcFile destFile
		return ()
	    else
		copyFile srcFile destFile
  where dirname f = fst (splitFileName f)

-- Pass 3: compile FFI modules in build directory

compileFFI :: PackageDescription -> LocalBuildInfo -> FilePath -> IO ()
compileFFI pkg_descr lbi file = do
	withLib pkg_descr $ \ libInfo -> do
	options <- getOptions file
	let srcDir = hsSourceDir libInfo
	let pkg_incs = ["\"" ++ inc ++ "\"" | inc <- includes libInfo]
	let incs = uniq (sort (includeOpts options ++ pkg_incs))
	let pathFlag = "-P" ++ buildDir lbi ++ [searchPathSeparator]
	let hugsArgs = "-98" : pathFlag : map ("-i" ++) incs
	cfiles <- getCFiles file
	let cArgs =
		ccOptions pkg_descr ++
		map (joinFileName srcDir) cfiles ++
		["-L" ++ dir | dir <- extraLibDirs libInfo] ++
		ldOptions pkg_descr ++
		["-l" ++ lib | lib <- extraLibs libInfo] ++
		concat [["-framework", f] | f <- frameworks pkg_descr]
	rawSystem ffihugs (hugsArgs ++ file : cArgs)
	return ()
  where	ffihugs = compilerPath (compiler lbi)

includeOpts :: [String] -> [String]
includeOpts [] = []
includeOpts ("-#include" : arg : opts) = arg : includeOpts opts
includeOpts (_ : opts) = includeOpts opts

uniq :: Ord a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (dropWhile (== x) xs)

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

whenM :: IO Bool -> IO a -> IO ()
whenM cond act = do
	b <- cond
	when b $ do
		act
		return ()
