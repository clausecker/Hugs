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
import Distribution.PreProcess
import Distribution.PreProcess.Unlit
import Distribution.Setup
import Distribution.Simple.Utils
import Distribution.Version

import Control.Monad	(liftM, foldM, filterM, when)
import Data.Char	(isAlpha, isAlphaNum)
import Data.List	(isPrefixOf, isSuffixOf, sort, intersperse)
import Data.Maybe	(isNothing, fromMaybe, mapMaybe)
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
		(flags, optFns, args) <-
			parseConfigureArgs flags args [buildDirOpt]
		localbuildinfo <- configure pkg_descr flags args
		writePersistBuildConfig (foldr id localbuildinfo optFns)
		when (not (buildPackage (buildParams localbuildinfo)))
			exitFailure

	    BuildCmd -> do
		(_, args) <- parseBuildArgs args []
		no_extra_flags args
		localbuildinfo <- getPersistBuildConfig
		let buildPref = buildDir localbuildinfo
		build buildPref pkg_descr localbuildinfo

	    CleanCmd -> do
		(_, args) <- parseCleanArgs args []
		no_extra_flags args
		localbuildinfo <- getPersistBuildConfig
		let buildPref = buildDir localbuildinfo
		try $ removeFileRecursive buildPref
		try $ removeFile localBuildInfoFile
		return ()

	    CopyCmd mprefix -> do
	        (mprefix, _, args) <- parseCopyArgs mprefix args []
		no_extra_flags args
		localbuildinfo <- getPersistBuildConfig
		let buildPref = buildDir localbuildinfo
		install buildPref pkg_descr localbuildinfo mprefix False

	    InstallCmd mprefix uInst -> do
		((mprefix,uInst), _, args) <- parseInstallArgs (mprefix,uInst) args []
		no_extra_flags args
		localbuildinfo <- getPersistBuildConfig
		let buildPref = buildDir localbuildinfo
		install buildPref pkg_descr localbuildinfo mprefix uInst
		when (isNothing mprefix && hasLibs pkg_descr)
			 (register pkg_descr localbuildinfo uInst)

	    SDistCmd -> do
		(_, args) <- parseSDistArgs args []
		no_extra_flags args
		sdist srcPref distPref pkg_descr

	    RegisterCmd uInst -> do
		(uInst, _, args) <- parseRegisterArgs uInst args []
		no_extra_flags args
		localbuildinfo <- getPersistBuildConfig
		when (hasLibs pkg_descr) (register pkg_descr localbuildinfo uInst)

	    UnregisterCmd -> do
		(_, args) <- parseUnregisterArgs args []
		no_extra_flags args
		localbuildinfo <- getPersistBuildConfig
		unregister pkg_descr localbuildinfo

no_extra_flags :: [String] -> IO ()
no_extra_flags [] = return ()
no_extra_flags extra_flags  = 
  die ("Unrecognised flags: " ++ concat (intersperse "," (extra_flags)))

buildDirOpt :: OptDescr (LocalBuildInfo -> LocalBuildInfo)
buildDirOpt = Option "b" ["builddir"] (ReqArg setBuildDir "DIR")
		"directory to receive the built package [dist/build]"

-- actions for Hugs

configure :: PackageDescription -> ConfigFlags -> [String] -> IO LocalBuildInfo
configure pkg (mb_hc_flavor, mb_hc_path, mb_hc_pkg, mb_prefix) args = do
	whenM (doesFileExist "configure") $
		rawSystem "./configure" (maybe id prefix_opt mb_prefix args)
	params <- getBuildParams currentDir
	compPath <- maybe findFFIHugs return mb_hc_path
	let comp = Compiler {
			compilerFlavor = fromMaybe Hugs mb_hc_flavor,
			compilerVersion = Version [] [],
			compilerPath = compPath,
			compilerPkgTool = ""
		}
	return $ LocalBuildInfo {
			prefix = instPrefix,
			compiler = comp,
			packageDeps = [],
			executableDeps = [],
			buildDir = "dist" `joinFileName` "build",
			buildParams = params
		}
  where prefix_opt pref opts = ("--prefix=" ++ pref) : opts
	instPrefix = fromMaybe "/usr/local" mb_prefix
	findFFIHugs = do
		mb_path <- findExecutable "ffihugs"
		maybe (die "Cannot find ffihugs") return mb_path

build :: FilePath -> PackageDescription -> LocalBuildInfo -> IO ()
build buildPref pkg lbi = when (buildPackage params) $
	withLib pkg $ \ libInfo -> do
	-- Pass 1: preprocess files
	let srcDir = hsSourceDir libInfo
	files <- prepPackage libInfo params srcDir buildPref
	-- Pass 2: compile foreign stubs
	ffiFiles <- filterM testFFIModule files
	mapM_ (compileFFI libInfo lbi srcDir) ffiFiles
  where params = buildParams lbi

install :: FilePath -> PackageDescription -> LocalBuildInfo ->
	Maybe FilePath -> Bool -> IO ()
install buildPref pkg lbi mprefix uInst =
	when (buildPackage (buildParams lbi)) $
	withLib pkg $ \ libInfo -> do
	pkgDir <- hugsPackageDir pkg lbi uInst
	maybeRemoveFileRecursive pkgDir
	moveSources buildPref pkgDir (biModules libInfo) installSuffixes

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

-- Local build information

data LocalBuildInfo = LocalBuildInfo {
		-- fields of Distribution.Simple.LocalBuildInfo
		prefix		:: FilePath,
		compiler	:: Compiler,
		packageDeps	:: [PackageIdentifier],
		executableDeps	:: [(String,[PackageIdentifier])],
		-- extra fields for Hugs
		buildDir	:: FilePath,
		buildParams	:: BuildParameters
	}
	deriving (Show, Read)

setBuildDir :: String -> LocalBuildInfo -> LocalBuildInfo
setBuildDir dir lbi = lbi { buildDir = dir }

-- Possibly system-dependent build parameters

data BuildParameters = BuildParameters {
		buildPackage	:: Bool,
		ccOptions	:: [String],
		ldOptions	:: [String],
		frameworks	:: [String]
	}
	deriving (Show, Read)

setBuildPackage :: Bool -> BuildParameters -> BuildParameters
setBuildPackage build params = params { buildPackage = build }

setCcOptions :: [String] -> BuildParameters -> BuildParameters
setCcOptions opts params = params { ccOptions = opts }

setLdOptions :: [String] -> BuildParameters -> BuildParameters
setLdOptions opts params = params { ldOptions = opts }

setFrameworks :: [String] -> BuildParameters -> BuildParameters
setFrameworks fws params = params { frameworks = fws }

emptyBuildParameters :: BuildParameters
emptyBuildParameters = BuildParameters {
		buildPackage = True,
		ccOptions = [],
		ldOptions = [],
		frameworks = []
	}

-- Reading local build information from Setup.buildinfo (if present)

buildInfoFile :: FilePath
buildInfoFile = "Setup.buildinfo"

getBuildParams :: FilePath -> IO BuildParameters
getBuildParams srcDir = do
	exists <- doesFileExist fpath
	if exists then do
		inp <- readFile fpath
		case parseBuildParameters inp of
		    Left err -> die (fpath ++ ": " ++ showError err)
		    Right params -> return params
	    else
		return emptyBuildParameters
  where
	fpath = srcDir `joinFileName` buildInfoFile

parseBuildParameters :: String -> Either PError BuildParameters
parseBuildParameters inp = do
	fieldLines <- singleStanza (stripComments inp)
	foldM (parseBasicStanza fields) emptyBuildParameters fieldLines
  where fields = [
		plainField "build-package" buildPackage setBuildPackage,
		wordsField "cc-options" ccOptions setCcOptions,
		wordsField "ld-options" ldOptions setLdOptions,
		wordsField "frameworks" frameworks setFrameworks
	   ]
	plainField name = simpleField name (text . show) parseReadS
	wordsField name = simpleField name (fsep . map text)
				(liftM words (munch (const True)))

-- stolen from Distribution.InstalledPackageInfo
parseBasicStanza ((StanzaField name _ _ set):fields) pkg (lineNo, f, val)
  | name == f = set lineNo val pkg
  | otherwise = parseBasicStanza fields pkg (lineNo, f, val)
parseBasicStanza [] pkg (lineNo, f, val) = return pkg

-- Persistence of local build information

localBuildInfoFile :: FilePath
localBuildInfoFile = "setup-config"

writePersistBuildConfig :: LocalBuildInfo -> IO ()
writePersistBuildConfig lbi = writeFile localBuildInfoFile (shows lbi "\n")

getPersistBuildConfig :: IO LocalBuildInfo
getPersistBuildConfig = do
	inp <- readFile localBuildInfoFile
	readIO inp

-- Building a package for Hugs

-- Pass 1: preprocess files

-- Preprocess a package, returning names of output files.
prepPackage :: BuildInfo -> BuildParameters ->
		FilePath -> FilePath -> IO [FilePath]
prepPackage libInfo buildParams srcDir destDir =
	mapM preprocess (biModules libInfo)
  where preprocess mod = prepModule handlers useCpp cpp
				(stem srcDir mod) (stem destDir mod)
	handlers = ppHandlers incls
	useCpp = CPP `elem` extensions libInfo
	cpp = ppCpp incls
	incls = ccOptions buildParams
	stem dir mod = dir `joinFileName` dotToSep mod

-- Preprocess a file, returning name of output file.
prepModule :: [PPHandler] -> Bool -> PreProcessor ->
		FilePath -> FilePath -> IO FilePath
prepModule handlers cppAll cpp srcStem destStem = do
	createIfNotExists True (dirname destStem)
	chooseHandler handlers
  where
	dirname f = fst (splitFileName f)
	chooseHandler [] = die (srcStem ++ ".*: not found")
	chooseHandler ((suffix, maybe_pp):handlers) = do
		exists <- doesFileExist srcFile
		if exists then do
			opts <- getOptions srcFile
			let useCpp = cppAll || "-cpp" `elem` opts
			case maybe_pp of
			    Nothing -> do
				let destFile = destStem ++ suffix
				(if useCpp then cpp else ppIdentity)
					srcFile destFile
				return destFile
			    Just pp -> do
				let destFile = destStem ++ ".hs"
				(if useCpp then (pp >-> cpp) else pp)
					srcFile destFile
				return destFile
		    else chooseHandler handlers
	  where srcFile = srcStem ++ suffix

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

ppCpp :: [String] -> PreProcessor
ppCpp flags inFile outFile =
	rawSystemPath "cpp"
		(["-traditional", "-P", defHugs] ++ flags ++ [inFile, outFile])

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

compileFFI :: BuildInfo -> LocalBuildInfo ->
	FilePath -> FilePath -> IO ExitCode
compileFFI libInfo lbi srcDir file = do
	options <- getOptions file
	let incs = uniq (sort (includeOpts options ++ pkg_incs))
	let pathFlag = "-P" ++ buildDir lbi ++ [searchPathSeparator]
	let hugsArgs = "-98" : pathFlag : map ("-i" ++) incs
	cfiles <- getCFiles file
	let cArgs =
		ccOptions params ++
		map (joinFileName srcDir) cfiles ++
		["-L" ++ dir | dir <- extraLibDirs libInfo] ++
		ldOptions params ++
		["-l" ++ lib | lib <- extraLibs libInfo] ++
		concat [["-framework", f] | f <- frameworks params]
	rawSystem ffihugs (hugsArgs ++ file : cArgs)
  where	pkg_incs = ["\"" ++ inc ++ "\"" | inc <- includes libInfo]
	params = buildParams lbi
	ffihugs = compilerPath (compiler lbi)

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
