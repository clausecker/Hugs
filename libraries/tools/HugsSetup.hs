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
import Distribution.Simple.Build
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
		let buildPref = buildDir localbuildinfo
		build buildPref pkg_descr localbuildinfo knownSuffixHandlers
		return ()

	    CleanCmd -> do
		(_, args) <- parseCleanArgs args []
		no_extra_flags args
		pkg_descr <- getBuildParams currentDir pkg_descr
		localbuildinfo <- getPersistBuildConfig
		let buildPref = buildDir localbuildinfo
		try $ removeFileRecursive buildPref
		try $ removeFile localBuildInfoFile
		removePreprocessedPackage pkg_descr currentDir (ppSuffixes knownSuffixHandlers)
		return ()

	    CopyCmd mprefix -> do
	        (mprefix, _, args) <- parseCopyArgs mprefix args []
		no_extra_flags args
		pkg_descr <- getBuildParams currentDir pkg_descr
		localbuildinfo <- getPersistBuildConfig
		install pkg_descr localbuildinfo False

	    InstallCmd uInst -> do
		(uInst, _, args) <- parseInstallArgs uInst args []
		no_extra_flags args
		pkg_descr <- getBuildParams currentDir pkg_descr
		localbuildinfo <- getPersistBuildConfig
		install pkg_descr localbuildinfo uInst
		when (hasLibs pkg_descr)
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

install :: PackageDescription -> LocalBuildInfo -> Bool -> IO ()
install pkg lbi uInst =
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
	fieldLines <- singleStanza (stripComments False inp)
	foldM (parseBasicStanza basicStanzaFields) pkg_descr fieldLines

-- stolen from Distribution.InstalledPackageInfo
parseBasicStanza ((StanzaField name _ _ set):fields) pkg (lineNo, f, val)
  | name == f = set lineNo val pkg
  | otherwise = parseBasicStanza fields pkg (lineNo, f, val)
parseBasicStanza [] pkg (lineNo, f, val) = return pkg

whenM :: IO Bool -> IO a -> IO ()
whenM cond act = do
	b <- cond
	when b $ do
		act
		return ()
