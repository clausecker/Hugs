{-
Prototype Cabal setup script for Hugs
	* configure runs ./configure if present
	* then reads build parameters from Setup.buildinfo, if present
	* user packages go in $HOME/hugs/packages/<pkg>
		(add {Home}/hugs/packages/* to your path)
	* probably doesn't work on Windows, and assumes gcc

Missing features compared with hugs-package:
	* hugs/exclude
	* doesn't use *_hsc_make.c if present
-}

module Main where

import Distribution.PackageDescription
import Distribution.ParseUtils
import Distribution.Simple
import Distribution.Simple.Utils

import Control.Monad	(foldM, when)
import System.Cmd	(rawSystem)
import System.Directory

defaultPackageDesc :: FilePath
defaultPackageDesc = "Setup.description"

-- Read local build information from Setup.buildinfo (if present)

buildInfoFile :: FilePath
buildInfoFile = "Setup.buildinfo"

main :: IO ()
main = defaultMainWithHooks hooks

hooks :: UserHooks
hooks = emptyUserHooks {
	preConf  = simplePreConf,
	preBuild = \ _ -> readHook,
	preClean = \ _ -> readHook,
	preCopy  = \ _ _ -> readHook,
	preInst  = \ _ _ -> readHook,
	preSDist = \ _ -> readHook,
	preReg   = \ _ _ -> readHook,
	preUnreg = \ _ -> readHook
   }
  where simplePreConf args (_, _, _, mb_prefix) = do
		configureExists <- doesFileExist "configure"
		when configureExists $ do
			rawSystem "./configure" $
				maybe id prefix_opt mb_prefix args
			return ()
		readHook

	prefix_opt pref opts = ("--prefix=" ++ pref) : opts

	readHook = do
		pkg_descr <- readPackageDescription defaultPackageDesc
		exists <- doesFileExist buildInfoFile
		if exists then do
			inp <- readFile buildInfoFile
			case parseBuildParameters pkg_descr inp of
			    Left err -> die (buildInfoFile ++ ": " ++ showError err)
			    Right pkg_descr' -> return (Just pkg_descr')
		    else
			return (Just pkg_descr)

parseBuildParameters :: PackageDescription -> String ->
	Either PError PackageDescription
parseBuildParameters pkg_descr inp = do
	fieldLines <- singleStanza inp
	foldM (parseBasicStanza basicStanzaFields) pkg_descr fieldLines

-- stolen from Distribution.InstalledPackageInfo
parseBasicStanza ((StanzaField name _ _ set):fields) pkg (lineNo, f, val)
  | name == f = set lineNo val pkg
  | otherwise = parseBasicStanza fields pkg (lineNo, f, val)
parseBasicStanza [] pkg (lineNo, f, val) = return pkg
