module ParsePkgConf( parsePackageConfig, parseOnePackageConfig ) where

-- ParSec version of fptools/ghc/utils/ghc-pkg/ParsePkgConfLite.y
-- (so we don't have to rely on Happy)

import Control.Monad(liftM)
import Data.Char
import Text.ParserCombinators.Parsec

import Package

parsePackageConfig :: String -> [PackageConfig]
parsePackageConfig = doParse (list package)

parseOnePackageConfig :: String -> PackageConfig
parseOnePackageConfig = doParse package

doParse :: Parser a -> String -> a
doParse p s = case parse p "" s of
    Left err -> error (show err)
    Right a -> a

package :: Parser PackageConfig
package = do
    spaces
    symbol "Package"
    symbol "{"
    fs <- sepBy field (symbol ",")
    symbol "}"
    return (foldl (flip ($)) defaultPackageConfig fs)

field :: Parser (PackageConfig -> PackageConfig)
field = do
    fieldName <- identifier
    symbol "="
    case fieldName of
	"name"		   -> liftM set_name stringLiteral
	"auto"		   -> liftM set_auto bool
	"import_dirs"      -> liftM set_import_dirs (list stringLiteral)
	"source_dirs"      -> liftM set_source_dirs (list stringLiteral)
	"library_dirs"     -> liftM set_library_dirs (list stringLiteral)
	"hs_libraries"     -> liftM set_hs_libraries (list stringLiteral)
	"extra_libraries"  -> liftM set_extra_libraries (list stringLiteral)
	"include_dirs"     -> liftM set_include_dirs (list stringLiteral)
	"c_includes"       -> liftM set_c_includes (list stringLiteral)
	"package_deps"     -> liftM set_package_deps (list stringLiteral)
	"extra_ghc_opts"   -> liftM set_extra_ghc_opts (list stringLiteral)
	"extra_cc_opts"    -> liftM set_extra_cc_opts (list stringLiteral)
	"extra_ld_opts"    -> liftM set_extra_ld_opts (list stringLiteral)
	"framework_dirs"   -> liftM set_framework_dirs (list stringLiteral)
	"extra_frameworks" -> liftM set_extra_frameworks (list stringLiteral)
  where
    set_name s p		= p{name = s}
    set_auto b p		= p{auto = b}
    set_import_dirs ss p	= p{import_dirs = ss}
    set_source_dirs ss p	= p{source_dirs = ss}
    set_library_dirs ss p	= p{library_dirs = ss}
    set_hs_libraries ss p	= p{hs_libraries = ss}
    set_extra_libraries ss p	= p{extra_libraries = ss}
    set_include_dirs ss p	= p{include_dirs = ss}
    set_c_includes ss p		= p{c_includes = ss}
    set_package_deps ss p	= p{package_deps = ss}
    set_extra_ghc_opts ss p	= p{extra_ghc_opts = ss}
    set_extra_cc_opts ss p	= p{extra_cc_opts = ss}
    set_extra_ld_opts ss p	= p{extra_ld_opts = ss}
    set_framework_dirs ss p	= p{framework_dirs = ss}
    set_extra_frameworks ss p	= p{extra_frameworks = ss}

bool :: Parser Bool
bool = (symbol "True" >> return True) <|> (symbol "False" >> return False)

list :: Parser a -> Parser [a]
list p = between (symbol "[") (symbol "]") (sepBy p (symbol ","))

symbol :: String -> Parser String
symbol s = word $ string s

stringLiteral :: Parser String
stringLiteral = word $ between (char '"') (char '"') (many stringChar)
  where
    stringChar :: Parser Char
    stringChar = (char '\\' >> anyChar) <|> satisfy (/= '"')

identifier :: Parser String
identifier = word $ do
    c <- lower
    cs <- many (alphaNum <|> char '_')
    return (c:cs)

word :: Parser a -> Parser a
word p = do
    v <- p
    spaces
    return v
