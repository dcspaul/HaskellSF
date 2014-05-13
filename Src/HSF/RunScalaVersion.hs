{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Run sfParser (Scala version)
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.RunScalaVersion
	( runSfParser
	) where

import HSF.Options
import HSF.Errors

import Data.String.Utils (rstrip)
import System.FilePath.Posix (takeDirectory, (</>))
import System.Environment (getExecutablePath)
import GHC.IO.Exception (ExitCode(..))
import System.Process (runProcess, waitForProcess)
import System.Environment (lookupEnv)
import Text.Regex (mkRegex, matchRegex)

{------------------------------------------------------------------------------
    compile source using the scala version of the SF compiler
------------------------------------------------------------------------------}

-- run the script runSfParser.sh
-- (assumed to be in same directory as the hsf binary)
-- return the output or an error code

runSfParser :: Opts -> String -> IO (Either Error String)
runSfParser opts srcPath = do
	let dstPath = jsonPath srcPath opts ("-s")
	execPath <- getExecutablePath
	parserPath <- getSfParserPath opts
	let scriptPath = (takeDirectory execPath) </> "runSfParser.sh"
	ph <- runProcess scriptPath [ srcPath, dstPath, parserPath  ]
		Nothing Nothing Nothing Nothing Nothing
	exitCode <- waitForProcess ph
	case (exitCode) of
		ExitSuccess -> do
			result <- readFile dstPath
			return (stringToErrorOrResult result)
		ExitFailure code ->
			return (Left ( ESYSFAIL ( scriptPath ++
			 	" " ++ srcPath ++ " " ++ dstPath ++ " " ++ parserPath )))

{------------------------------------------------------------------------------
    convert sfparser error messages to error codes
------------------------------------------------------------------------------}

stringToErrorOrResult :: String -> Either Error String
stringToErrorOrResult s
		| isError s "^\\[err4\\] invalid prototype reference" = Left (EPROTONOTSTORE s)
		| isError s "^\\[err5\\] cannot find link reference" = Left (ENOLR s)
		| isError s "^\\[err6\\] prefix of .* is not a component" = Left (EASSIGN s)
		| isError s "^\\[err7\\] sfConfig is not exist or a component" = Left ENOSPEC
		| isError s "^Exception in thread \"main\" java.lang.StackOverflowError" = Left (EPARSEFAIL s)
		| isError s "\\(Is a directory\\)$" = Left (EPARSEFAIL s)
		| isError s "^invalid statement" = Left (EPARSEFAIL s)
		| otherwise = Right (rstrip s)
	where isError s r =
		case (matchRegex (mkRegex r) s) of
			Just _ -> True
			Nothing -> False

{------------------------------------------------------------------------------
    get the path to the sfParser compiler
------------------------------------------------------------------------------}

-- try the command line arguments (-s PATHNAME) first
-- then try the environment (SFPARSER)
-- otherwise return the default (sfparser)

getSfParserPath :: Opts -> IO (String)
getSfParserPath opts = do
	let arg = sfParserPath opts
	sfParserEnv <- lookupEnv "SFPARSER"
	case (arg) of
		[] -> case (sfParserEnv) of
			Just s -> return s
			Nothing -> return "sfparser"
		f -> return f
