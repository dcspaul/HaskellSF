{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Run sfParser (Scala version)
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.RunScalaVersion
	( runSfParser
	, compareSfError
	) where

import HSF.Options
import HSF.Utils

import Data.String.Utils (rstrip)
import System.FilePath.Posix (takeDirectory, (</>))
import System.Environment (getExecutablePath)
import GHC.IO.Exception (ExitCode(..))
import System.Process (runProcess, waitForProcess)
import System.Environment (lookupEnv)

{------------------------------------------------------------------------------
    compile source using the scala version of the SF compiler
------------------------------------------------------------------------------}

-- run the script runSfParser.sh
-- (assumed to be in same directory as the hsf binary)
-- return the output

runSfParser :: [OptionFlag] -> String -> IO (Either Error String)
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
			return (Right (rstrip result))
		ExitFailure code ->
			return (Left ( ESFPARSEFAIL ( "runSfParser failed: " ++ scriptPath ++
			 	" " ++ srcPath ++ " " ++ dstPath ++ " " ++ parserPath )))

-- get the path to the sfParser compiler
-- try the command line arguments (-s PATHNAME) first
-- then try the environment (SFPARSER)
-- otherwise return the default (sfparser)

getSfParserPath :: [OptionFlag] -> IO (String)
getSfParserPath fs = do
	let arg = sfParserPath fs
	sfParserEnv <- lookupEnv "SFPARSER"
	case (arg) of
		[] -> case (sfParserEnv) of
			Just s -> return s
			Nothing -> return "sfparser"
		f -> return f
