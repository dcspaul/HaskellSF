{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Run sfParser (Scala version)
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.RunScalaVersion
	( runSfParser
	) where

import HSF.Options

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

runSfParser :: String -> String -> [OptionFlag] -> IO (String)
runSfParser sourcePath destPath opts = do
	execPath <- getExecutablePath
	parserPath <- getSfParserPath opts
	let scriptPath = (takeDirectory execPath) </> "runSfParser.sh"
	ph <- runProcess scriptPath [ sourcePath, destPath, parserPath  ]
		Nothing Nothing Nothing Nothing Nothing
	exitCode <- waitForProcess ph
	case (exitCode) of
		ExitSuccess -> readFile destPath
		ExitFailure code -> fail ("runSfParser failed: " ++ scriptPath ++
			 " " ++ sourcePath ++ " " ++ destPath ++ " " ++ parserPath )

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
