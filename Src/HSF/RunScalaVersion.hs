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
import System.Cmd(rawSystem)

{------------------------------------------------------------------------------
    compile using the scala or haskell compiler
------------------------------------------------------------------------------}

runSfParser :: String -> String -> [OptionFlag] -> IO (String)
runSfParser sourcePath destPath opts = do
	execPath <- getExecutablePath
	sfParserPath <- getSfParserPath opts
	let scriptPath = (takeDirectory execPath) </> "runSfParser.sh"
 	exitCode <- rawSystem scriptPath [ sourcePath, destPath, sfParserPath  ]
	case (exitCode) of
		ExitSuccess -> readFile destPath
		ExitFailure code -> fail ("script failed: " ++ scriptPath ++
			 " " ++ sourcePath ++ " " ++ destPath ++ " " ++ sfParserPath )
