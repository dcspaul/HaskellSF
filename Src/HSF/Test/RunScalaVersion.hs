{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Run sfParser (Scala version)
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Test.RunScalaVersion
	( compareWithScala
	) where

import HSF.Options
import HSF.Errors
import HSF.Utils

import Data.String.Utils (rstrip)
import System.FilePath.Posix (takeDirectory, (</>))
import System.Environment (getExecutablePath)
import GHC.IO.Exception (ExitCode(..))
import System.Process (runProcess, waitForProcess)
import System.Environment (lookupEnv)
import Text.Regex (mkRegex, matchRegex)
import System.FilePath.Posix (takeBaseName)

{------------------------------------------------------------------------------
    compare hsf output with scala compiler output
------------------------------------------------------------------------------}

type Compile = Opts -> String -> IO (Either Error String)

compareWithScala :: Opts -> Compile -> String -> IO (Bool)
compareWithScala opts compile srcPath = do

	haskellResult <- compile (opts { format=CompactJSON } ) srcPath
	scalaResult <- runSfParser opts srcPath

	if (matchSfParser haskellResult scalaResult)
		then do
			putStrLn ( ">> match ok: " ++ (takeBaseName srcPath) )
			return True
		else do
			putStrLn ( "** match failed: " ++ indentMsg ((takeBaseName srcPath) ++ "\n"	
				++ "Haskell: " ++ (outputOrError haskellResult) ++ "\n"
				++ "Scala:   " ++ (outputOrError scalaResult) ))
			return False

{------------------------------------------------------------------------------
    compile source using the scala version of the SF compiler
------------------------------------------------------------------------------}

-- run the script runSfParser.sh
-- (assumed to be in same directory as the hsf binary)
-- return the output or an error code

runSfParser :: Opts -> String -> IO (Either S_Error String)
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
			return (Left ( S_ESYSFAIL ( scriptPath ++
			 	" " ++ srcPath ++ " " ++ dstPath ++ " " ++ parserPath )))

{------------------------------------------------------------------------------
    convert sfparser error messages to error codes
------------------------------------------------------------------------------}

data S_Error

	= S_ESYSFAIL String
	| S_EPARSEFAIL String
	| S_ERR1 String
	| S_ERR2 String
	| S_ERR3 String
	| S_ERR4 String
	| S_ERR5 String
	| S_ERR6 String
	| S_ERR7 String

stringToErrorOrResult :: String -> (Either S_Error String)
stringToErrorOrResult s
		| isError s "^\\[err1\\]" = Left (S_ERR1 s)
		| isError s "^\\[err2\\]" = Left (S_ERR2 s)
		| isError s "^\\[err3\\]" = Left (S_ERR3 s)
		| isError s "^\\[err4\\]" = Left (S_ERR4 s)
		| isError s "^\\[err5\\]" = Left (S_ERR5 s)
		| isError s "^\\[err6\\]" = Left (S_ERR6 s)
		| isError s "^\\[err7\\]" = Left (S_ERR7 s)
		| isError s "^Exception in thread \"main\" java.lang.StackOverflowError" = Left (S_EPARSEFAIL s)
		| isError s "\\(Is a directory\\)$" = Left (S_EPARSEFAIL s)
		| isError s "^invalid statement" = Left (S_EPARSEFAIL s)
		| otherwise = Right (rstrip s)
	where isError s r =
		case (matchRegex (mkRegex r) s) of
			Just _ -> True
			Nothing -> False

-- match with hsf error codes

matchSfParser :: (Either Error String) -> (Either S_Error String) -> Bool
matchSfParser (Right _) (Right _) = True
matchSfParser (Left (ESYSFAIL _)) (Left (S_ESYSFAIL _)) = True
matchSfParser (Left (EPARSEFAIL _)) (Left (S_EPARSEFAIL _)) = True
matchSfParser (Left (EPARENTNOTSTORE _)) (Left (S_ERR1 _)) = True
matchSfParser (Left (ENOPARENT _)) (Left (S_ERR2 _)) = True
matchSfParser (Left EREPLACEROOTSTORE) (Left (S_ERR3 _)) = True
matchSfParser (Left (ENOPROTO _)) (Left (S_ERR4 _)) = True
matchSfParser (Left (EPROTONOTSTORE _)) (Left (S_ERR4 _)) = True
matchSfParser (Left (ENOLR _)) (Left (S_ERR5 _)) = True
matchSfParser (Left (EASSIGN _)) (Left (S_ERR6 _)) = True
matchSfParser (Left (EREFNOTOBJ _)) (Left (S_ERR6 _)) = True
matchSfParser (Left ENOSPEC) (Left (S_ERR7 _)) = True
matchSfParser (Left (ESPEC _)) (Left (S_ERR7 _)) = True
matchSfParser _ _ = False

instance ErrorMessage S_Error where
	errorString (S_ESYSFAIL s) = s
	errorString (S_EPARSEFAIL s) = s
	errorString (S_ERR1 s) = s
	errorString (S_ERR2 s) = s
	errorString (S_ERR3 s) = s
	errorString (S_ERR4 s) = s
	errorString (S_ERR5 s) = s
	errorString (S_ERR6 s) = s
	errorString (S_ERR7 s) = s

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
