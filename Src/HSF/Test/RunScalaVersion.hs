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
			let status = case scalaResult of
				(Left e) -> " (" ++ (errorCode e) ++ ")"
				(Right _) -> " (ok)"
			if (verbosity opts >= Verbose)
				then putStrLn ( ">> match ok: " ++ (takeBaseName srcPath) ++ status )
				else return ()
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
	let dstPath = jsonPath srcPath opts ("-scala")
	execPath <- getExecutablePath
	parserPath <- getSfParserPath opts
	let scriptPath = (takeDirectory execPath) </> "runSF.sh"
	ph <- runProcess scriptPath [ "scala", srcPath, dstPath, parserPath  ]
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
	| S_ERR7 String

stringToErrorOrResult :: String -> (Either S_Error String)
stringToErrorOrResult s
		| isError s "^\\[err1\\]" = Left (S_ERR1 s)
		| isError s "^\\[err2\\]" = Left (S_ERR2 s)
		| isError s "^\\[err3\\]" = Left (S_ERR3 s)
		| isError s "^\\[err4\\]" = Left (S_ERR4 s)
		| isError s "^\\[err5\\]" = Left (S_ERR5 s)
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
matchSfParser (Right h) (Right s) = h==s
matchSfParser (Left e) (Left s) = (errorCode e) == (errorCode s)
matchSfParser _ _ = False

instance ErrorMessage S_Error where

	errorString (S_ESYSFAIL s) = s
	errorString (S_EPARSEFAIL s) = s
	errorString (S_ERR1 s) = s
	errorString (S_ERR2 s) = s
	errorString (S_ERR3 s) = s
	errorString (S_ERR4 s) = s
	errorString (S_ERR5 s) = s
	errorString (S_ERR7 s) = s

	errorCode (S_ESYSFAIL s) = "sys fail"
	errorCode (S_EPARSEFAIL s) = "parse fail"
	errorCode (S_ERR1 s) = "err1"
	errorCode (S_ERR2 s) = "err2"
	errorCode (S_ERR3 s) = "err3"
	errorCode (S_ERR4 s) = "err4"
	errorCode (S_ERR5 s) = "err5"
	errorCode (S_ERR7 s) = "err7"

{------------------------------------------------------------------------------
    get the path to the sfParser compiler
------------------------------------------------------------------------------}

-- try the environment (SFPARSER)
-- otherwise return the default (sfparser)

getSfParserPath :: Opts -> IO (String)
getSfParserPath opts = do
	sfParserEnv <- lookupEnv "SF_SCALA_COMPILER"
	case (sfParserEnv) of
		Just s -> return s
		Nothing -> return "sfParser"
