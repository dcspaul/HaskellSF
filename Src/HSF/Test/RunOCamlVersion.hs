{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Run OCaml version of compiler
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Test.RunOCamlVersion
	( compareWithOCaml
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
    compare hsf output with ocaml compiler output
------------------------------------------------------------------------------}

type Compile = Opts -> String -> IO (Either Error String)

compareWithOCaml :: Opts -> Compile -> String -> IO (Bool)
compareWithOCaml opts compile srcPath = do

	haskellResult <- compile (opts { format=CompactJSON } ) srcPath
	ocamlResult <- runCsf opts srcPath

	if (matchCsf haskellResult ocamlResult)
		then do
			let status = case ocamlResult of
				(Left e) -> "\n" ++ (errorString e)
				(Right _) -> ""
			if (verbosity opts >= Verbose)
				then putStrLn ( ">> match ok: " ++ (takeBaseName srcPath) ++ status )
				else return ()
			return True
		else do
			putStrLn ( "** match failed: " ++ indentMsg ((takeBaseName srcPath) ++ "\n"	
				++ "Haskell: " ++ (outputOrError haskellResult) ++ "\n"
				++ "OCaml:   " ++ (outputOrError ocamlResult) ))
			return False

{------------------------------------------------------------------------------
    compile source using the ocaml version of the SF compiler
------------------------------------------------------------------------------}

-- run the script runSfParser.sh
-- (assumed to be in same directory as the hsf binary)
-- return the output or an error code

runCsf :: Opts -> String -> IO (Either O_Error String)
runCsf opts srcPath = do
	let dstPath = jsonPath srcPath opts ("-s")
	execPath <- getExecutablePath
	parserPath <- getCsfPath opts
	let scriptPath = (takeDirectory execPath) </> "runSF.sh"
	ph <- runProcess scriptPath [ "ocaml", srcPath, dstPath, parserPath  ]
		Nothing Nothing Nothing Nothing Nothing
	exitCode <- waitForProcess ph
	case (exitCode) of
		ExitSuccess -> do
			result <- readFile dstPath
			return (stringToErrorOrResult result)
		ExitFailure code ->
			return (Left ( O_ESYSFAIL ( scriptPath ++
			 	" " ++ srcPath ++ " " ++ dstPath ++ " " ++ parserPath )))

{------------------------------------------------------------------------------
    convert sfparser error messages to error codes
------------------------------------------------------------------------------}

data O_Error

	= O_ESYSFAIL String
	| O_EPARSEFAIL String
	| O_ERR1 String
	| O_ERR2 String
	| O_ERR3 String
	| O_ERR4 String
	| O_ERR5 String
	| O_ERR6 String
	| O_ERR7 String

stringToErrorOrResult :: String -> (Either O_Error String)
stringToErrorOrResult s
		| isError s "^Fatal error:.*\\[err1\\]" = Left (O_ERR1 s)
		| isError s "^Fatal error:.*\\[err2\\]" = Left (O_ERR2 s)
		| isError s "^Fatal error:.*\\[err3\\]" = Left (O_ERR3 s)
		| isError s "^Fatal error:.*\\[err4\\]" = Left (O_ERR4 s)
		| isError s "^Fatal error:.*\\[err5\\]" = Left (O_ERR5 s)
		| isError s "^Fatal error:.*\\[err6\\]" = Left (O_ERR6 s)
		| isError s "^Fatal error:.*\\[err7\\]" = Left (O_ERR7 s)
		| isError s "^Fatal error: exception Failure" = Left (O_EPARSEFAIL s)
		| otherwise = Right (rstrip s)
	where isError s r =
		case (matchRegex (mkRegex r) s) of
			Just _ -> True
			Nothing -> False

-- match with hsf error codes

matchCsf :: (Either Error String) -> (Either O_Error String) -> Bool
matchCsf (Right _) (Right _) = True
matchCsf (Left (ESYSFAIL _)) (Left (O_ESYSFAIL _)) = True
matchCsf (Left (EPARSEFAIL _)) (Left (O_EPARSEFAIL _)) = True
matchCsf (Left (EPARENTNOTSTORE _)) (Left (O_ERR1 _)) = True
matchCsf (Left (ENOPARENT _)) (Left (O_ERR2 _)) = True
matchCsf (Left EREPLACEROOTSTORE) (Left (O_ERR3 _)) = True
matchCsf (Left (ENOPROTO _)) (Left (O_ERR4 _)) = True
matchCsf (Left (EPROTONOTSTORE _)) (Left (O_ERR4 _)) = True
matchCsf (Left (ENOLR _)) (Left (O_ERR5 _)) = True
matchCsf (Left (EASSIGN _)) (Left (O_ERR6 _)) = True
matchCsf (Left (EREFNOTOBJ _)) (Left (O_ERR6 _)) = True
matchCsf (Left ENOSPEC) (Left (O_ERR7 _)) = True
matchCsf (Left (ESPEC _)) (Left (O_ERR7 _)) = True
matchCsf _ _ = False

instance ErrorMessage O_Error where
	errorString (O_ESYSFAIL s) = s
	errorString (O_EPARSEFAIL s) = s
	errorString (O_ERR1 s) = s
	errorString (O_ERR2 s) = s
	errorString (O_ERR3 s) = s
	errorString (O_ERR4 s) = s
	errorString (O_ERR5 s) = s
	errorString (O_ERR6 s) = s
	errorString (O_ERR7 s) = s

{------------------------------------------------------------------------------
    get the path to the compiler
------------------------------------------------------------------------------}

-- try the command line arguments (-s PATHNAME) first
-- then try the environment (CSF)
-- otherwise return the default (csf)

getCsfPath :: Opts -> IO (String)
getCsfPath opts = do
	sfParserEnv <- lookupEnv "CSF"
	case (sfParserEnv) of
		Just s -> return s
		Nothing -> return "csf"
