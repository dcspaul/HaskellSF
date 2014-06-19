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
	otherResult <- runCsf opts srcPath

	let (s1,s2) = (s haskellResult, s otherResult)
		where s r = case r of
			(Left e) -> errorCode e
			(Right _) -> "ok"
			
	let status = " (" ++ s1 ++ "/" ++ s2 ++ ")"

	if (matchCsf haskellResult otherResult)
		then do
			if (verbosity opts >= Verbose)
				then putStrLn ( ">> match ok: " ++ (takeBaseName srcPath) ++ status )
				else return ()
			return True
		else do
			putStrLn ( "** match failed: " ++ indentMsg ((takeBaseName srcPath) 
				++ status ++ "\n"	
				++ "Haskell: " ++ (indentMsgBy (tabString 9) (outputOrError haskellResult)) ++ "\n"
				++ "OCaml:   " ++ (indentMsgBy (tabString 9) (outputOrError otherResult)) ))
			return False

{------------------------------------------------------------------------------
    compile source using the ocaml version of the SF compiler
------------------------------------------------------------------------------}

-- run the script runSfParser.sh
-- (assumed to be in same directory as the hsf binary)
-- return the output or an error code

runCsf :: Opts -> String -> IO (Either O_Error String)
runCsf opts srcPath = do
	let dstPath = jsonPath srcPath opts ("-ocaml")
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
	| O_ERR7 String

stringToErrorOrResult :: String -> (Either O_Error String)
stringToErrorOrResult s
		| isError s "\\[err1\\]" = Left (O_ERR1 s)
		| isError s "\\[err2\\]" = Left (O_ERR2 s)
		| isError s "\\[err3\\]" = Left (O_ERR3 s)
		| isError s "\\[err4\\]" = Left (O_ERR4 s)
		| isError s "\\[err5\\]" = Left (O_ERR5 s)
		| isError s "\\[err7\\]" = Left (O_ERR7 s)
		| isError s "exception Failure" = Left (O_EPARSEFAIL s)
		| otherwise = Right (rstrip s)
	where isError s r =
		case (matchRegex (mkRegex r) s) of
			Just _ -> True
			Nothing -> False

-- match with hsf error codes

matchCsf :: (Either Error String) -> (Either O_Error String) -> Bool
matchCsf (Right h) (Right o) = (h==o)
matchCsf (Left e) (Left o) = (errorCode e) == (errorCode o)
matchCsf _ _ = False

instance ErrorMessage O_Error where
	
	errorString (O_ESYSFAIL s) = s
	errorString (O_EPARSEFAIL s) = s
	errorString (O_ERR1 s) = s
	errorString (O_ERR2 s) = s
	errorString (O_ERR3 s) = s
	errorString (O_ERR4 s) = s
	errorString (O_ERR5 s) = s
	errorString (O_ERR7 s) = s

	errorCode (O_ESYSFAIL s) = "sys fail"
	errorCode (O_EPARSEFAIL s) = "parse fail"
	errorCode (O_ERR1 s) = "err1"
	errorCode (O_ERR2 s) = "err2"
	errorCode (O_ERR3 s) = "err3"
	errorCode (O_ERR4 s) = "err4"
	errorCode (O_ERR5 s) = "err5"
	errorCode (O_ERR7 s) = "err7"

{------------------------------------------------------------------------------
    get the path to the compiler
------------------------------------------------------------------------------}

-- try the environment (CSF)
-- otherwise return the default (csf)

getCsfPath :: Opts -> IO (String)
getCsfPath opts = do
	sfParserEnv <- lookupEnv "SF_OCAML_COMPILER"
	case (sfParserEnv) of
		Just s -> return s
		Nothing -> return "csf"
