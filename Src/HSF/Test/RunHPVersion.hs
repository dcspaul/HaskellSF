{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Run HP version of compiler
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Test.RunHPVersion
	( compareWithHP
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
    compare hsf output with HP compiler output
------------------------------------------------------------------------------}

type Compile = Opts -> String -> IO (Either Error String)

compareWithHP :: Opts -> Compile -> String -> IO (Bool)
compareWithHP opts compile srcPath = do

	haskellResult <- compile (opts { format=CompactJSON } ) srcPath
	hpResult <- runHP opts srcPath

	if (matchCsf haskellResult hpResult)
		then do
			let status = case hpResult of
				(Left e) -> "\n" ++ (errorString e)
				(Right _) -> ""
			if (verbosity opts >= Verbose)
				then putStrLn ( ">> match ok: " ++ (takeBaseName srcPath) ++ status )
				else return ()
			return True
		else do
			putStrLn ( "** match failed: " ++ indentMsg ((takeBaseName srcPath) ++ "\n"	
				++ "Haskell: " ++ (outputOrError haskellResult) ++ "\n"
				++ "HP:      " ++ (outputOrError hpResult) ))
			return False

{------------------------------------------------------------------------------
    compile source using the HP version of the SF compiler
------------------------------------------------------------------------------}

-- run the script runSfParser.sh
-- (assumed to be in same directory as the hsf binary)
-- return the output or an error code

runHP :: Opts -> String -> IO (Either H_Error String)
runHP opts srcPath = do
	let dstPath = jsonPath srcPath opts ("-s")
	execPath <- getExecutablePath
	parserPath <- getCsfPath opts
	let scriptPath = (takeDirectory execPath) </> "runSF.sh"
	ph <- runProcess scriptPath [ "hp", srcPath, dstPath, parserPath  ]
		Nothing Nothing Nothing Nothing Nothing
	exitCode <- waitForProcess ph
	case (exitCode) of
		ExitSuccess -> do
			result <- readFile dstPath
			return (stringToErrorOrResult result)
		ExitFailure code ->
			return (Left ( H_ESYSFAIL ( scriptPath ++
			 	" " ++ srcPath ++ " " ++ dstPath ++ " " ++ parserPath )))

{------------------------------------------------------------------------------
    convert sfparser error messages to error codes
------------------------------------------------------------------------------}

data H_Error

	= H_ESYSFAIL String
	| H_EPARSEFAIL String
	| H_ERR1 String
	| H_ERR2 String
	| H_ERR3 String
	| H_ERR4 String
	| H_ERR5 String
	| H_ERR7 String

stringToErrorOrResult :: String -> (Either H_Error String)
stringToErrorOrResult s
		| isError s ( "cannot be cast to org.smartfrog.sfcore.languages.sf." ++ 
						"sfcomponentdescription.SFComponentDescription" ) = Left (H_ERR4 s)
		| isError s ( "HERE sfConfig, Reference not found" ) = Left (H_ERR7 s)
		| otherwise = Right (rstrip s)
	where isError s r =
		case (matchRegex (mkRegex r) s) of
			Just _ -> True
			Nothing -> False

-- match with hsf error codes

matchCsf :: (Either Error String) -> (Either H_Error String) -> Bool
matchCsf (Right _) (Right _) = True
matchCsf (Left (ESYSFAIL _)) (Left (H_ESYSFAIL _)) = True
matchCsf (Left (EPARSEFAIL _)) (Left (H_EPARSEFAIL _)) = True
matchCsf (Left (EPARENTNOTSTORE _)) (Left (H_ERR1 _)) = True
matchCsf (Left (ENOPARENT _)) (Left (H_ERR2 _)) = True
matchCsf (Left EREPLACEROOTSTORE) (Left (H_ERR3 _)) = True
matchCsf (Left (ENOPROTO _)) (Left (H_ERR4 _)) = True
matchCsf (Left (EPROTONOTSTORE _)) (Left (H_ERR4 _)) = True
matchCsf (Left (ENOLR _)) (Left (H_ERR5 _)) = True
matchCsf (Left ENOSPEC) (Left (H_ERR7 _)) = True
matchCsf (Left (ESPEC _)) (Left (H_ERR7 _)) = True
matchCsf _ _ = False

instance ErrorMessage H_Error where
	errorString (H_ESYSFAIL s) = s
	errorString (H_EPARSEFAIL s) = s
	errorString (H_ERR1 s) = s
	errorString (H_ERR2 s) = s
	errorString (H_ERR3 s) = s
	errorString (H_ERR4 s) = s
	errorString (H_ERR5 s) = s
	errorString (H_ERR7 s) = s

{------------------------------------------------------------------------------
    get the path to the compiler
------------------------------------------------------------------------------}

-- try the command line arguments (-s PATHNAME) first
-- then try the environment (HPSF)
-- otherwise return the default (sfParse)

getCsfPath :: Opts -> IO (String)
getCsfPath opts = do
	sfParserEnv <- lookupEnv "HPSF"
	case (sfParserEnv) of
		Just s -> return s
		Nothing -> return "sfParse"
