{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Command line options
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Options
	( Opts , Format(..)
	, parseOptions
	, format
	, isComparing, isChecking
	, outputPath, sfParserPath
	, jsonPath
	) where

import System.IO (hPutStrLn, stderr)
import System.Console.GetOpt(getOpt,OptDescr(..),ArgDescr(..),ArgOrder(..),usageInfo)
import System.FilePath.Posix (takeDirectory, addExtension, dropExtension, takeFileName, (</>))
import System.Exit (exitWith,ExitCode(..))

{------------------------------------------------------------------------------
    option handling
------------------------------------------------------------------------------}

data Format = JSON | CompactJSON | UnknownFormat String deriving(Show,Eq)

data Opts = Opts
	{ format :: Format
	, isComparing :: Bool
	, isChecking :: Bool
	, sfParserPath :: String
	, outputPath :: String
	}

data OptionFlag = Output String | Compare | Check | Format String
				| SfParser String deriving(Show,Eq)

defaults = Opts
	{ format = JSON
	, isComparing = False
	, isChecking = False
	, sfParserPath = ""
	, outputPath = ""
	}

options :: [OptDescr OptionFlag]
options =
	[ Option ['o'] ["output"]	(ReqArg Output "DIR")			"directory for json output"
	, Option ['c'] ["compare"]	(NoArg Compare)					"compare with output of Scala compiler"
	, Option ['q'] ["quickcheck"] (NoArg Check)					"quickcheck"
	, Option ['f'] ["format"] 	(ReqArg Format "json|compact")	"output format"
	, Option ['s'] ["sfparser"]	(ReqArg SfParser "FILE")		"location of sfparser"
	]

parseOptions :: [String] -> IO (Opts, [String])
parseOptions argv = case getOpt RequireOrder options argv of
	(opts,fs,[]) -> do
		let exopts = extractOptions opts
		case (format exopts) of
			UnknownFormat f -> do
				hPutStrLn stderr ("unknown format: " ++ f)
				exitWith (ExitFailure 1)
			_ -> return (exopts,fs)
	(_,_,errs) -> do
		hPutStrLn stderr (concat errs ++ usageInfo usage options)
		exitWith (ExitFailure 1)
	where usage = "Usage: options file .."

extractOptions :: [OptionFlag] -> Opts
extractOptions [] = defaults
extractOptions ((Format "json"):rest) = (extractOptions rest) { format = JSON }
extractOptions ((Format "compact"):rest) = (extractOptions rest) { format = CompactJSON }
extractOptions ((Format f):rest) = (extractOptions rest) { format = (UnknownFormat f) }
extractOptions ((Output p):rest) = (extractOptions rest) { outputPath = p }
extractOptions ((SfParser p):rest) = (extractOptions rest) { sfParserPath = p }
extractOptions (Compare:rest) = (extractOptions rest) { isComparing = True }
extractOptions (Check:rest) = (extractOptions rest) { isChecking = True }
extractOptions (_:rest) = (extractOptions rest)

-- where to put the json output:
-- the default is the same directory as the source
-- if the output arg is absolute, it is used as the directory for the output
-- if it is relative, it is interpreted relative to the source
-- "-" is interpreted as stdout

jsonPath :: String -> Opts -> String -> String
jsonPath srcPath opts postfix =
	if (((outputPath opts) == "-") && (postfix == ""))
		then "-"
		else ((takeDirectory srcPath) </> (outputPath opts) </>
			(addExtension ((dropExtension (takeFileName srcPath)) ++ postfix) ".json"))
