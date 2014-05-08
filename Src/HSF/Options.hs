{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Command line options
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Options
	( OptionFlag
	, parseOptions
	, outputPath
	, jsonPath
	, sfParserPath
	, compareOptionPresent
	) where


import System.IO (hPutStrLn, stderr)
import System.Console.GetOpt(getOpt,OptDescr(..),ArgDescr(..),ArgOrder(..),usageInfo)
import System.FilePath.Posix (takeDirectory, addExtension, dropExtension, takeFileName, (</>))
import System.Exit (exitWith,ExitCode(..))

{------------------------------------------------------------------------------
    option handling
------------------------------------------------------------------------------}

data OptionFlag = Output String | Compare | SfParser String deriving(Show,Eq)

options :: [OptDescr OptionFlag]
options =
	[ Option ['o'] ["output"]	(ReqArg Output "DIR")		"directory for json output"
	, Option ['c'] ["compare"]	(NoArg Compare)				"compare with output of Scala compiler"
	, Option ['s'] ["sfparser"]	(ReqArg SfParser "FILE")	"location of sfparser"
	]
 
parseOptions :: [String] -> IO ([OptionFlag], [String])
parseOptions argv = case getOpt RequireOrder options argv of
	(args,fs,[]) -> do
		return (args,fs)
	(_,_,errs) -> do
		hPutStrLn stderr (concat errs ++ usageInfo usage options)
		exitWith (ExitFailure 1)
	where usage = "Usage: options file .."

outputPath :: [OptionFlag] -> String
outputPath [] = ""
outputPath ((Output d):_) = d
outputPath (_:rest) = outputPath rest

sfParserPath :: [OptionFlag] -> String
sfParserPath [] = ""
sfParserPath ((SfParser f):_) = f
sfParserPath (_:rest) = sfParserPath rest

compareOptionPresent :: [OptionFlag] -> Bool
compareOptionPresent opts = (Compare `elem` opts)

-- where to put the json output:
-- the default is the same directory as the source
-- if the output arg is absolute, it is used as the directory for the output
-- if it is relative, it is interpreted relative to the source
-- "-" is interpreted as stdout
--
-- if we are comparing the two compilers ...
-- the postfix -1 or -2 is added to the filename for the appropriate compiler
-- output to the stdout does not make sense

jsonPath :: String -> [OptionFlag] -> String -> String
jsonPath srcPath opts postfix =
	if (((outputPath opts) == "-") && (postfix == ""))
		then "-"
		else ((takeDirectory srcPath) </> (outputPath opts) </>
			(addExtension ((dropExtension (takeFileName srcPath)) ++ postfix) ".json"))
