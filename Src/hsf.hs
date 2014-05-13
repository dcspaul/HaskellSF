{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

import System.Environment (getArgs)

import HSF.Compile
import HSF.Options
import HSF.QuickCheck

{------------------------------------------------------------------------------
    main program
------------------------------------------------------------------------------}

main = do

	(opts, files) <- getArgs >>= parseOptions
	do if (isChecking opts) then (check opts) else return ()
	mapM_ (processFile opts) files
	
processFile :: Opts -> String -> IO ()
processFile opts srcPath = do
	if (isComparing opts)
		then compileAndCompare opts srcPath
		else compileAndSave opts srcPath
