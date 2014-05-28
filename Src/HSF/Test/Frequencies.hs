{------------------------------------------------------------------------------
    HSF - SmartFrog Core Language Compiler: Frequencies for random tests
	Paul Anderson <dcspaul@ed.ac.uk>
------------------------------------------------------------------------------}

module HSF.Test.Frequencies
	( lhsFreq
	) where

-- for now, there is only one "configurable" frequency set from here
-- but we might want to add others later if we need to tweak the distribution

lhsFreq :: (Int,Int)
lhsFreq = (3,1)				-- ratio of identifiers vs references for the lhs of assignments
