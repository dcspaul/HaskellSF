## Haskell SmartFrog Compiler (hsf)

hsf is compiler which implements the formal semantics of the core [SmartFrog](http://smartfrog.org) configuration language. hsf is a Haskell implementation which is intended to be compatible with the [Scala implementation](https://github.com/herry13/smartfrog-lang/blob/master/README.md) sfparser. The output can be compared with the output of sfparser to validate the semantics and the implementations.

### Compiling SmartFrog files

A list of SmartFrog source files can be  compiled into corresponding JSON output:

	hsf file1.sf file2.sf ...
	
The output files can be generated in a different directory (relative to the source files):

	hsf -o ../output-dir file1.sf file2.sf ...
	
The output format can be set with "-f json" or "-f compact"
The output logging can be increased with "-v" (verbose) or "-d" (debug)

### Comparing output

The "-c COMPILER" option compiles each source file using the external COMPILER, and compares the result with the hsf output. Differences are displayed on the stdout. Valid compilers are "scala", "ocaml" and "hp". Eg.

	hsf -c scala file1.sf file2.sf ...

In this mode, any error messages are placed in the output file allowing them to be compared with the corresponding messages from the external compiler.

The script runSF.sh is used to run the external compilers and must be in the same directory as the hsf binary. If external compiler is not in the path, the locations can be specified with the environment variables SFPARSER, CSF, and HPSF.

### Quickcheck

The "-q scala" option generates random SF source files and compares the output of two compilers using quickcheck:

	hsf -q scala

### Building hsf

hsf requires some additional Haskell modules to build. These can be installed with cabal:

	cabal install Parsec (depending on the Haskell version)
	cabal install MissingH
	cabal install Safe

The Bin directory may contain binaries for different platforms & different versions of Haskell.

The default Makefile target builds on the current platform. The "remote" target can do a build on
a remote machine (and retrieve the result). Platform-specific Build directories are created for
the compile.

Paul Anderson
<dcspaul@ed.ac.uk>
