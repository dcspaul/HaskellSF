## Haskell SmartFrog Compiler (hsf)

hsf is compiler which implements the formal semantics of the core [SmartFrog](http://smartfrog.org) configuration language. hsf is a Haskell implementation which is intended to be compatible with the [Scala implementation](https://github.com/herry13/smartfrog-lang/blob/master/README.md) sfparser. The output can be compared with the output of sfparser to validate the semantics and the implementations.

The current version does not support ``placement''.

### Compiling SmartFrog files

A list of SmartFrog source files can be  compiled into corresponding JSON output:

	hsf file1.sf file2.sf ...
	
The output files can be generated in a different directory (relative to the source files):

	hsf -o ../output-dir file1.sf file2.sf ...

### Comparing output

The -c option compiles each source file using the Scala SF compiler sfparser, and compares the result with the hsf output. The output files are named *-1.json (for the hsf version) and *-2.json (for the sfparser version). Differences are displayed on the stdout.

	hsf -c file1.sf file2.sf ...

In this mode, any error messages are placed in the output file allowing them to be compared with the corresponding messages from sfParser. The compiler also generates slightly different error messages and JSON formatting which are compatible with sfParser. 

The script runSfParser.sh is used to run sfparser and must be in the same directory as the hsf binary. If sfparser is not in the path, the location can be specified with the SFPARSER environment variable, or with the -s option:

	hsf -s path-to-sfparser -c file1.sf file2.sf ...

### Building hsf

hsf requires some additional Haskell modules to build. These can be installed with cabal:

	cabal install Parsec (depending on the Haskell version)
	cabal install MissingH
	cabal install Safe

There are multiple Build directories for the different versions of Haskell (eg. 7.6 and 7.8).
The default Makefile build target currently uses version 7.6.

The files in the Build directories have the "platform" appended to the names to support builds
on multiple platforms. Eg. "hsf-Linux-x86_64.hs". The file "hsf" is linked to the binary for the
current platform.

The default target builds on the current platform. The "remote" target can do a build on a remote
machine (and retrieve the result). 

Paul Anderson
<dcspaul@ed.ac.uk>
