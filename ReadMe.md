## Haskell SmartFrog Compiler (hsf)

hsf is compiler which implements the formal semantics of the core [SmartFrog](http://smartfrog.org) configuration language. hsf is a Haskell implementation which is intended to be compatible with the [Scala implementation](https://github.com/herry13/smartfrog-lang/blob/master/README.md) sfParser. The output can be compared with the output of sfParser to validate the semantics and the implementations.

The current version does not support #include or ``placement''.

### Compiling SmartFrog files

A list of SmartForg source files can be compiled into corresponding JSON output:

	hsf file1.sf file2.sf ...
	
The output files can be generated in a different directory (relative to the source files):

	hsf -o ../output file1.sf file2.sf ...

### Comparing output

The -c option compiles each source file using the Scala compiler sfParser, and compares the result with the hsf output. The output files are named *-1.json (for the hsf version) and *-2.json (for the sfParser version). Differences are displayed on the stdout.

	hsf -c path-to-sfparser file1.sf file2.sf ...

In this mode, any error messages are placed in the output file allowing them to be compared with the corresponding messages from sfParser. The compiler also generates slightly different error messages and JSON formatting which are compatible with sfParser. 

### Building hsf

hsf requires some additional Haskell modules to build. These can be installed with cabal:

	cabal install MissingH
	cabal install Safe
