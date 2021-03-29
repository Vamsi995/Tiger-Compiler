# Change Log
All notable changes to this project will be documented in this file.

## [1.0.5] - 27-03-21
 
### Added
- Added the pretty printer (src/tc.sml)
- Added the src/tc.mlb file for instantiating a parser
- Added more test cases in the folder Tests/

### Changed
- Modified Makefile
- Modified tiger.grm file
- Modified tiger.lex to support multiline comments
 
### Fixed
- Fixed some minor bugs in tiger/tiger.grm
- Adjusted tiger/tiger.lex



## [1.0.4] - 17-03-21
 
### Added
- Added tiger/printast.sml (Prints ast syntax of a given tiger program)

### Changed
- Modified the ast syntax
- Modified tiger/tiger.grm file

### Fixed
- Fixed some bugs in the tiger/tiger.grm file and tiger/tiger.lex file



## [1.0.3] - 17-03-21
 
### Added
- Added tiger/tiger.lex (Lexer for the tiger language)
- Added tiger/tiger.grm (Concrete syntax and parser for the tiger language)
- Added tiger/tiger.sml (Building the parser for the language)
- Added tiger/tiger.mlb

### Changed
- Modified the ast syntax
 
### Fixed




## [1.0.2] - 08-03-21
 
### Added
- Added tiger/ast.sml (Abstract syntax tree for the tiger language)
- Added target/mips.sml (Abstract syntax tree for mips)
 
### Changed
- Moved Makefile to the main directory.
 
### Fixed



## [1.0.1] - 02-03-21
 
### Added
- Added reverse-polish directory copied from [this repository](https://gitlab.com/piyush-kurur/compilers/-/tree/master/reverse-polish).
 
### Changed
- Changed ast.sml by adding additional operator in the datatype
- Changed expr.lex by adding the tokens regular expression
- Changed expr.grm by adding additional tokens and also defining additional token rules to the CFG
- Changed rp.lex by adding the operator regular expression
 
### Fixed
 
## [1.0.0] - 20-02-21
 
### Added
- Added Makefile
- Added src directory
- Added src/tc.sml file
   
### Changed
 
### Fixed
 
