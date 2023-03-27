### An implementaion of Make

This is a homework for functional programming course.

License: LGPL for implementation code + WTFPL for test examles in miniLanguage

Author: Ivan Shurenkov

Done:
- Rule and variable parsing
  - Support multyline targets, prerequisites, commands
- Preaty print
- Inretpreting
  - Support variales
    - `$^` work as `$+`
    - `=` work as `:=`
  - Skiping circular prerequisites
  - Сhecking the existence and time of file modification
  - Error handing
    - `No rule`
    - `Nothing to be done`
    - `Error in rule`
    - `Is up to date`


TODO:
- Add ignoring comments and empty strings
- Expand the options of variables
  - Add mutliline declation
  - Add different variable assigment
    - `:=, ?=, +=, =`
  - Add recursively expanded for variables
  - Add variable assignment in command line
  - Add more automatic variables
    - `$<, $?, $+, $|, `
- Add special target `.PHONY`
- Add syntax of conditionals
- Add functions
  - `call`
  - `foreach`
- Add pattern matching
- Add defines