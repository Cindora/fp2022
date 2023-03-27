Tests

Test select target
  $ cd tests/target_select
  $ make --no-print-directory a > f1
  $ make --no-print-directory b >> f1
  $ demoInterpret a > f2
  $ demoInterpret b >> f2
  $ diff f1 f2

Test variables
  $ cd ../variable
  $ make --no-print-directory > f1
  $ make --no-print-directory ab >> f1
  $ make --no-print-directory ba >> f1
  $ demoInterpret > f2
  $ demoInterpret ab >> f2
  $ demoInterpret ba >> f2
  $ diff f1 f2

Test silent
  $ cd ../silent
  $ make --no-print-directory a > f1
  $ make --no-print-directory b >> f1
  $ demoInterpret a > f2
  $ demoInterpret b >> f2
  $ diff f1 f2


Test dependencies
  $ cd ../dependencies
  $ touch b_file.txt
  $ make --no-print-directory a > f1
  $ make --no-print-directory b >> f1
  $ make --no-print-directory a_file >> f1
  $ make --no-print-directory b_file >> f1
  $ demoInterpret a > f2
  $ demoInterpret b >> f2
  $ demoInterpret a_file >> f2
  $ demoInterpret b_file >> f2
  $ diff f1 f2

Test multilines
  $ cd ../multilines
  $ make --no-print-directory a > f1
  $ make --no-print-directory b >> f1
  $ make --no-print-directory some1 >> f1
  $ make --no-print-directory some2 >> f1
  $ demoInterpret a > f2
  $ demoInterpret b >> f2
  $ demoInterpret some1 >> f2
  $ demoInterpret some2 >> f2
  $ diff f1 f2

Test overriding
  $ cd ../overriding
  $ make --no-print-directory a > f1 2>/dev/null
  $ make --no-print-directory b >> f1 2>/dev/null
  $ make --no-print-directory x >> f1 2>/dev/null
  $ demoInterpret a > f2
  Makefile: warning: overriding recipe for target 'b'
  Makefile: warning: ignoring old recipe for target 'b'
  Makefile: warning: overriding recipe for target 'a'
  Makefile: warning: ignoring old recipe for target 'a'
  Makefile: warning: overriding recipe for target 'x'
  Makefile: warning: ignoring old recipe for target 'x'
  $ demoInterpret b >> f2
  Makefile: warning: overriding recipe for target 'b'
  Makefile: warning: ignoring old recipe for target 'b'
  Makefile: warning: overriding recipe for target 'a'
  Makefile: warning: ignoring old recipe for target 'a'
  Makefile: warning: overriding recipe for target 'x'
  Makefile: warning: ignoring old recipe for target 'x'
  $ demoInterpret x >> f2
  Makefile: warning: overriding recipe for target 'b'
  Makefile: warning: ignoring old recipe for target 'b'
  Makefile: warning: overriding recipe for target 'a'
  Makefile: warning: ignoring old recipe for target 'a'
  Makefile: warning: overriding recipe for target 'x'
  Makefile: warning: ignoring old recipe for target 'x'
  $ diff f1 f2

Test circular
  $ cd ../circular
  $ make --no-print-directory a > f1 2>/dev/null
  $ make --no-print-directory b >> f1 2>/dev/null
  $ demoInterpret a > f2
  make: Circular b <- a dependency dropped.
  $ demoInterpret b >> f2
  make: Circular a <- b dependency dropped.
  $ diff f1 f2

Test errors
  $ cd ../errors
  $ make --no-print-directory no > f1 2>/dev/null
  [2]
  $ demoInterpret no > f2
  make: *** No rule to make target `no`. Stop.
  $ make --no-print-directory no_rule >> f1 2>/dev/null
  [2]
  $ demoInterpret no_rule >> f2
  No rule to make target `no`, needed by `no_rule`.
  $ make --no-print-directory rule_error >> f1 2>/dev/null
  [2]
  $ demoInterpret rule_error >> f2
  cat: fds: No such file or directory
  make: *** [Makefile: rule_error] Error 1
  $ rm -f upToDate
  $ make --no-print-directory upToDate >> f1 2>/dev/null
  $ rm -f upToDate
  $ demoInterpret upToDate >> f2
  $ demoInterpret upToDate
  make: `upToDate` is up to date.
  $ demoInterpret is_up_to_date
  make: Nothing to be done for `is_up_to_date`.

  $ rm -f upToDate
  $ make --no-print-directory is_up_to_date >> f1 2>/dev/null
  $ rm -f upToDate
  $ demoInterpret is_up_to_date >> f2
  $ demoInterpret is_up_to_date
  make: Nothing to be done for `is_up_to_date`.
  $ demoInterpret nothing_to_do
  make: Nothing to be done for `nothing_to_do`.
  $ diff f1 f2

Test timestamp
  $ cd ../timestat
  $ touch t1; touch t2; touch t3
  $ demoInterpret
  touch t3s
  touch all
  $ demoInterpret t3s
  make: `t3s` is up to date.
  $ 
  $ touch t3
  $ demoInterpret t3s
  touch t3s
  $ 
  $ touch t3
  $ demoInterpret
  touch t3s
  touch all
  $ 
  $ rm t3s
  $ demoInterpret
  touch t3s
  touch all
  $ 
  $ rm all
  $ demoInterpret
  touch all
  $ touch t1
  $ demoInterpret
  touch all

