We create a test artifact called "foo"
  $ cat >foo <<EOF
  > foo
  > bar
  > baz
  > EOF
  $ cat >another <<EOF
  > first
  > second
  > EOF
When the file is not found:
  $ c0c apdofiuaspdofiu
  c0c: FILE argument: no 'apdofiuaspdofiu' file
  Usage: c0c [--verbose] [OPTION]… FILE
  Try 'c0c --help' for more information.
  [124]

After creating the fixture, we want to verify that ``wc`` gives us the right
result:
  $ c0c foo
  $TESTCASE_ROOT
  /home/brian/projects/aether4/_build/default/compiler/bin/c0c.exe
  ((verbose false) (path foo))
Just cat another
  $ cat another
  first
  second
Final
  $ cat foo
  foo
  bar
  baz
