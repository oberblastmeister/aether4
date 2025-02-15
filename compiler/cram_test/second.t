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
  $ c0 apdofiuaspdofiu
  C0 compiler
  
    c0 SUBCOMMAND
  
  === subcommands ===
  
    compile                    . Compile a c0 file
    version                    . print version information
    help                       . explain a given subcommand (perhaps recursively)
  
  unknown subcommand apdofiuaspdofiu
  [1]

After creating the fixture, we want to verify that ``wc`` gives us the right
result:
  $ c0 foo
  C0 compiler
  
    c0 SUBCOMMAND
  
  === subcommands ===
  
    compile                    . Compile a c0 file
    version                    . print version information
    help                       . explain a given subcommand (perhaps recursively)
  
  unknown subcommand foo
  [1]
Just cat another
  $ cat another
  first
  second
Final
  $ cat foo
  foo
  bar
  baz
