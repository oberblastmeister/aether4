We create a test artifact called "foo"
  $ cat >foo <<EOF
  > foo
  > bar
  > baz
  > EOF

After creating the fixture, we want to verify that ``wc`` gives us the right
result:
  $ wc -l foo | awk '{ print $1 }'
  3
