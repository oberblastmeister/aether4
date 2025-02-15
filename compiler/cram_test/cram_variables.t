$ echo $DUNE_ROOT | grep -o '[^/]*$'
my-project

# Create a file relative to project root
$ cat > $DUNE_ROOT/input.ml <<EOF
> let greeting = "Hello, World!"
> let () = print_endline greeting
> EOF
