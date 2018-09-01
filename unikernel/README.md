There are two ways of building the unikernel:

1: By first installing origami-sledgehammer as an opam package,

2: By building the dune library without installing anything.

## Method 1

After installing origami-sledgehammer:

```sh
mirage configure -t ukvm
make depends
make
```

Choose a different value for `-t` if you want to use a different backend than `ukvm`.

## Method 2

Run `dune build` at the project root.

```sh
dune exec -- mirage configure -t ukvm
dune exec -- make depends
dune exec -- make
```

Again, choose a different value for `-t` if you want to use something other than `ukvm`.
