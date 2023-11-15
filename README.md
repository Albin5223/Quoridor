# OCaml Project 2023 - Quoridor

## Description

Implementation of Quoridor game in OCaml\
PPO5-2023 / "Quoridor" Project

## Rules

See rules [here](doc/Quoridor.pdf)

## Building quoridor

To build the project, type:

```
$ dune build
```

For continuous build, use

```
$ dune build --watch
```

instead.

## Running quoridor

To play the game, type:

```
$ dune exec quoridor
```

## Testing quoridor

To test the project, type:

```
$ dune runtest
```

This can be combined with continuous build & test, using

```
$ dune runtest --watch
```

# Documentation

The internal project documentation can be compiled to HTML through

```
$ dune build @doc-private
```


## Main contributors
| Last Name | First Name | Student ID |
| ---- | ------- | ---------- |
| JOFFRIN | Evan | 22102052 |
| EL JAMAI | Ali | 22104439 |
| ABDOROHMANG | Mathaui Yves | 22100336 |