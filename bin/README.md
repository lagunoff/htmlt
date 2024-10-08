# nix-bins: A Low-Effort, Portable Solution for Caching Nix-Shell Environments

## Overview

`nix-bins` is a lightweight, portable script designed to reduce the
startup delay of `nix-shell` by caching environment setups. It
generates command wrappers that allow for immediate execution,
bypassing the usual evaluation of the Nix expressions.

## Purpose

Nix provides reproducible environments with `nix-shell`, but entering
the development environment requires extra step, incures a painful
delay before running CLI tools and doesn't do well with other tools
like text editors or IDEs. `nix-bins` solves these issues by creating
executable wrappers for each command in the environment, enabling
near-instant access.

## Setup and Usage

1. **Copy the `nix-bins` Script**: Place `nix-bins` in your project's
   `bin` directory.
2. **Generate Wrappers**: Execute:

```bash
nix-shell --pure --run bin/nix-bins
```
This command creates wrappers in ${project}/bin/${command}.
```
tree bin
bin/
├── nix-bins
├── ghci
├── ghc
├── ...
```
3. **Update Wrappers**: Whenever you change your '*.nix' files repeat
   the command from step 2 to keep the wrappers in sync.
4. **Update .gitignore**: Most likely you wouldn't want to see a bunch
   of 'bin/${command}' wrappers in version control, exclude them from
   git with following '.gitignore' config:
```
bin/**
!bin/nix-bins
!bin/README.md
```

## Advantages

- **Instant Command Execution**: Commands such as `bin/ghci` or
  `bin/ghc` execute instantaneously
- **Smoother development experience**: Just use `bin/you_command`
  without entering a new shell or relying on tools like `direnv`
- **Easier Integration**: Dev tools like Emacs require a command path,
  but don't know how to enter 'nix-shell'
- **No Need to Escape Arguments**: For example, you can run
`bin/ghc -e "2 + 2"` instead of `nix-shell--run "ghc -e \"2 + 2\""`
