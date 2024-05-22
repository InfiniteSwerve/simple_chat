# Simple chat
A simple two-terminal socket based chat program.

To run (using nix), 
```
nix develop -c $SHELL
dune build
```
And then `_build/default/bin/main.exe server` in one terminal.
and `_build/default/bin/main.exe client` in another.
