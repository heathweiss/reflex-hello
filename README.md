# reflex-hello

uses from hackage 
- reflex-0.8.0.0
- reflex-vty-0.1.4.1

Compiles with stack lts 14.17, which is what the majority of reflex dependencies are in.


===================================== getting hie server to work ======================================
$ stack hoogle
-does a big build

added to stack.yaml:
ghc-options:
  "$everything": -haddock

again ran $ stack hoogle
-did another huge build.
Sounds like it is building/putting all the stack snapshop items into a local db.
Maybe that will make hie work on all things, like 'go to definition' which only worked on local code before
Running lsp server in emacs still fails to connect. Next:


https://github.com/haskell/haskell-ide-engine#install-specific-ghc-version
$ stack ./install.hs hie-8.8.2
fails as does not seem to recognize ./install
failer msg: File does not exist or is not a regular file `./install.hs'

so tried: $ stack install hie-8.8.2
failed with: Error: While constructing the build plan, the following exceptions were encountered: Unknown package: hie


found problem:
hie-server will not run with lts 14.17.
Need to go up to 15.2, but then build fails. Will need to upgrade the project to 15.2
