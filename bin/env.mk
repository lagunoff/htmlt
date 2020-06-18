%.ghc-env: ./*.nix ./*.cabal
	nix-shell -A shells.ghc --pure --run 'export -p > .ghc-env'

%.ghcjs-env: ./*.nix ./*.cabal
	nix-shell -A shells.ghcjs --pure --run 'export -p > .ghcjs-env'
