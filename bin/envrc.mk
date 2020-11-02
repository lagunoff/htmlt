%.envrc: ./*.nix ./*.cabal
	nix-shell -A shells.ghc --pure --run 'export -p > .envrc'

%.ghcjs.envrc: ./*.nix ./*.cabal
	nix-shell -A shells.ghcjs --pure --run 'export -p > .ghcjs.envrc'
