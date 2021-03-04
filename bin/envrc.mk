.envrc: ./*.nix ./*.cabal
	nix-shell -A shell --arg isGhcjs false --pure --run 'export -p > .envrc'
.envrc-ghcjs: ./*.nix ./*.cabal
	nix-shell -A shell --arg isGhcjs true --pure --run 'export -p > .envrc-ghcjs'

default: .envrc
