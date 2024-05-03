# Docs for this file: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkhaskellprojectinshellargs
# See `shellArgs` in `mkHaskellProject` in ./project.nix for more details.

{ repoRoot, inputs, pkgs, lib, system }:

# Each flake variant defined in your project.nix project will yield a separate
# shell. If no flake variants are defined, then cabalProject is the original 
# project.
cabalProject:

{
  name = "nix-shell";

  packages = [
    pkgs.scriv
    pkgs.jq
    inputs.cardano-node.packages.cardano-node
    inputs.cardano-node.packages.cardano-cli
  ];

  preCommit = {
    cabal-fmt.enable = true;
    cabal-fmt.extraOptions = "--no-tabular";
    fourmolu.enable = true;
    hlint.enable = true;
    shellcheck.enable = true;
    nixpkgs-fmt.enable = true;
  };

  tools = {
    haskell-language-server =
      let
        hlsProject = pkgs.haskell-nix.cabalProject' {
          name = "haskell-language-server";
          src = inputs.iogx.inputs.haskell-nix.inputs."hls-2.6";
          configureArgs = "--disable-benchmarks --disable-tests";
          compiler-nix-name = lib.mkDefault "ghc96";
          modules = [{
            packages.ghcide.patches = [
              # https://github.com/haskell/haskell-language-server/issues/4046#issuecomment-1926242056
              ./ghcide-workaround.diff
            ];
          }];
        };
      in
      hlsProject.hsPkgs.haskell-language-server.components.exes.haskell-language-server;
  };

  # prompt = null;

  # welcomeMessage = null;

  scripts = {
    deploy-local-testnet = {
      description = "Start and run an ephemeral local testnet";
      group = "general";
      exec = ''
        cd $(git rev-parse --show-toplevel)/testnet
        rm -rf example
        rm -rf logs
        scripts/babbage/mkfiles.sh
        cp example/utxo-keys/utxo1.vkey ../orchestrator.vkey
        cp example/utxo-keys/utxo1.skey ../orchestrator.skey
        cardano-cli address build --payment-verification-key-file ../orchestrator.vkey > ../orchestrator.addr
        example/run/all.sh
      '';
    };
  };

  env = {
    CARDANO_NODE_NETWORK_ID = "42";
  };

  shellHook = ''
    export CARDANO_NODE_SOCKET_PATH="$(git rev-parse --show-toplevel)/testnet/example/node-spo1/node.sock"
  '';
}
 
