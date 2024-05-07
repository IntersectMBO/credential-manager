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
        set -e
        set -u
        set -o pipefail

        cd $(git rev-parse --show-toplevel)/testnet
        [ -d example ] || scripts/babbage/mkfiles.sh
        example/run/all.sh
      '';
    };

    purge-local-testnet = {
      description = "Cleanup the local testnet directory";
      group = "general";
      exec = ''
        set -e
        set -u
        set -o pipefail

        cd $(git rev-parse --show-toplevel)/testnet
        rm -rf example logs
      '';
    };

    setup-orchestrator = {
      description = "Configure the orchestrator's wallet";
      group = "general";
      exec = ''
        set -e
        set -u
        set -o pipefail

        cd $(git rev-parse --show-toplevel)
        cp testnet/example/utxo-keys/utxo1.vkey orchestrator.vkey
        cp testnet/example/utxo-keys/utxo1.skey orchestrator.skey
        cardano-cli address build \
          --payment-verification-key-file orchestrator.vkey \
          --out-file orchestrator.addr
      '';
    };

    mint-tokens = {
      description = "Mint two NFTs to use with the cold and hot credentials.";
      group = "general";
      exec = ''
        set -e
        set -u
        set -o pipefail

        cd $(git rev-parse --show-toplevel)
        UTXO_1_ADDR=$(
          cardano-cli address build \
            --payment-verification-key-file testnet/example/utxo-keys/utxo1.vkey
        )
        COLD_INPUT=$(
          cardano-cli query utxo --address $UTXO_1_ADDR --output-json \
            | jq -r 'keys[0]'
        )
        UTXO_2_ADDR=$(
          cardano-cli address build \
            --payment-verification-key-file testnet/example/utxo-keys/utxo2.vkey
        )
        HOT_INPUT=$(
          cardano-cli query utxo --address $UTXO_2_ADDR --output-json \
            | jq -r 'keys[0]'
        )
        COLD_INPUT_CBOR=$(echo $COLD_INPUT | sed 's/#/0/')
        HOT_INPUT_CBOR=$(echo $HOT_INPUT | sed 's/#/0/')
        cat mint.plutus.template | sed s/{0}/$COLD_INPUT_CBOR/ > coldMint.plutus
        cat mint.plutus.template | sed s/{0}/$HOT_INPUT_CBOR/ > hotMint.plutus
        cardano-cli transaction policyid --script-file coldMint.plutus > cold.pol
        cardano-cli transaction policyid --script-file hotMint.plutus > hot.pol
        cardano-cli conway transaction build \
          --tx-in $COLD_INPUT \
          --tx-in-collateral $COLD_INPUT \
          --tx-out "$(cat orchestrator.addr)+5000000 + 1 $(cat cold.pol)" \
          --mint "1 $(cat cold.pol)" \
          --mint-script-file coldMint.plutus \
          --mint-redeemer-value {} \
          --change-address $UTXO_1_ADDR \
          --out-file coldMint.body
        cardano-cli conway transaction build \
          --tx-in $HOT_INPUT \
          --tx-in-collateral $HOT_INPUT \
          --tx-out "$(cat orchestrator.addr)+5000000 + 1 $(cat hot.pol)" \
          --mint "1 $(cat hot.pol)" \
          --mint-script-file hotMint.plutus \
          --mint-redeemer-value {} \
          --change-address $UTXO_2_ADDR \
          --out-file hotMint.body
        cardano-cli conway transaction sign \
          --signing-key-file testnet/example/utxo-keys/utxo1.skey \
          --tx-body-file coldMint.body \
          --tx-file coldMint.tx
        cardano-cli conway transaction sign \
          --signing-key-file testnet/example/utxo-keys/utxo2.skey \
          --tx-body-file hotMint.body \
          --tx-file hotMint.tx
        cardano-cli conway transaction submit --tx-file coldMint.tx
        cardano-cli conway transaction submit --tx-file hotMint.tx
      '';
    };

    orchestrator-cli = {
      description = "Wrapper for orchestrator CLI executable";
      group = "general";
      exec = ''
        set -e
        set -u
        set -o pipefail

        cabal run orchestrator-cli -- "$@"
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
 
