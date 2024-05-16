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
    repoRoot.nix.jsonld-nix.default.nodeDependencies
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
        cardano-cli stake-address key-gen \
          --verification-key-file orchestrator-stake.vkey \
          --signing-key-file orchestrator-stake.skey
        cardano-cli address build \
          --payment-verification-key-file orchestrator.vkey \
          --out-file orchestrator.addr
      '';
    };

    get-orchestrator-utxo = {
      description = "Query the utxo for the orchestrator's address";
      group = "general";
      exec = ''
        set -e
        set -u
        set -o pipefail

        cd $(git rev-parse --show-toplevel)
        cardano-cli conway query utxo --address $(cat orchestrator.addr) "$@"
      '';
    };

    get-orchestrator-ada-only = {
      description = "Query the utxo for an ADA-only output at the orchestrator's address";
      group = "general";
      exec = ''
        set -e
        set -u
        set -o pipefail

        cd $(git rev-parse --show-toplevel)
        cardano-cli conway query utxo --address $(cat orchestrator.addr) --output-json \
          | jq 'to_entries | .[] | select(.value.value | keys | length == 1)'
      '';
    };

    get-output-by-policy-id = {
      description = "Query the UTxO for a specific minting policy ID";
      group = "general";
      exec = ''
        set -e
        set -u
        set -o pipefail

        cd $(git rev-parse --show-toplevel)
        cardano-cli conway query utxo --whole-utxo --output-json \
          | jq 'to_entries | .[] | select(.value.value["'"$1"'"])'
      '';
    };

    fetch-cold-nft-utxo = {
      description = "Update the local copy of the cold NFT UTxO";
      group = "general";
      exec = ''
        set -e
        set -u
        set -o pipefail

        cd $(git rev-parse --show-toplevel)
        cardano-cli conway query utxo --address $(cat cold-nft/script.addr) --output-json \
          | jq 'to_entries | .[0].value' \
          > cold-nft.utxo
      '';
    };

    fetch-hot-nft-utxo = {
      description = "Update the local copy of the hot NFT UTxO";
      group = "general";
      exec = ''
        set -e
        set -u
        set -o pipefail

        cd $(git rev-parse --show-toplevel)
        cardano-cli conway query utxo --address $(cat hot-nft/script.addr) --output-json \
          | jq 'to_entries | .[0].value' \
          > hot-nft.utxo
      '';
    };

    mint-tokens = {
      description = "Mint two NFTs to use with the cold and hot credentials.";
      group = "general";
      exec = ''
        set -e
        set -u
        set -o pipefail
        echo "Minting cold NFT"
        cardano-cli conway transaction build \
          --tx-in $(get-orchestrator-ada-only | jq -r '.key') \
          --tx-out "$(cat orchestrator.addr)+5000000 + 1 $COLD_POLICY_ID" \
          --mint "1 $COLD_POLICY_ID" \
          --mint-script-file coldMint.native \
          --change-address $(cat orchestrator.addr) \
          --out-file coldMint.body
        cardano-cli conway transaction sign \
          --signing-key-file orchestrator.skey \
          --signing-key-file coldMint.skey \
          --tx-body-file coldMint.body \
          --out-file coldMint.tx
        echo "Sending cold NFT to orchestrator..."
        cardano-cli conway transaction submit --tx-file coldMint.tx
        sleep 2
        echo "Minting hot NFT"
        cardano-cli conway transaction build \
          --tx-in $(get-orchestrator-ada-only | jq -r '.key') \
          --tx-out "$(cat orchestrator.addr)+5000000 + 1 $HOT_POLICY_ID" \
          --mint "1 $HOT_POLICY_ID" \
          --mint-script-file hotMint.native \
          --change-address $(cat orchestrator.addr) \
          --out-file hotMint.body
        cardano-cli conway transaction sign \
          --signing-key-file orchestrator.skey \
          --signing-key-file hotMint.skey \
          --tx-body-file hotMint.body \
          --out-file hotMint.tx
        echo "Sending hot NFT to orchestrator..."
        cardano-cli conway transaction submit --tx-file hotMint.tx
      '';
    };

    create-dummy-gov-action = {
      description = "Create a dummy governance action for voting on";
      group = "general";
      exec = ''
        set -e
        set -u
        set -o pipefail

        cd $(git rev-parse --show-toplevel)
        cardano-cli conway governance action create-info \
          --testnet \
          --governance-action-deposit 1000000000 \
          --anchor-url https://example.com \
          --anchor-data-hash 0000000000000000000000000000000000000000000000000000000000000000 \
          --deposit-return-stake-verification-key-file orchestrator-stake.vkey \
          --out-file dummy.action
        cardano-cli conway transaction build \
          --tx-in $(get-orchestrator-ada-only | jq -r '.key') \
          --change-address $(cat orchestrator.addr) \
          --proposal-file dummy.action \
          --out-file dummy.body
        cardano-cli conway transaction sign \
          --signing-key-file orchestrator.skey \
          --tx-body-file dummy.body \
          --out-file dummy.tx
        cardano-cli conway transaction submit --tx-file dummy.tx
        echo "Governance Action Tx ID: $(cardano-cli transaction txid --tx-body-file dummy.body)"
        echo "Governance Action Index: 0"
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
    COLD_POLICY_ID = "c8aa0de384ad34d844dc479085c3ed00deb1306afb850a2cde6281f4";
    HOT_POLICY_ID = "e2ab737f528cd043927496dd34e6629beb1e57ee8fe92c582cf76bd0";
  };

  shellHook = ''
    export CARDANO_NODE_SOCKET_PATH="$(git rev-parse --show-toplevel)/testnet/example/node-spo1/node.sock"
    cabal update
  '';
}
 
