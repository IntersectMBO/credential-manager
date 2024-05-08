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

        cd $(git rev-parse --show-toplevel)
        UTXO_2_ADDR=$(
          cardano-cli address build \
            --payment-verification-key-file testnet/example/utxo-keys/utxo2.vkey
        )
        COLD_INPUT=$(
          cardano-cli query utxo --address $UTXO_2_ADDR --output-json \
            | jq -r 'keys[0]'
        )
        UTXO_3_ADDR=$(
          cardano-cli address build \
            --payment-verification-key-file testnet/example/utxo-keys/utxo3.vkey
        )
        HOT_INPUT=$(
          cardano-cli query utxo --address $UTXO_3_ADDR --output-json \
            | jq -r 'keys[0]'
        )
        COLD_INPUT_CBOR=$(echo $COLD_INPUT | sed 's/#/0/')
        HOT_INPUT_CBOR=$(echo $HOT_INPUT | sed 's/#/0/')
        cat mint.plutus.template | sed s/{0}/$COLD_INPUT_CBOR/ > coldMint.plutus
        cat mint.plutus.template | sed s/{0}/$HOT_INPUT_CBOR/ > hotMint.plutus
        cardano-cli transaction policyid --script-file coldMint.plutus > cold.pol
        cardano-cli transaction policyid --script-file hotMint.plutus > hot.pol
        echo "Minting cold NFT, minting policy: $(cat cold.pol)"
        cardano-cli conway transaction build \
          --tx-in $COLD_INPUT \
          --tx-in-collateral $COLD_INPUT \
          --tx-out "$(cat orchestrator.addr)+5000000 + 1 $(cat cold.pol)" \
          --mint "1 $(cat cold.pol)" \
          --mint-script-file coldMint.plutus \
          --mint-redeemer-value {} \
          --change-address $UTXO_2_ADDR \
          --out-file coldMint.body
        echo "Minting hot NFT, minting policy: $(cat hot.pol)"
        cardano-cli conway transaction build \
          --tx-in $HOT_INPUT \
          --tx-in-collateral $HOT_INPUT \
          --tx-out "$(cat orchestrator.addr)+5000000 + 1 $(cat hot.pol)" \
          --mint "1 $(cat hot.pol)" \
          --mint-script-file hotMint.plutus \
          --mint-redeemer-value {} \
          --change-address $UTXO_3_ADDR \
          --out-file hotMint.body
        cardano-cli conway transaction sign \
          --signing-key-file testnet/example/utxo-keys/utxo2.skey \
          --tx-body-file coldMint.body \
          --out-file coldMint.tx
        cardano-cli conway transaction sign \
          --signing-key-file testnet/example/utxo-keys/utxo3.skey \
          --tx-body-file hotMint.body \
          --out-file hotMint.tx
        echo "Sending cold NFT to orchestrator..."
        cardano-cli conway transaction submit --tx-file coldMint.tx
        echo "Sending hot NFT to orchestrator..."
        cardano-cli conway transaction submit --tx-file hotMint.tx
      '';
    };

    setup-drep = {
      description = "Register a Stake Address and a DRep and delegate to the DRep";
      group = "general";
      exec = ''
        set -e
        set -u
        set -o pipefail

        cd $(git rev-parse --show-toplevel)
        UTXO_2_ADDR=$(
          cardano-cli address build \
            --payment-verification-key-file testnet/example/utxo-keys/utxo2.vkey
        )
        UTXO_2_INPUT=$(
          cardano-cli query utxo --address $UTXO_2_ADDR --output-json \
            | jq -r 'keys[0]'
        )
        cardano-cli address key-gen \
          --verification-key-file drep-payment.vkey \
          --signing-key-file drep-payment.skey
        cardano-cli stake-address key-gen \
          --verification-key-file drep-stake.vkey \
          --signing-key-file drep-stake.skey
        cardano-cli address build \
          --payment-verification-key-file drep-payment.vkey \
          --stake-verification-key-file drep-stake.vkey \
          --out-file drep-payment.addr
        cardano-cli stake-address build \
          --stake-verification-key-file drep-stake.vkey \
          --out-file drep-stake.addr
        cardano-cli conway governance drep key-gen \
          --verification-key-file drep.vkey \
          --signing-key-file drep.skey
        echo "Creating DRep wallet and funding..."
        cardano-cli conway transaction build \
          --tx-in $UTXO_2_INPUT \
          --tx-out "$(cat drep-payment.addr)+10000000000" \
          --change-address $UTXO_2_ADDR \
          --out-file fund-drep.body
        cardano-cli conway transaction sign \
          --signing-key-file testnet/example/utxo-keys/utxo2.skey \
          --tx-body-file fund-drep.body \
          --out-file fund-drep.tx
        cardano-cli conway transaction submit --tx-file fund-drep.tx
        sleep 2
        echo "Registering DRep staking address..."
        DREP_ADDR=$(cat drep-payment.addr)
        cardano-cli conway stake-address registration-certificate \
          --key-reg-deposit-amt 0 \
          --stake-verification-key-file drep-stake.vkey \
          --out-file register-stake.cert
        cardano-cli conway transaction build \
          --witness-override 2 \
          --tx-in $(cardano-cli query utxo --address $DREP_ADDR --output-json | jq -r 'keys[0]') \
          --change-address $DREP_ADDR \
          --certificate-file register-stake.cert \
          --out-file register-stake.body
        cardano-cli conway transaction sign \
          --signing-key-file drep-payment.skey \
          --signing-key-file drep-stake.skey \
          --tx-body-file register-stake.body \
          --out-file register-stake.tx
        cardano-cli conway transaction submit --tx-file register-stake.tx
        sleep 2
        echo "Registering DRep..."
        cardano-cli conway governance drep registration-certificate \
          --key-reg-deposit-amt 2000000 \
          --drep-verification-key-file drep.vkey \
          --out-file register-drep.cert
        cardano-cli conway transaction build \
          --witness-override 2 \
          --tx-in $(cardano-cli query utxo --address $DREP_ADDR --output-json | jq -r 'keys[0]') \
          --change-address $DREP_ADDR \
          --certificate-file register-drep.cert \
          --out-file register-drep.body
        cardano-cli conway transaction sign \
          --signing-key-file drep-payment.skey \
          --signing-key-file drep.skey \
          --tx-body-file register-drep.body \
          --out-file register-drep.tx
        cardano-cli conway transaction submit --tx-file register-drep.tx
        sleep 2
        echo "Delegating to DRep..."
        cardano-cli conway stake-address vote-delegation-certificate \
          --stake-verification-key-file drep-stake.vkey \
          --drep-verification-key-file drep.vkey \
          --out-file delegate-voting.cert
        cardano-cli conway transaction build \
          --witness-override 2 \
          --tx-in $(cardano-cli query utxo --address $DREP_ADDR --output-json | jq -r 'keys[0]') \
          --change-address $DREP_ADDR \
          --certificate-file delegate-voting.cert \
          --out-file delegate-voting.body
        cardano-cli conway transaction sign \
          --signing-key-file drep-payment.skey \
          --signing-key-file drep-stake.skey \
          --tx-body-file delegate-voting.body \
          --out-file delegate-voting.tx
        cardano-cli conway transaction submit --tx-file delegate-voting.tx
      '';
    };

    add-cold-credential-to-committee = {
      description = "Add the cold credential script to the committee via a gov action";
      group = "general";
      exec = ''
        set -e
        set -u
        set -o pipefail

        cd $(git rev-parse --show-toplevel)
        COLD_SCRIPT_HASH=$(cat cold-credential/script.hash)
        echo "Adding script hash $COLD_SCRIPT_HASH to committee via governance action..."
        cardano-cli conway governance action update-committee \
          --testnet \
          --governance-action-deposit 1000000000 \
          --anchor-url https://example.com \
          --anchor-data-hash 0000000000000000000000000000000000000000000000000000000000000000 \
          --deposit-return-stake-verification-key-file drep-stake.vkey \
          --add-cc-cold-script-hash $COLD_SCRIPT_HASH \
          --epoch 50000 \
          --threshold 0 \
          --out-file update-committee.action
        DREP_ADDR=$(cat drep-payment.addr)
        cardano-cli conway transaction build \
          --tx-in $(cardano-cli query utxo --address $DREP_ADDR --output-json | jq -r 'keys[0]') \
          --change-address $DREP_ADDR \
          --proposal-file update-committee.action \
          --out-file update-committee.body
        cardano-cli conway transaction sign \
          --signing-key-file drep-payment.skey \
          --tx-body-file update-committee.body \
          --out-file update-committee.tx
        cardano-cli conway transaction submit --tx-file update-committee.tx
        sleep 2
        echo "Voting yes on action..."
        cardano-cli conway governance vote create \
          --yes \
          --governance-action-tx-id $(cardano-cli transaction txid --tx-body-file update-committee.body) \
          --governance-action-index 0 \
          --drep-verification-key-file drep.vkey \
          --out-file drep.vote
        cardano-cli conway governance vote create \
          --yes \
          --governance-action-tx-id $(cardano-cli transaction txid --tx-body-file update-committee.body) \
          --governance-action-index 0 \
          --cold-verification-key-file testnet/example/pools/cold1.vkey \
          --out-file spo1.vote
        cardano-cli conway governance vote create \
          --yes \
          --governance-action-tx-id $(cardano-cli transaction txid --tx-body-file update-committee.body) \
          --governance-action-index 0 \
          --cold-verification-key-file testnet/example/pools/cold2.vkey \
          --out-file spo2.vote
        cardano-cli conway governance vote create \
          --yes \
          --governance-action-tx-id $(cardano-cli transaction txid --tx-body-file update-committee.body) \
          --governance-action-index 0 \
          --cold-verification-key-file testnet/example/pools/cold3.vkey \
          --out-file spo3.vote
        cardano-cli conway transaction build \
          --witness-override 5 \
          --tx-in $(cardano-cli query utxo --address $DREP_ADDR --output-json | jq -r 'keys[0]') \
          --change-address $DREP_ADDR \
          --vote-file drep.vote \
          --vote-file spo1.vote \
          --vote-file spo2.vote \
          --vote-file spo3.vote \
          --out-file vote.body
        cardano-cli conway transaction sign \
          --signing-key-file drep-payment.skey \
          --signing-key-file drep.skey \
          --signing-key-file testnet/example/pools/cold1.skey \
          --signing-key-file testnet/example/pools/cold2.skey \
          --signing-key-file testnet/example/pools/cold3.skey \
          --tx-body-file vote.body \
          --out-file vote.tx
        cardano-cli conway transaction submit --tx-file vote.tx
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
          --deposit-return-stake-verification-key-file drep-stake.vkey \
          --out-file dummy.action
        DREP_ADDR=$(cat drep-payment.addr)
        cardano-cli conway transaction build \
          --tx-in $(cardano-cli query utxo --address $DREP_ADDR --output-json | jq -r 'keys[0]') \
          --change-address $DREP_ADDR \
          --proposal-file dummy.action \
          --out-file dummy.body
        cardano-cli conway transaction sign \
          --signing-key-file drep-payment.skey \
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
  };

  shellHook = ''
    export CARDANO_NODE_SOCKET_PATH="$(git rev-parse --show-toplevel)/testnet/example/node-spo1/node.sock"
    cabal update
  '';
}
 
