#!/usr/bin/env bash

# Run this script after mkfiles.sh
# This scripts uses set -x to show in terminal the commands executed by the script. Remove or comment set -x to disable this behavior
# The "exec 2>" below this comment helps the user to differenciate between the commands and its outputs by changing the color
# of the set -x output (the commands).

exec 2> >(while IFS= read -r line; do echo -e "\e[34m${line}\e[0m" >&2; done)

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -x
set -euo pipefail

setup-orchestrator

sleep 7

orchestrator-cli init-cold \
    --policy-id "$COLD_POLICY_ID" \
    --token-name "" \
    --testnet \
    --ca-cert example-certificates/ca-cert.pem \
    --membership-cert example-certificates/child-1.cert \
    --membership-cert example-certificates/child-2.cert \
    --membership-cert example-certificates/child-3.cert \
    --delegation-cert example-certificates/child-4.cert \
    --delegation-cert example-certificates/child-5.cert \
    --delegation-cert example-certificates/child-6.cert \
    -o init-cold

cp coldMint.hash init-cold/minting.plutus.hash
echo "" > init-cold/nft-token-name

cardano-cli conway transaction build \
  --change-address "$(< orchestrator.addr)" \
  --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
  --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
  --tx-out "$(cat init-cold/nft.addr) + 5000000 + 1 $COLD_POLICY_ID" \
  --tx-out-inline-datum-file init-cold/nft.datum.json \
  --mint "1 $COLD_POLICY_ID" \
  --mint-script-file coldMint.native \
  --out-file init-cold/body.json

cardano-cli conway transaction sign \
  --signing-key-file orchestrator.skey \
  --signing-key-file coldMint.skey \
  --tx-body-file init-cold/body.json \
  --out-file init-cold/tx.json

cardano-cli conway transaction submit --tx-file init-cold/tx.json

sleep 7

cardano-cli conway query committee-state --cold-script-hash "$(cat init-cold/credential.plutus.hash)"

orchestrator-cli init-hot \
    --seed-input "$(get-orchestrator-ada-only | jq -r '.key')" \
    --testnet \
    --cold-nft-policy-id "$(< init-cold/minting.plutus.hash)" \
    --cold-nft-token-name "$(< init-cold/nft-token-name)" \
    --voting-cert example-certificates/child-7.cert \
    --voting-cert example-certificates/child-8.cert \
    --voting-cert example-certificates/child-9.cert \
    -o init-hot

cardano-cli conway transaction build \
  --change-address "$(< orchestrator.addr)" \
  --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
  --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
  --tx-out "$(cat init-hot/nft.addr) + 5000000 + 1 $(cat init-hot/minting.plutus.hash).$(cat init-hot/nft-token-name)" \
  --tx-out-inline-datum-file init-hot/nft.datum.json \
  --mint "1 $(cat init-hot/minting.plutus.hash).$(cat init-hot/nft-token-name)" \
  --mint-script-file init-hot/minting.plutus \
  --mint-redeemer-file init-hot/mint.redeemer.json \
  --out-file init-hot/body.json

cardano-cli conway transaction sign \
  --signing-key-file orchestrator.skey \
  --tx-body-file init-hot/body.json \
  --out-file init-hot/tx.json

cardano-cli conway transaction submit --tx-file init-hot/tx.json

sleep 7 

cardano-cli conway query utxo --address "$(< init-hot/nft.addr)" --output-json

cardano-cli conway query utxo --address "$(< init-cold/nft.addr)" --output-json > cold-nft.utxo

orchestrator-cli authorize \
  -u cold-nft.utxo \
  --cold-credential-script-file init-cold/credential.plutus \
  --hot-credential-script-file init-hot/credential.plutus \
  --out-dir authorize

