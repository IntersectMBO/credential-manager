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

UTXO_DIR=testnet/example/utxo-keys
POOL_DIR=testnet/example/pools
TRANSACTIONS_DIR=testnet/example/transactions

mkdir -p "$TRANSACTIONS_DIR"

# ----------------------

# GENERATE NEW PAYMENT KEYS

for i in {1..3}; do
cardano-cli conway address key-gen \
  --verification-key-file "${UTXO_DIR}/payment${i}.vkey" \
  --signing-key-file "${UTXO_DIR}/payment${i}.skey"

done

# GENERATE NEW STAKE KEYS

for i in {1..3}; do
cardano-cli conway stake-address key-gen \
  --verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
  --signing-key-file "${UTXO_DIR}/stake${i}.skey"
done

# BUILD ADDRESSES FOR OUR NEW KEYS
for i in {1..3}; do
  cardano-cli conway address build \
    --payment-verification-key-file "${UTXO_DIR}/payment${i}.vkey" \
    --stake-verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
    --out-file  "${UTXO_DIR}/payment${i}.addr"
done

for i in {1..3}; do
  cardano-cli conway stake-address build \
  --stake-verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
  --out-file "${UTXO_DIR}/stake${i}.addr"
done

# BUILD ADDRESSES FOR THE EXISTING KEYS, WE WILL NEED THEM FOR OUT FUTURE TRANSACTIONS

for i in {1..3}; do
  cardano-cli conway address build \
    --payment-verification-key-file "${UTXO_DIR}/utxo${i}.vkey" \
    --out-file  "${UTXO_DIR}/utxo${i}.addr"
done

# --------------------
# FUND OUR NEWLY CREATED ADDRESSES

cardano-cli conway transaction build \
  --tx-in "$(cardano-cli conway query utxo --address "$(cat "${UTXO_DIR}/utxo1.addr")" --output-json | jq -r 'keys[0]')" \
  --tx-in "$(cardano-cli conway query utxo --address "$(cat "${UTXO_DIR}/utxo2.addr")" --output-json | jq -r 'keys[0]')" \
  --tx-in "$(cardano-cli conway query utxo --address "$(cat "${UTXO_DIR}/utxo3.addr")" --output-json | jq -r 'keys[0]')" \
  --tx-out "$(cat ${UTXO_DIR}/payment1.addr)+500000000000" \
  --tx-out "$(cat ${UTXO_DIR}/payment2.addr)+500000000000" \
  --tx-out "$(cat ${UTXO_DIR}/payment3.addr)+500000000000" \
  --change-address "$(cat ${UTXO_DIR}/utxo1.addr)" \
  --out-file "${TRANSACTIONS_DIR}/tx.raw"

cardano-cli conway transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/tx.raw" \
  --signing-key-file "${UTXO_DIR}/utxo1.skey" \
  --signing-key-file "${UTXO_DIR}/utxo2.skey" \
  --signing-key-file "${UTXO_DIR}/utxo3.skey" \
  --out-file "${TRANSACTIONS_DIR}/tx.signed"

cardano-cli conway transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/tx.signed"

sleep 5

# SHOW THE UTXO DISTRIBUTION

cardano-cli conway query utxo --whole-utxo 

# REGISTRATION CERTS FOR STAKE ADDRESSES

keyDeposit="$(cardano-cli conway query protocol-parameters --testnet-magic 42 | jq .stakeAddressDeposit)"

for i in {1..3}; do
  cardano-cli conway stake-address registration-certificate \
    --stake-verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
    --key-reg-deposit-amt "$keyDeposit" \
    --out-file "${TRANSACTIONS_DIR}/stake${i}reg.cert"
done

# REGISTRATION CERTS FOR POOL'S STAKE ADDRESSES

for i in {1..2}; do
  cardano-cli conway stake-address registration-certificate \
    --stake-verification-key-file "${POOL_DIR}/staking-reward${i}.vkey" \
    --key-reg-deposit-amt "$keyDeposit" \
    --out-file "${TRANSACTIONS_DIR}/pool${i}reg.cert"
done

# BUILD THE REGISTRATION TRANSACTION

cardano-cli conway transaction build \
  --tx-in "$(cardano-cli conway query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --output-json | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --certificate-file "${TRANSACTIONS_DIR}/stake1reg.cert" \
  --certificate-file "${TRANSACTIONS_DIR}/stake2reg.cert" \
  --certificate-file "${TRANSACTIONS_DIR}/stake3reg.cert" \
  --certificate-file "${TRANSACTIONS_DIR}/pool1reg.cert" \
  --certificate-file "${TRANSACTIONS_DIR}/pool2reg.cert" \
  --witness-override 6 \
  --out-file "${TRANSACTIONS_DIR}/reg-stake-tx.raw"

cardano-cli conway transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/reg-stake-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${UTXO_DIR}/stake1.skey" \
  --signing-key-file "${UTXO_DIR}/stake2.skey" \
  --signing-key-file "${UTXO_DIR}/stake3.skey" \
  --signing-key-file "${POOL_DIR}/staking-reward1.skey" \
  --signing-key-file "${POOL_DIR}/staking-reward2.skey" \
  --out-file "${TRANSACTIONS_DIR}/reg-stake-tx.signed"

cardano-cli conway transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/reg-stake-tx.signed"

sleep 9

# DELEGATE STAKE KEYS TO POOLS

for i in {1..2}; do
  cardano-cli conway stake-address stake-delegation-certificate \
    --stake-verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
    --stake-pool-verification-key-file "${POOL_DIR}/cold1.vkey" \
    --out-file "${TRANSACTIONS_DIR}/stake${i}-pool-deleg.cert"
done

for i in {3..3}; do
  cardano-cli conway stake-address stake-delegation-certificate \
    --stake-verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
    --stake-pool-verification-key-file "${POOL_DIR}/cold2.vkey" \
    --out-file "${TRANSACTIONS_DIR}/stake${i}-pool-deleg.cert"
done

# DELEGATE REWARD ACCOUNT TO POOLS

for i in {1..2}; do
  cardano-cli conway stake-address stake-delegation-certificate \
    --stake-verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
    --stake-pool-verification-key-file "${POOL_DIR}/cold1.vkey" \
    --out-file "${TRANSACTIONS_DIR}/stake${i}-pool-deleg.cert"
done

cardano-cli conway transaction build \
  --tx-in "$(cardano-cli conway query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --output-json  | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --certificate-file "${TRANSACTIONS_DIR}/stake1-pool-deleg.cert" \
  --certificate-file "${TRANSACTIONS_DIR}/stake2-pool-deleg.cert" \
  --certificate-file "${TRANSACTIONS_DIR}/stake3-pool-deleg.cert" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/stake-delegation-tx.raw"

cardano-cli conway transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/stake-delegation-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${UTXO_DIR}/stake1.skey" \
  --signing-key-file "${UTXO_DIR}/stake2.skey" \
  --signing-key-file "${UTXO_DIR}/stake3.skey" \
  --out-file "${TRANSACTIONS_DIR}/stake-delegation-tx.raw"

cardano-cli conway transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/stake-delegation-tx.raw"

sleep 9

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

tx-bundle build \
  --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
  --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
  --tx-in "$(cardano-cli query utxo --address "$(cat init-cold/nft.addr)" --output-json | jq -r 'keys[0]')" \
  --tx-in-script-file init-cold/nft.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file authorize/redeemer.json \
  --tx-out "$(cat authorize/value)" \
  --tx-out-inline-datum-file authorize/datum.json \
  --required-signer-group-name delegation \
  --required-signer-group-threshold 2 \
  --required-signer-hash "$(orchestrator-cli extract-pub-key-hash example-certificates/child-4.cert)" \
  --required-signer-hash "$(orchestrator-cli extract-pub-key-hash example-certificates/child-5.cert)" \
  --required-signer-hash "$(orchestrator-cli extract-pub-key-hash example-certificates/child-6.cert)" \
  --certificate-file authorize/authorizeHot.cert \
  --certificate-script-file init-cold/credential.plutus \
  --certificate-redeemer-value {} \
  --change-address "$(cat orchestrator.addr)" \
  --out-file authorize/body.txbundle

cc-sign -q \
  --tx-bundle-file authorize/body.txbundle \
  --private-key-file example-certificates/children/child-4/child-4.private \
  --out-file authorize/child-4.witbundle

cc-sign -q \
   --tx-bundle-file authorize/body.txbundle \
   --private-key-file example-certificates/children/child-5/child-5.private \
   --out-file authorize/child-5.witbundle

tx-bundle witness \
   --all \
   --tx-bundle-file authorize/body.txbundle \
   --signing-key-file orchestrator.skey \
   --out-file authorize/orchestrator.witbundle

tx-bundle assemble \
   --tx-bundle-file authorize/body.txbundle \
   --witness-bundle-file authorize/child-4.witbundle \
   --witness-bundle-file authorize/child-5.witbundle \
   --witness-bundle-file authorize/orchestrator.witbundle \
   --out-file authorize/tx.json

cardano-cli conway transaction submit --tx-file authorize/tx.json

sleep 7 

for i in {1..2} ; do
  cardano-cli conway governance committee create-hot-key-authorization-certificate \
  --cold-verification-key-file cc"${i}"-cold.vkey \
  --hot-verification-key-file cc"${i}"-hot.vkey \
  --out-file cc"${i}"-authorizeHot.cert
done



cardano-cli conway transaction build \
  --tx-in 

