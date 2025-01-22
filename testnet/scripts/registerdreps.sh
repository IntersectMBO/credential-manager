#!/usr/bin/env bash

exec 2> >(while IFS= read -r line; do echo -e "\e[34m${line}\e[0m" >&2; done)
set -x
set -euo pipefail

DREP_DIR=testnet/example/dreps
UTXO_DIR=testnet/example/utxo-keys
TRANSACTIONS_DIR=testnet/example/transactions

mkdir -p "$TRANSACTIONS_DIR"
mkdir -p "$DREP_DIR"

# GENERATE DREP KEYS

for i in {1..3}; do
  cardano-cli conway governance drep key-gen \
    --verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --signing-key-file "${DREP_DIR}/drep${i}.skey"
done

# GENERATE AND SUBMIT REGISTRATION CERTIFICATES

drepDeposit=$(cardano-cli conway query protocol-parameters | jq -r .dRepDeposit)

for i in {1..3}; do
  cardano-cli conway governance drep registration-certificate \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --key-reg-deposit-amt "$drepDeposit" \
    --out-file "${TRANSACTIONS_DIR}/drep${i}-reg.cert"
done

cardano-cli conway transaction build \
  --tx-in "$(cardano-cli conway query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --certificate-file "${TRANSACTIONS_DIR}/drep1-reg.cert" \
  --certificate-file "${TRANSACTIONS_DIR}/drep2-reg.cert" \
  --certificate-file "${TRANSACTIONS_DIR}/drep3-reg.cert" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/drep-reg-tx.raw"

cardano-cli conway transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/drep-reg-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${DREP_DIR}/drep1.skey" \
  --signing-key-file "${DREP_DIR}/drep2.skey" \
  --signing-key-file "${DREP_DIR}/drep3.skey" \
  --out-file "${TRANSACTIONS_DIR}/drep-reg-tx.signed"

cardano-cli conway transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/drep-reg-tx.signed"

sleep 9

# GENERATE DELEGATION CERTIFICATES FROM STAKE ADDRESSES TO THE DREPS

for i in {1..3}; do
  cardano-cli conway stake-address vote-delegation-certificate \
    --stake-verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/drep-deleg${i}.cert"
done

cardano-cli conway transaction build \
  --tx-in "$(cardano-cli conway query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --certificate-file "${TRANSACTIONS_DIR}/drep-deleg1.cert" \
  --certificate-file "${TRANSACTIONS_DIR}/drep-deleg2.cert" \
  --certificate-file "${TRANSACTIONS_DIR}/drep-deleg3.cert" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/drep-deleg-tx.raw"

cardano-cli conway transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/drep-deleg-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${UTXO_DIR}/stake1.skey" \
  --signing-key-file "${UTXO_DIR}/stake2.skey" \
  --signing-key-file "${UTXO_DIR}/stake3.skey" \
  --out-file "${TRANSACTIONS_DIR}/drep-deleg-tx.signed"

cardano-cli conway transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/drep-deleg-tx.signed"

sleep 9

# QUERY DREP STATE TO CONFIRM

cardano-cli conway query drep-state --all-dreps --include-stake

echo "DREPS REGISTERED SUCCESFULLY" 