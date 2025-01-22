#!/usr/bin/env bash



# Wait for the user to press any key
pause() {
    echo "Press any key to continue..."
    read -n1 -s -r  # -n1 reads one character; -s suppresses the output
}

tl() {
    echo ""
    echo ""
} 

set -euo pipefail

DREP_DIR=testnet/example/dreps
UTXO_DIR=testnet/example/utxo-keys
TRANSACTIONS_DIR=testnet/example/transactions
GREEN="\e[32m"
BOLD="\e[1m"
RESET="\e[0m"

echo -e "${BOLD}${GREEN}LET'S HAVE THE DREPS VOTE FIRST${RESET}"
tl
ID="$(cardano-cli conway query gov-state | jq -r '.proposals[0].actionId.txId')"
IX="$(cardano-cli conway query gov-state | jq -r '.proposals[0].actionId.govActionIx')"
echo "GOV ACTION ID: $ID#$IX"
tl
pause
clear
echo -e "${BOLD}${GREEN}BUILD THE VOTES${RESET}"
tl
# shellcheck disable=SC2016
echo 'for i in {1..1}; do
  cardano-cli conway governance vote create \
    --no \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/${ID}-drep${i}.vote"
done'

for i in {1..1}; do
  cardano-cli conway governance vote create \
    --no \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/${ID}-drep${i}.vote"
done

# shellcheck disable=SC2016
echo 'for i in {2..3}; do
  cardano-cli conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/${ID}-drep${i}.vote"
done'

for i in {2..3}; do
  cardano-cli conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/${ID}-drep${i}.vote"
done
tl
pause
clear
echo -e "${BOLD}${GREEN}BUILD, SIGN AND SUBMIT THE VOTE TRANSACTION${RESET}"

# shellcheck disable=SC1078
# shellcheck disable=SC2016
# shellcheck disable=SC2026
# shellcheck disable=SC1079
echo 'cardano-cli conway transaction build \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --output-json | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/${ID}-drep1.vote" \
  --vote-file "${TRANSACTIONS_DIR}/${ID}-drep2.vote" \
  --vote-file "${TRANSACTIONS_DIR}/${ID}-drep3.vote" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/constitution-drep-votes-tx.raw"'

cardano-cli conway transaction build \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --output-json | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/${ID}-drep1.vote" \
  --vote-file "${TRANSACTIONS_DIR}/${ID}-drep2.vote" \
  --vote-file "${TRANSACTIONS_DIR}/${ID}-drep3.vote" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/constitution-drep-votes-tx.raw"
tl
# shellcheck disable=SC2016
echo 'cardano-cli conway transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/constitution-drep-votes-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${DREP_DIR}/drep1.skey" \
  --signing-key-file "${DREP_DIR}/drep2.skey" \
  --signing-key-file "${DREP_DIR}/drep3.skey" \
  --out-file "${TRANSACTIONS_DIR}/constitution-drep-votes-tx.signed"'

cardano-cli conway transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/constitution-drep-votes-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${DREP_DIR}/drep1.skey" \
  --signing-key-file "${DREP_DIR}/drep2.skey" \
  --signing-key-file "${DREP_DIR}/drep3.skey" \
  --out-file "${TRANSACTIONS_DIR}/constitution-drep-votes-tx.signed"
tl
# shellcheck disable=SC2016
echo 'cardano-cli conway transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/constitution-drep-votes-tx.signed"'
cardano-cli conway transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/constitution-drep-votes-tx.signed"

echo -e "${BOLD}${GREEN}WAIT A FEW SECONDS AND CHECK THE VOTES ON-CHAIN${RESET}"
pause 

cardano-cli conway query gov-state | jq .proposals
