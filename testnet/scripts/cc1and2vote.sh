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

CC_DIR=testnet/cckeys
UTXO_DIR=testnet/example/utxo-keys
TRANSACTIONS_DIR=testnet/example/transactions
GREEN="\e[32m"
BOLD="\e[1m"
RESET="\e[0m"


echo -e "${BOLD}${GREEN}TIME TO VOTE WITH THE CC MEMBERS${RESET}"
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
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cc-hot-verification-key-file "${CC_DIR}/cc${i}-hot.vkey" \
    --out-file "${TRANSACTIONS_DIR}/cc${i}.vote"
done'

for i in {1..1}; do
  cardano-cli conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cc-hot-verification-key-file "${CC_DIR}/cc${i}-hot.vkey" \
    --out-file "${TRANSACTIONS_DIR}/cc${i}.vote"
done
# shellcheck disable=SC2016
echo 'for i in {2..2}; do
  cardano-cli conway governance vote create \
    --no \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cc-hot-verification-key-file "${CC_DIR}/cc${i}-hot.vkey" \
    --out-file "${TRANSACTIONS_DIR}/cc${i}.vote"
done'

for i in {2..2}; do
  cardano-cli conway governance vote create \
    --no \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cc-hot-verification-key-file "${CC_DIR}/cc${i}-hot.vkey" \
    --out-file "${TRANSACTIONS_DIR}/cc${i}.vote"
done
tl 
pause
clear

echo -e "${BOLD}${GREEN}BUILD, SIGN AND SUBMIT THE VOTE TRANSACTIONS ${RESET}"

# shellcheck disable=SC2016 disable=SC2026 disable=SC1079 disable=SC1078
echo 'cardano-cli conway transaction build \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --output-json | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/cc1.vote" \
  --vote-file "${TRANSACTIONS_DIR}/cc2.vote" \
  --witness-override 3 \
  --out-file "${TRANSACTIONS_DIR}/constitution-cc-votes-tx.raw"'

cardano-cli conway transaction build \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --output-json | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/cc1.vote" \
  --vote-file "${TRANSACTIONS_DIR}/cc2.vote" \
  --witness-override 3 \
  --out-file "${TRANSACTIONS_DIR}/constitution-cc-votes-tx.raw"
echo ""
# shellcheck disable=SC2016
echo 'cardano-cli conway transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/constitution-cc-votes-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${CC_DIR}/cc1-hot.skey" \
  --signing-key-file "${CC_DIR}/cc2-hot.skey" \
  --out-file "${TRANSACTIONS_DIR}/constitution-cc-votes-tx.signed"'

cardano-cli conway transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/constitution-cc-votes-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${CC_DIR}/cc1-hot.skey" \
  --signing-key-file "${CC_DIR}/cc2-hot.skey" \
  --out-file "${TRANSACTIONS_DIR}/constitution-cc-votes-tx.signed"
echo ""
# shellcheck disable=SC2016
echo 'cardano-cli conway transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/constitution-cc-votes-tx.signed"'

cardano-cli conway transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/constitution-cc-votes-tx.signed"
tl
echo -e "${BOLD}${GREEN}VOTING IS DONE, WAIT A FEW SECONDS TO VERIFY ON-CHAIN${RESET}"
pause 
clear
cardano-cli conway query gov-state | jq -r '.proposals' 



