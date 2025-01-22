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

TRANSACTIONS_DIR=testnet/example/transactions
GREEN="\e[32m"
BOLD="\e[1m"
RESET="\e[0m"


echo -e "${BOLD}${GREEN}TIME TO VOTE WITH THE LAST CC MEMBER${RESET}"
echo -e "${BOLD}${GREEN}THIS ONE USES THE CREDENTIAL MANAGER BY ${RESET}"
echo -e "${BOLD}${GREEN} By Ricky Rand, Jamie Bertram, Tomasz Rybarczyk, Thomas Velekoop${RESET}"
tl
ID="$(cardano-cli conway query gov-state | jq -r '.proposals[0].actionId.txId')"
IX="$(cardano-cli conway query gov-state | jq -r '.proposals[0].actionId.govActionIx')"
echo "GOV ACTION ID: $ID#$IX"
tl
pause
clear

echo -e "${BOLD}${GREEN}BUILD THE VOTES${RESET}"

echo -e "${BOLD}${GREEN}WE'LL NEED SOME RATIONALE FOR OUR VOTE:${RESET}"
curl https://raw.githubusercontent.com/carloslodelar/proposals/refs/heads/main/constitutionalProposal.txt -o "${TRANSACTIONS_DIR}/vote.txt"
tl
voteURL="https://raw.githubusercontent.com/carloslodelar/proposals/refs/heads/main/constitutionalProposal.txt"
voteHash="$(cardano-cli hash anchor-data --file-text ${TRANSACTIONS_DIR}/vote.txt)"
tl
echo "THE HASH OF THE VOTE'S RATIONALE IS: $voteHash"
tl
pause
clear

echo -e "${BOLD}${GREEN}BUILD THE VOTES USING THE ORCHESTRATOR-CLI${RESET}"

echo "fetch-hot-nft-utxo"
fetch-hot-nft-utxo

# shellcheck disable=SC2016
echo 'orchestrator-cli vote \
  --utxo-file hot-nft.utxo \
  --hot-credential-script-file init-hot/credential.plutus \
  --governance-action-tx-id "$ID" \
  --governance-action-index "$IX" \
  --yes \
  --metadata-url "$voteURL" \
  --metadata-hash "$voteHash" \
  --out-dir vote'

orchestrator-cli vote \
  --utxo-file hot-nft.utxo \
  --hot-credential-script-file init-hot/credential.plutus \
  --governance-action-tx-id "$ID" \
  --governance-action-index "$IX" \
  --yes \
  --metadata-url "$voteURL" \
  --metadata-hash "$voteHash" \
  --out-dir vote
tl 
pause 
clear

echo -e "${BOLD}${GREEN}LET'S EXPLORE THE VOTE FILE:${RESET}"
cardano-cli conway governance vote view --vote-file vote/vote
tl
pause
clear

echo -e "${BOLD}${GREEN}BUILD THE TRANSACTION USING THE TX-BUNDE TOOL:${RESET}"

# shellcheck disable=SC2016 disable=SC2026 disable=SC1079 disable=SC1078
echo 'tx-bundle build \
   --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
   --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
   --tx-in $(jq -r 'keys[0]' hot-nft.utxo) \
   --tx-in-script-file init-hot/nft.plutus \
   --tx-in-inline-datum-present \
   --tx-in-redeemer-file vote/redeemer.json \
   --tx-out "$(cat vote/value)" \
   --tx-out-inline-datum-file vote/datum.json \
   --required-signer-group-name voting \
   --required-signer-group-threshold 2 \
   --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-7.cert) \
   --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-8.cert) \
   --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-9.cert) \
   --vote-file vote/vote \
   --vote-script-file init-hot/credential.plutus \
   --vote-redeemer-value {} \
   --change-address $(cat orchestrator.addr) \
   --out-file vote/body.txbundle'

# shellcheck disable=SC2046
tx-bundle build \
   --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
   --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
   --tx-in $(jq -r 'keys[0]' hot-nft.utxo) \
   --tx-in-script-file init-hot/nft.plutus \
   --tx-in-inline-datum-present \
   --tx-in-redeemer-file vote/redeemer.json \
   --tx-out "$(cat vote/value)" \
   --tx-out-inline-datum-file vote/datum.json \
   --required-signer-group-name voting \
   --required-signer-group-threshold 2 \
   --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-7.cert) \
   --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-8.cert) \
   --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-9.cert) \
   --vote-file vote/vote \
   --vote-script-file init-hot/credential.plutus \
   --vote-redeemer-value {} \
   --change-address $(cat orchestrator.addr) \
   --out-file vote/body.txbundle

echo -e "${BOLD}${GREEN}NOW, THE MEMBERS OF THIS ORGANIZATON THAT HAVE THE VOTER ROLE CAN SIGN THE TRANSACTION.${RESET}"
tl
echo -e "${BOLD}${GREEN}THEY WILL USE CC-SIGN TOOL${RESET}"

echo 'cc-sign -q \
  --tx-bundle-file vote/body.txbundle \
  --private-key-file example-certificates/children/child-7/child-7.private \
  --out-file vote/child-7.witbundle'
cc-sign -q \
  --tx-bundle-file vote/body.txbundle \
  --private-key-file example-certificates/children/child-7/child-7.private \
  --out-file vote/child-7.witbundle

echo 'cc-sign -q \
  --tx-bundle-file vote/body.txbundle \
  --private-key-file example-certificates/children/child-8/child-8.private \
  --out-file vote/child-8.witbundle'

cc-sign -q \
  --tx-bundle-file vote/body.txbundle \
  --private-key-file example-certificates/children/child-8/child-8.private \
  --out-file vote/child-8.witbundle

echo 'cc-sign -q \
  --tx-bundle-file vote/body.txbundle \
  --private-key-file example-certificates/children/child-9/child-9.private \
  --out-file vote/child-9.witbundle'

cc-sign -q \
   --tx-bundle-file vote/body.txbundle \
   --private-key-file example-certificates/children/child-9/child-9.private \
   --out-file vote/child-9.witbundle

echo 'tx-bundle witness \
   --all \
   --tx-bundle-file vote/body.txbundle \
   --signing-key-file orchestrator.skey \
   --out-file vote/orchestrator.witbundle'

tx-bundle witness \
   --all \
   --tx-bundle-file vote/body.txbundle \
   --signing-key-file orchestrator.skey \
   --out-file vote/orchestrator.witbundle
tl
pause
clear

echo -e "${BOLD}${GREEN}ASSEMBLE THE TRANSACTION${RESET}"

echo 'tx-bundle assemble \
   --tx-bundle-file vote/body.txbundle \
   --witness-bundle-file vote/child-8.witbundle \
   --witness-bundle-file vote/child-9.witbundle \
   --witness-bundle-file vote/orchestrator.witbundle \
   --out-file vote/tx.json'

tx-bundle assemble \
   --tx-bundle-file vote/body.txbundle \
   --witness-bundle-file vote/child-8.witbundle \
   --witness-bundle-file vote/child-9.witbundle \
   --witness-bundle-file vote/orchestrator.witbundle \
   --out-file vote/tx.json
tl
pause
clear
echo -e "${BOLD}${GREEN}SUBMIT THE TRANSACTION${RESET}"

echo 'cardano-cli conway transaction submit --tx-file vote/tx.json'
cardano-cli conway transaction submit --tx-file vote/tx.json

pause

echo -e "${BOLD}${GREEN}WAIT A FEW SECONDS TO CONFIRM THE VOTE IS ON-CHAIN${RESET}"
cardano-cli conway query gov-state | jq '.proposals[]'
