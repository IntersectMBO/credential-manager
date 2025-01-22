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


UTXO_DIR=testnet/example/utxo-keys
TRANSACTIONS_DIR=testnet/example/transactions
GREEN="\e[32m"
BOLD="\e[1m"
RESET="\e[0m"


mkdir -p "$TRANSACTIONS_DIR"
tl
echo -e "${BOLD}${GREEN}QUERY THE CURRENT CONSITUTITION${RESET}"
tl
echo "cardano-cli conway query constitution"
cardano-cli conway query constitution
tl
pause
clear 
echo -e "${BOLD}${GREEN}LET'S PROPOSE A NEW CONSTITUTION${RESET}"
tl
pause
clear 
echo -e "${BOLD}${GREEN}DOWNLOAD A PROPOSAL FILE, THIS IS WHERE WE EXPLAIN WHY THIS PROPOSAL IS RELEVANT${RESET}"
tl
curl https://raw.githubusercontent.com/carloslodelar/proposals/refs/heads/main/constitutionalProposal.txt -o "${TRANSACTIONS_DIR}/proposal.txt"
tl
pause
clear 
echo -e "${BOLD}${GREEN}GET THE HASH OF THE PROPOSAL:${RESET}"
tl
echo "cardano-cli hash anchor-data --file-text ${TRANSACTIONS_DIR}/proposal.txt"
proposalURL="https://raw.githubusercontent.com/carloslodelar/proposals/refs/heads/main/constitutionalProposal.txt"
proposalHash="$(cardano-cli hash anchor-data --file-text ${TRANSACTIONS_DIR}/proposal.txt)"
echo "$proposalHash"
tl
pause
clear 
echo -e "${BOLD}${GREEN}DOWNLOAD THE CONSTIUTION RATIFIED IN ARGENTINA AND NAIROBI:${RESET}"
tl
curl https://ipfs.io/ipfs/QmbiATXEFuuAktbjLJJPiRyZowAgqqM3hfZoNFNmMCygjb?filename=QmbiATXEFuuAktbjLJJPiRyZowAgqqM3hfZoNFNmMCygjb \
-o ${TRANSACTIONS_DIR}/constitution.txt
tl
pause
clear 
echo -e "${BOLD}${GREEN}GET THE HASH OF THE CONSITUTION, MAKE SURE IT MATCHES THE ONE RATIFIED IN THE CONSTITUTIONAL CONVENTION:${RESET}"
tl
echo "cardano-cli hash anchor-data --file-text ${TRANSACTIONS_DIR}/constitution.txt"
constitutionURL="https://ipfs.io/ipfs/QmbiATXEFuuAktbjLJJPiRyZowAgqqM3hfZoNFNmMCygjb?filename=QmbiATXEFuuAktbjLJJPiRyZowAgqqM3hfZoNFNmMCygjb"
hash=$(cardano-cli hash anchor-data --file-text ${TRANSACTIONS_DIR}/constitution.txt)
echo "$hash"
tl
pause
clear 
echo -e "${BOLD}${GREEN}GET THE HASH OF THE GUARDRAIL SCRIPT:${RESET}"
tl
echo "cardano-cli hash script --script-file testnet/guardrailscriptV2/guardrail-script.plutus"
guardrail=$(cardano-cli hash script --script-file testnet/guardrailscriptV2/guardrail-script.plutus)
echo "$guardrail"
tl
pause
clear  
echo -e "${BOLD}${GREEN}CREATE A GOVERNANCE ACTION TO UPDATE THE CONSTITUTION${RESET}"
tl
pause
clear 
echo -e "${BOLD}${GREEN}QUERY THE GOVERNANCE ACTION DEPOSIT AMOUNT${RESET}"
tl

echo "cardano-cli conway query protocol-parameters | jq -r .govActionDeposit"
govActDeposit=$(cardano-cli conway query protocol-parameters | jq -r .govActionDeposit)
echo "$govActDeposit"
tl
pause
clear 
echo -e " CREATE A PROPOSAL TO UPDATE THE CONSTITUTION${RESET}"
tl
# shellcheck disable=SC2016
echo 'cardano-cli conway governance action create-constitution \
  --testnet \
  --governance-action-deposit "$govActDeposit" \
  --deposit-return-stake-verification-key-file "${UTXO_DIR}/stake1.vkey" \
  --anchor-url "$proposalURL" \
  --anchor-data-hash "$proposalHash" \
  --constitution-url "$constitutionURL" \
  --constitution-hash "$hash" \
  --constitution-script-hash "$guardrail" \
  --out-file ${TRANSACTIONS_DIR}/constitution.action'

cardano-cli conway governance action create-constitution \
  --testnet \
  --governance-action-deposit "$govActDeposit" \
  --deposit-return-stake-verification-key-file "${UTXO_DIR}/stake1.vkey" \
  --anchor-url "$proposalURL" \
  --anchor-data-hash "$proposalHash" \
  --constitution-url "$constitutionURL" \
  --constitution-hash "$hash" \
  --constitution-script-hash "$guardrail" \
  --out-file "${TRANSACTIONS_DIR}/constitution.action"
tl
pause
clear 
echo -e "${BOLD}${GREEN}THIS IS THE ENCODED GOVERNANCE ACTION FILE${RESET}"
cat "${TRANSACTIONS_DIR}/constitution.action" 
tl
pause
clear 
echo -e "${BOLD}${GREEN}THIS IS THE DECODED THE GOVERNANCE ACTION${RESET}"

cardano-cli conway governance action view \
  --action-file "${TRANSACTIONS_DIR}/constitution.action"
pause
clear 
#"BUILD, SIGN AND SUBMIT THE CONSTITUTION"
echo -e "${BOLD}${GREEN}BUILD, SIGN AND SUBMIT THE CONSTITUTION${RESET}"
pause
tl
# shellcheck disable=SC1079
# shellcheck disable=SC2016
# shellcheck disable=SC1078
# shellcheck disable=SC2026
echo 'cardano-cli conway transaction build \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --output-json | jq -r 'keys[0]')" \
  --proposal-file "${TRANSACTIONS_DIR}/constitution.action" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --out-file "${TRANSACTIONS_DIR}/constitution-tx.raw"'
tl
cardano-cli conway transaction build \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --output-json | jq -r 'keys[0]')" \
  --proposal-file "${TRANSACTIONS_DIR}/constitution.action" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --out-file "${TRANSACTIONS_DIR}/constitution-tx.raw"
tl
pause
clear 
# shellcheck disable=SC2016
echo 'cardano-cli conway transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/constitution-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --out-file "${TRANSACTIONS_DIR}/constitution-tx.signed"'

cardano-cli conway transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/constitution-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --out-file "${TRANSACTIONS_DIR}/constitution-tx.signed"
tl
pause
clear 
# shellcheck disable=SC2016
echo 'cardano-cli conway transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/constitution-tx.signed"'
echo ""
cardano-cli conway transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/constitution-tx.signed"
tl
tl
echo -e "${BOLD}${GREEN}WAIT A FEW SECONDS AND VERIFY THE PROPOSAL ON CHAIN${RESET}"
tl
pause
clear 

cardano-cli conway query gov-state | jq .proposals
