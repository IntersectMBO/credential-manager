#!/usr/bin/env bash


# Wait for the user to press any key
pause() {
    echo "Press any key to continue..."
    read -n1 -s -r  # -n1 reads one character; -s suppresses the output
}

exec 2> >(while IFS= read -r line; do echo -e "\e[34m${line}\e[0m" >&2; done)
set -x
set -euo pipefail

#DREP_DIR=testnet/example/dreps
UTXO_DIR=testnet/example/utxo-keys
#POOL_DIR=testnet/example/pools
TRANSACTIONS_DIR=testnet/example/transactions
#CC_DIR=testnet/cckeys

mkdir -p "$TRANSACTIONS_DIR"

# "QUERY GOVERNANCE STATE"

echo "DOWNLOAD A PROPOSAL FILE, THIS IS WHERE WE EXPLAIN WHY THIS PROPOSAL IS RELEVANT"

curl https://raw.githubusercontent.com/carloslodelar/proposals/refs/heads/main/constitutionalProposal.txt -o "${TRANSACTIONS_DIR}/proposal.txt"

# Calculate the hash of our proposal document (justification)
proposalURL="https://raw.githubusercontent.com/carloslodelar/proposals/refs/heads/main/constitutionalProposal.txt"
proposalHash="$(cardano-cli hash anchor-data --file-text ${TRANSACTIONS_DIR}/proposal.txt)"

echo "DOWNLOAD THE CONSTIUTION RATIFIED IN ARGENTINA"

sleep 2

curl https://ipfs.io/ipfs/QmbiATXEFuuAktbjLJJPiRyZowAgqqM3hfZoNFNmMCygjb?filename=QmbiATXEFuuAktbjLJJPiRyZowAgqqM3hfZoNFNmMCygjb \
-o ${TRANSACTIONS_DIR}/constitution.txt

echo "CALCULATE THE HASH of PROPOSED CONSTITUTION"
constitutionURL="https://ipfs.io/ipfs/QmbiATXEFuuAktbjLJJPiRyZowAgqqM3hfZoNFNmMCygjb?filename=QmbiATXEFuuAktbjLJJPiRyZowAgqqM3hfZoNFNmMCygjb"
hash=$(cardano-cli hash anchor-data --file-text ${TRANSACTIONS_DIR}/constitution.txt)
echo "$hash"

guardrail=$(cardano-cli hash script --script-file testnet/guardrailscriptV2/guardrail-script.plutus)

currentConstitution=$(cardano-cli conway query constitution | jq -r .anchor.dataHash)

echo "CURRENT CONSTITUTION HASH IS:"
echo "$currentConstitution"


# Query the governance action deposit amount
govActDeposit=$(cardano-cli conway query protocol-parameters | jq -r .govActionDeposit)

# "CREATE A PROPOSAL TO UPDATE THE CONSTITUTION"

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

cat "${TRANSACTIONS_DIR}/constitution.action" 

cardano-cli conway governance action view \
  --action-file "${TRANSACTIONS_DIR}/constitution.action"

#"BUILD, SIGN AND SUBMIT THE CONSTITUTION"

cardano-cli conway transaction build \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --output-json | jq -r 'keys[0]')" \
  --proposal-file "${TRANSACTIONS_DIR}/constitution.action" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --out-file "${TRANSACTIONS_DIR}/constitution-tx.raw"

cardano-cli conway transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/constitution-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --out-file "${TRANSACTIONS_DIR}/constitution-tx.signed"

cardano-cli conway transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/constitution-tx.signed"

sleep 7

# Get the gov action id and index

ID="$(cardano-cli conway query gov-state | jq -r '.proposals[0].actionId.txId')"
IX="$(cardano-cli conway query gov-state | jq -r '.proposals[0].actionId.govActionIx')"

echo "$ID"
echo "$IX"
