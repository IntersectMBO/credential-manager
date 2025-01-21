#!/usr/bin/env bash
set -euo pipefail

#DREP_DIR=testnet/example/dreps
UTXO_DIR=testnet/example/utxo-keys
#POOL_DIR=testnet/example/pools
TRANSACTIONS_DIR=testnet/example/transactions
#CC_DIR=testnet/cckeys

mkdir -p "$TRANSACTIONS_DIR"

# "QUERY GOVERNANCE STATE"

echo "DOWNLOAD A PROPOSAL FILE, THIS IS WHERE WE EXPLAIN WHY THIS PROPOSAL IS RELEVANT"

curl https://tinyurl.com/3wrwb2as -o "${TRANSACTIONS_DIR}/proposal.txt"

# Calculate the hash of our proposal document (justification)
proposalHash="$(cardano-cli hash anchor-data --file-text ${TRANSACTIONS_DIR}/proposal.txt)"

echo "DOWNLOAD THE CONSTIUTION RATIFIED IN ARGENTINA"

sleep 2

curl https://ipfs.io/ipfs/QmbiATXEFuuAktbjLJJPiRyZowAgqqM3hfZoNFNmMCygjb?filename=QmbiATXEFuuAktbjLJJPiRyZowAgqqM3hfZoNFNmMCygjb \
-o ${TRANSACTIONS_DIR}/constitution.txt

echo "CALCULATE THE HASH of PROPOSED CONSTITUTION"

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
  --anchor-url "https://tinyurl.com/3wrwb2as" \
  --anchor-data-hash "$proposalHash" \
  --constitution-url "https://ipfs.io/ipfs/QmbiATXEFuuAktbjLJJPiRyZowAgqqM3hfZoNFNmMCygjb?filename=QmbiATXEFuuAktbjLJJPiRyZowAgqqM3hfZoNFNmMCygjb"  \
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

sleep 5

# Get the gov action id and index

ID="$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.txId')"
IX="$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.govActionIx')"

echo "$ID"
echo "$IX"

echo "VOTE AS DREPS AND AS SPO"

# ### ----------––––––––
# # DREP VOTES, all yes
# ### ----------––––––––

# for i in {1..3}; do
#   cardano-cli conway governance vote create \
#     --yes \
#     --governance-action-tx-id "${ID}" \
#     --governance-action-index "${IX}" \
#     --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
#     --out-file "${TRANSACTIONS_DIR}/${ID}-drep${i}.vote"
# done

# cardano-cli conway transaction build \
#   --testnet-magic $NETWORK_MAGIC \
#   --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
#   --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
#   --vote-file "${TRANSACTIONS_DIR}/${ID}-drep1.vote" \
#   --vote-file "${TRANSACTIONS_DIR}/${ID}-drep2.vote" \
#   --vote-file "${TRANSACTIONS_DIR}/${ID}-drep3.vote" \
#   --witness-override 4 \
#   --out-file "${TRANSACTIONS_DIR}/constitution-drep-votes-tx.raw"

# cardano-cli conway transaction sign \
#   --testnet-magic $NETWORK_MAGIC \
#   --tx-body-file "${TRANSACTIONS_DIR}/constitution-drep-votes-tx.raw" \
#   --signing-key-file "${UTXO_DIR}/payment1.skey" \
#   --signing-key-file "${DREP_DIR}/drep1.skey" \
#   --signing-key-file "${DREP_DIR}/drep2.skey" \
#   --signing-key-file "${DREP_DIR}/drep3.skey" \
#   --out-file "${TRANSACTIONS_DIR}/constitution-drep-votes-tx.signed"

# cardano-cli conway transaction submit \
#   --testnet-magic $NETWORK_MAGIC \
#   --tx-file "${TRANSACTIONS_DIR}/constitution-drep-votes-tx.signed"

# sleep 7

# ### ----------––––––––
# # CC VOTES
# ### ----------––––––––

# for i in {1..3}; do
#   cardano-cli conway governance vote create \
#     --yes \
#     --governance-action-tx-id "${ID}" \
#     --governance-action-index "${IX}" \
#     --cc-hot-verification-key-file "${CC_DIR}/cc${i}-hot.vkey" \
#     --out-file "${TRANSACTIONS_DIR}/cc${i}.vote"
# done

# cardano-cli conway transaction build \
#   --testnet-magic $NETWORK_MAGIC \
#   --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
#   --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
#   --vote-file "${TRANSACTIONS_DIR}/cc1.vote" \
#   --vote-file "${TRANSACTIONS_DIR}/cc2.vote" \
#   --vote-file "${TRANSACTIONS_DIR}/cc3.vote" \
#   --witness-override 4 \
#   --out-file "${TRANSACTIONS_DIR}/constitution-cc-votes-tx.raw"

# cardano-cli conway transaction sign \
#   --testnet-magic $NETWORK_MAGIC \
#   --tx-body-file "${TRANSACTIONS_DIR}/constitution-cc-votes-tx.raw" \
#   --signing-key-file "${UTXO_DIR}/payment1.skey" \
#   --signing-key-file "${CC_DIR}/cc1-hot.skey" \
#   --signing-key-file "${CC_DIR}/cc2-hot.skey" \
#   --signing-key-file "${CC_DIR}/cc3-hot.skey" \
#   --out-file "${TRANSACTIONS_DIR}/constitution-cc-votes-tx.signed"

# cardano-cli conway transaction submit \
#   --testnet-magic $NETWORK_MAGIC \
#   --tx-file "${TRANSACTIONS_DIR}/constitution-cc-votes-tx.signed"

# cardano-cli conway query gov-state --testnet-magic $NETWORK_MAGIC | jq -r '.proposals' 

# cardano-cli conway query constitution --testnet-magic $NETWORK_MAGIC
