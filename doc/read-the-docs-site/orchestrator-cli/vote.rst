.. _vote:

Voting on Governance Actions
============================

Now that we have authorized our hot credential, we are able to vote on
governance actions.

Step 1: Obtaining relevant voting information
---------------------------------------------

The voting group is responsible for deliberating, documenting, and casting
votes on governance action. When they request a vote transaction from you, they
are responsible for providing:

1. The ID of the governance action for which to vote, consisting of a tx ID and
   governance action index.
2. A URL that resolves to a rationale document in CIP-100-compliant JSONLD
   format.
3. The decision for their vote (yes, no, or abstain).

If you have been following along with the guide, you can create a sample
governance action with the command ``create-dummy-gov-action``

.. code-block:: bash

   $ create-dummy-gov-action
   Estimated transaction fee: Coin 174213
   Transaction successfully submitted.
   Governance Action Tx ID: 662d4c4eb59673a2c67882a58dd42465088973edc4f5314211169ea2c981dbe3
   Governance Action Index: 0

This will print out the governance action's tx ID and index, both of which will
be needed later.

For the rationale document URL, we will use
``https://raw.githubusercontent.com/cardano-foundation/CIPs/master/CIP-0100/example.json``,
and we will be voting ``yes`` on the governance action.

The only missing piece of information we need to prepare is a hash of the
rationale document, for downstream verification of the contents. In accordance
with CIP-100, the way to obtain this hash is by converting the JSONLD document
to `RDF 1.2 canonical N-Quads form <https://www.w3.org/TR/rdf12-n-quads/#canonical-quads>`_
and hashing it. We can use the `jsonld` CLI to help with this:

.. code-block:: bash

   $ ANCHOR=https://raw.githubusercontent.com/cardano-foundation/CIPs/master/CIP-0100/example.json 
   $ cardano-cli conway governance hash anchor-data \
      --text "$(curl -s $ANCHOR)" \
      --out-file anchor.hash

As before, we also need the current output of the hot NFT script:

.. code-block:: bash

   $ fetch-hot-nft-utxo


Step 2: Creating the assets
---------------------------

Now we can use ``orchestrator-cli`` to build our transaction assets:

.. code-block:: bash

   $ orchestrator-cli vote \
     --utxo-file hot-nft.utxo \
     --hot-credential-script-file init-hot/credential.plutus \
     --governance-action-tx-id $(cardano-cli conway query gov-state | jq -r '.proposals[0].actionId.txId') \
     --governance-action-index 0 \
     --yes \
     --metadata-url $ANCHOR \
     --metadata-hash $(cat anchor.hash) \
     --out-dir vote

.. note::
   You can cast more than one vote per transaction. To specify multiple votes,
   specify ``--governance-action-tx-id``, ``--governance-action-index``,
   ``(--yes|--no|--abstain)``, ``--metadata-url``, and ``--metadata-hash`` once
   for each vote. Be careful about the ordering when specifying multiple votes,
   as the order of the arguments determines how they are grouped when building
   the vote file.

Let's see what assets were created.

.. code-block:: bash

   $ ls vote -1
   datum.json
   redeemer.json
   value
   vote

As before, the output datum is the same as the input datum:

.. code-block:: bash

   diff <(jq 'to_entries | .[0].value.inlineDatum' < hot-nft.utxo) <(jq '.' < vote/datum.json)

And the redeemer instructs the script to perform the ``Vote`` action:

.. code-block:: bash

   cat vote/redeemer.json
   {
       "constructor": 7,
       "fields": []
   }

Ignoring the ``value`` file, which as before is just a convenience for building
the transaction with ``cardano-cli``, the last file of note is ``vote``. This
is a vote file that we will add to the transaction to cast the vote:

.. code-block:: bash

   $ cardano-cli conway governance vote view --vote-file vote/vote
   {
       "committee-scriptHash-ed846aa91731bfda6ea5e3b63db8de547b201f8dee5013de05accd3a": {
           "7d1233eccb44570bb7e5f418188af542edb3a68fda6d109bac8148cd8ec6ca47#0": {
               "anchor": {
                   "dataHash": "0a5479805b25fcfd7a35d4016747659f47c1f8558ea17f5aeabb684ed537950d",
                   "url": "https://raw.githubusercontent.com/cardano-foundation/CIPs/master/CIP-0100/example.json"
               },
               "decision": "VoteYes"
           }
       }
   }

Step 3: Create the Vote Transaction
-----------------------------------

Now we have everything we need to build the transaction. Note that at the time
of writing this documentation, there is a bug in ``cardano-cli conway
transaction build`` which causes underestimation of vote script execution
units. This unfortunately means that we will have to use ``build-raw`` instead
of ``build`` for the time being. The main differences between the two are:

1. ``build-raw`` does not balance the transaction for you - you must compute
   your own change output.
2. ``build-raw`` does not validate the transaction for you
3. You need to compute fees and execution budgets for yourself. We make the job
   easier here by massively overestimating both, but you could use more
   conservative values if you wanted to.
4. you need to download and pass the protocol parameters into the command
   explicitly.

With that out of the way, here is the command to build the transaction:

.. code-block:: bash

   $ cardano-cli conway query protocol-parameters --out-file pparams.json
   $ ORCHESTRATOR_STARTING_BALANCE=$(get-orchestrator-ada-only | jq -r '.value.value.lovelace')
   $ FEE=5000000
   $ ORCHESTRATOR_ENDING_BALANCE=$(($ORCHESTRATOR_STARTING_BALANCE - $FEE))
   $ cardano-cli conway transaction build-raw \
      --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in $(cardano-cli query utxo --address $(cat init-hot/nft.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-script-file init-hot/nft.plutus \
      --tx-in-inline-datum-present \
      --tx-in-redeemer-file vote/redeemer.json \
      --tx-in-execution-units "(3000000000, 4000000)" \
      --tx-out "$(cat vote/value)" \
      --tx-out-inline-datum-file vote/datum.json \
      --tx-out "$(cat orchestrator.addr)+$ORCHESTRATOR_ENDING_BALANCE" \
      --fee $FEE \
      --protocol-params-file pparams.json \
      --required-signer-hash $(cat example-certificates/children/child-8/child-8.keyhash) \
      --required-signer-hash $(cat example-certificates/children/child-9/child-9.keyhash) \
      --vote-file vote/vote \
      --vote-script-file init-hot/credential.plutus \
      --vote-redeemer-value {} \
      --vote-execution-units "(6000000000,4000000)" \
      --out-file vote/body.json

Most of what we covered when building the hot credential authorization script
also applies here, so we won't cover it again. The only difference is that we
are attaching and authorizing a vote file instead of a certificate, but the
mechanism is similar.

Step 4. Distribute the Transaction to The Voting Group
------------------------------------------------------

We now have an unsigned transaction body which we need our voters to sign.

.. code-block:: bash

   $ cardano-cli conway transaction witness \
      --tx-body-file vote/body.json \
      --signing-key-file example-certificates/children/child-8/child-8.skey \
      --out-file vote/child-8.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file vote/body.json \
      --signing-key-file example-certificates/children/child-9/child-9.skey \
      --out-file vote/child-9.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file vote/body.json \
      --signing-key-file orchestrator.skey \
      --out-file vote/orchestrator.witness

Step 5. Assemble and Submit the Transaction
-------------------------------------------

Finally, we can put everything together to submit the transaction:

.. code-block:: bash

   $ cardano-cli conway transaction assemble \
      --tx-body-file vote/body.json \
      --witness-file vote/child-8.witness \
      --witness-file vote/child-9.witness \
      --witness-file vote/orchestrator.witness \
      --out-file vote/tx.json
   $ cardano-cli conway transaction submit --tx-file vote/tx.json
   Transaction successfully submitted.

Step 6. Verify the Vote On Chain
--------------------------------

We can see the results of our vote by querying the gov state from the node:

.. code-block:: bash

   $ cardano-cli conway query gov-state | jq '.proposals[]'
   {
     "actionId": {
       "govActionIx": 0,
       "txId": "7d1233eccb44570bb7e5f418188af542edb3a68fda6d109bac8148cd8ec6ca47"
     },
     "committeeVotes": {
       "scriptHash-ed846aa91731bfda6ea5e3b63db8de547b201f8dee5013de05accd3a": "VoteYes"
     },
     "dRepVotes": {},
     "expiresAfter": 111,
     "proposalProcedure": {
       "anchor": {
         "dataHash": "0000000000000000000000000000000000000000000000000000000000000000",
         "url": "https://example.com"
       },
       "deposit": 1000000000,
       "govAction": {
         "tag": "InfoAction"
       },
       "returnAddr": {
         "credential": {
           "keyHash": "75d120c4fdb6f5a978357663ac3884074a8344e17964bcc7e3ec3d8e"
         },
         "network": "Testnet"
       }
     },
     "proposedIn": 11,
     "stakePoolVotes": {}
   }
