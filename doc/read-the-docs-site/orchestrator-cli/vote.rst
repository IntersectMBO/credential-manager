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
with CIP-100, the way to obtain this hash is by using the hashing algorithm
specified in the metadata ``"hashAlgorithm": "blake2b-256"``. We can use the CLI
to help with this:

.. code-block:: bash

   $ ANCHOR=https://raw.githubusercontent.com/cardano-foundation/CIPs/master/CIP-0100/example.json 
   $ cardano-cli hash anchor-data \
      --text "$(curl -s $ANCHOR)" \
      --out-file anchor.hash

As before, we also need the current output of the hot NFT script:

.. code-block:: bash

   $ fetch-hot-nft-utxo

Alternatively, you can use a ``cardano-cli`` query for this:

.. code-block:: bash

   $ cardano-cli conway query utxo --address $(cat init-hot/nft.addr) --output-json > hot-nft.utxo


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
       "committee-scriptHash-b8928f246d726b59c51f33fc9d643b808dd273e5d9985762e464783d": {
           "febe3bd850e2c34ec6612f32d6438c9ccf965ea3b1bcc843efe3c42331fd3fec#0": {
               "anchor": {
                   "dataHash": "7b7d4a28a599bbb8c08b239be2645fa82d63a848320bf4760b07d86fcf1aabdc",
                   "url": "https://raw.githubusercontent.com/cardano-foundation/CIPs/master/CIP-0100/example.json"
               },
               "decision": "VoteYes"
           }
       }
   }

Step 3: Create the Vote Transaction
-----------------------------------

Now we have everything we need to build the transaction.

.. code-block:: bash

   $ cardano-cli conway transaction build \
      --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in $(jq -r 'keys[0]' hot-nft.utxo) \
      --tx-in-script-file init-hot/nft.plutus \
      --tx-in-inline-datum-present \
      --tx-in-redeemer-file vote/redeemer.json \
      --tx-out "$(cat vote/value)" \
      --tx-out-inline-datum-file vote/datum.json \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-8.cert) \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-9.cert) \
      --vote-file vote/vote \
      --vote-script-file init-hot/credential.plutus \
      --vote-redeemer-value {} \
      --change-address $(cat orchestrator.addr) \
      --out-file vote/body.json
   Estimated transaction fee: Coin 702241

Most of what we covered when building the hot credential authorization script
also applies here, so we won't cover it again. The only difference is that we
are attaching and authorizing a vote file instead of a certificate, but the
mechanism is similar.

Step 4. Distribute the Transaction to The Voting Group
------------------------------------------------------

We now have an unsigned transaction body which we need our voters to sign.

.. code-block:: bash

   $ cc-sign -q \
      --tx-body-file vote/body.json \
      --private-key-file example-certificates/children/child-8/child-8.private \
      --out-file vote/child-8.witness
   $ cc-sign -q \
      --tx-body-file vote/body.json \
      --private-key-file example-certificates/children/child-9/child-9.private \
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
       "txId": "febe3bd850e2c34ec6612f32d6438c9ccf965ea3b1bcc843efe3c42331fd3fec"
     },
     "committeeVotes": {
       "scriptHash-b8928f246d726b59c51f33fc9d643b808dd273e5d9985762e464783d": "VoteYes"
     },
     "dRepVotes": {},
     "expiresAfter": 121,
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
           "keyHash": "9f2512df96406ee5033478c432c7f10622ebf4a3715c0a8d50096238"
         },
         "network": "Testnet"
       }
     },
     "proposedIn": 21,
     "stakePoolVotes": {}
   }
