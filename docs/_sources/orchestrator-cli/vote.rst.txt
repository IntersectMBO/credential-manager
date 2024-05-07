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

For example, let's assume the following:

1. The governance action ID is

   * Tx ID: ``868993aa77d9666ff9b59512d75b0d5ea088922082568a32f14bd6d29e0c5c8e``
   * Governance action index: ``0``

2. The rationale document URL is ``https://raw.githubusercontent.com/cardano-foundation/CIPs/master/CIP-0100/example.json``
3. They have decided to vote ``yes`` on the governance action.

The only missing piece of information we need to prepare is a hash of the
rationale document, for downstream verification of the contents. In accordance
with CIP-100, the way to obtain this hash is by converting the JSONLD document
to `RDF 1.2 canonical N-Quads form <https://www.w3.org/TR/rdf12-n-quads/#canonical-quads>`_
and hashing it. We can use the `jsonld` CLI to help with this:

.. code-block:: bash

   ANCHOR=https://raw.githubusercontent.com/cardano-foundation/CIPs/master/CIP-0100/example.json 
   cardano-cli conway governance hash anchor-data \
      --text "$(curl -s $ANCHOR | jsonld canonize)" \
      --out-file anchor.hash

As before, we also need the current output of the hot NFT script:

.. code-block:: bash

   $ cardano-cli conway query utxo --address $(cat hot-nft/script.addr) --output-json \
     | jq 'to_entries | .[0].value' \
     > hot-nft.utxo


Step 2: Creating the assets
---------------------------

Now we can use ``orchestrator-cli`` to build our transaction assets:

.. code-block:: bash

   $ orchestrator-cli hot-nft vote \
     --utxo-file hot-nft.utxo \
     --hot-credential-script-file hot-credential/script.plutus \
     --governance-action-tx-id 868993aa77d9666ff9b59512d75b0d5ea088922082568a32f14bd6d29e0c5c8e \
     --governance-action-index 0 \
     --yes \
     --metadata-url $ANCHOR \
     --metadata-hash $(cat anchor.hash) \
     --out-dir vote

The ``-p`` option (or ``--policy-id`` in long form) specifies the minting
policy ID to use, and the ``-o`` option (or ``--out-dir`` in long form)
specifies a directory to write the output assets to. This directory will be
created if it doesn't already exist. Every other command has an ``-o`` option
and it will not be explained elsewhere.

Let's see what assets were created.

.. code-block:: bash

   $ ls vote -1
   datum.json
   redeemer.json
   value
   vote

As before, the output datum is the same as the input datum:

.. code-block:: bash

   diff <(jq '.inlineDatum' < hot-nft.utxo) <(jq '.' < vote/datum.json)

And the redeemer instructs the script to perform the ``Vote`` action with the
provided details:

.. code-block:: bash

   cat vote/redeemer.json
   {
       "constructor": 0,
       "fields": [
           {
               "constructor": 0,
               "fields": [
                   {
                       "bytes": "868993aa77d9666ff9b59512d75b0d5ea088922082568a32f14bd6d29e0c5c8e"
                   },
                   {
                       "int": 0
                   }
               ]
           },
           {
               "constructor": 1,
               "fields": []
           }
       ]
   }

The outer constructor is the ``Vote`` constructor (index ``0``), the first
field is the governance action ID, and the second field is the ``Yes`` vote
(index ``1``).

Ignoring the ``value`` file, which as before is just a convenience for building
the transaction with ``cardano-cli``, the last file of note is ``vote``. This
is a vote file that we will add to the transaction to cast the vote:

.. code-block:: bash

   $ cardano-cli conway governance vote view --vote-file vote/vote
   {
       "committee-scriptHash-75a630a046d93e4e4415a31a1823860870aa7e84829c80645aac1e20": {
           "868993aa77d9666ff9b59512d75b0d5ea088922082568a32f14bd6d29e0c5c8e#0": {
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
   $ ORCHESTRATOR_STARTING_BALANCE=$(cardano-cli query utxo --address $(cat orchestrator.addr) --output-json | jq -r 'to_entries | .[0].value.value.lovelace')
   $ FEE=5000000
   $ ORCHESTRATOR_ENDING_BALANCE=$(($ORCHESTRATOR_STARTING_BALANCE - $FEE))
   $ cardano-cli conway transaction build-raw \
      --tx-in $(cardano-cli query utxo --address $(cat orchestrator.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-collateral $(cardano-cli query utxo --address $(cat orchestrator.addr) --output-json | jq -r 'keys[0]') \
      --tx-in $(cardano-cli query utxo --address $(cat hot-nft/script.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-script-file hot-nft/script.plutus \
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
      --vote-script-file hot-credential/script.plutus \
      --vote-redeemer-value {} \
      --vote-execution-units "(6000000000,4000000)" \
      --out-file vote.body
   Estimated transaction fee: Coin 522091

Most of what we covered when building the hot credential authorization script
also applies here, so we won't cover it again.

Step 4. Distribute the Transaction to The Voting Group
------------------------------------------------------

We now have an unsigned transaction body which we need our voters to sign.

.. code-block:: bash

   $ cardano-cli conway transaction witness \
      --tx-body-file vote.body \
      --signing-key-file example-certificates/children/child-8/child-8.skey \
      --out-file vote.child-8.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file vote.body \
      --signing-key-file example-certificates/children/child-9/child-9.skey \
      --out-file vote.child-9.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file vote.body \
      --signing-key-file orchestrator.skey \
      --out-file vote.orchestrator.witness

Step 5. Assemble and Submit the Transaction
-------------------------------------------

Finally, we can put everything together to submit the transaction:

.. code-block:: bash

   $ cardano-cli conway transaction assemble \
      --tx-body-file vote.body \
      --witness-file vote.child-8.witness \
      --witness-file vote.child-9.witness \
      --witness-file vote.orchestrator.witness \
      --out-file vote.tx
   $ cardano-cli conway transaction submit --tx-file vote.tx
   Transaction successfully submitted.

Step 6. Verify the Vote On Chain
--------------------------------

We can see the results of our vote by querying the gov state from the node:

.. code-block:: bash

   $ cardano-cli conway query gov-state | jq .proposals.[]
   {
     "actionId": {
       "govActionIx": 0,
       "txId": "868993aa77d9666ff9b59512d75b0d5ea088922082568a32f14bd6d29e0c5c8e"
     },
     "committeeVotes": {
       "scriptHash-75a630a046d93e4e4415a31a1823860870aa7e84829c80645aac1e20": "VoteYes"
     },
     "dRepVotes": {},
     "expiresAfter": 5146,
     "proposalProcedure": {
       "anchor": {
         "dataHash": "10c6d67e1968551c44d140d2f1af37d0d9b9385c8ac0c200426e3b6edb4d7c26",
         "url": "metadata.json"
       },
       "deposit": 1000000000,
       "govAction": {
         "tag": "InfoAction"
       },
       "returnAddr": {
         "credential": {
           "keyHash": "ed7a78f87bcf8640f6785ba8616101114ba4ea6d3c1b5cebfe180d69"
         },
         "network": "Testnet"
       }
     },
     "proposedIn": 5046,
     "stakePoolVotes": {}
   }
