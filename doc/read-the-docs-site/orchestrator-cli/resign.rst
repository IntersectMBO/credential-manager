.. _resign:

Resigning from the Committee
============================

.. warning::
   Resigning from the constitutional committee is irreversible. The only way to
   get added back to the committee is via a governance action.

It's time to remove ourselves from the committee. It's been a long road and
we've learned a lot along the way, but our time has come.

Step 1: Creating the assets
---------------------------

We can use ``orchestrator-cli`` to build our transaction assets. As before, we
need an anchor that resolves to a metadata context document documenting our
reason for resigning. For the purposes of demonstration, let's reuse the same
one from :ref:`voting <vote>`.

.. code-block:: bash

   $ ANCHOR=https://raw.githubusercontent.com/cardano-foundation/CIPs/master/CIP-0100/example.json
   $ cardano-cli conway governance hash anchor-data \
     --text "$(curl -s $ANCHOR | jsonld canonize)" \
     --out-file anchor.hash
   $ fetch-cold-nft-utxo
   $ orchestrator-cli cold-nft resign \
     --utxo-file cold-nft.utxo \
     --cold-credential-script-file cold-credential/script.plutus \
     --metadata-url $ANCHOR \
     --metadata-hash $(cat anchor.hash) \
     --out-dir resign

Let's see what assets were created.

.. code-block:: bash

   $ ls resign -1
   datum.json
   redeemer.json
   resign.cert
   value

Once again, the output datum is the same as the input datum:

.. code-block:: bash

   diff <(jq '.inlineDatum' < cold-nft.utxo) <(jq '.' < resign/datum.json)

And the redeemer instructs the script to perform the ``resign`` action.

.. code-block:: bash

   cat resign/redeemer.json
   {
       "constructor": 1,
       "fields": []
   }

We also have a new certificate, `resign.cert`:

.. code-block:: bash

   $ cat resign/resign.cert
   {
       "type": "CertificateConway",
       "description": "Constitution committee member hot key resignation",
       "cborHex": "830f8201581ce04f00fc3676b756fcebeaabeb5b297d9cb6f0589db0893f6d5beb8b82785668747470733a2f2f7261772e67697468756275736572636f6e74656e742e636f6d2f63617264616e6f2d666f756e646174696f6e2f434950732f6d61737465722f4349502d303130302f6578616d706c652e6a736f6e58200a5479805b25fcfd7a35d4016747659f47c1f8558ea17f5aeabb684ed537950d"
   }

.. note::
   You may have noticed that all transactions in the system can be broadly
   divided into two categories: transactions that change gov state and
   transactions that change the script's datum. This is not a coincidence: by
   design, each transaction type is only capable of enacting one effect at a
   time. This is to prevent possible exploits such as a single voter including
   a vote while resigning, effectively bypassing the multisig requirement. So,
   either a transaction issues a certificate/ casts a vote, or it changes the
   datum.

   The only exception are the two unlock actions, which impose no requirements
   other than the appropriate multisig one.


Step 2: Create the Resign Transaction
-------------------------------------

The transaction must be signed by the membership group.

.. code-block:: bash

   $ cardano-cli conway transaction build \
      --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in $(cardano-cli query utxo --address $(cat cold-nft/script.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-script-file cold-nft/script.plutus \
      --tx-in-inline-datum-present \
      --tx-in-redeemer-file resign/redeemer.json \
      --tx-out "$(cat resign/value)" \
      --tx-out-inline-datum-file resign/datum.json \
      --required-signer-hash $(cat example-certificates/children/child-4/child-4.keyhash) \
      --required-signer-hash $(cat example-certificates/children/child-5/child-5.keyhash) \
      --certificate-file resign/resign.cert \
      --certificate-script-file cold-credential/script.plutus \
      --certificate-redeemer-value {} \
      --change-address $(cat orchestrator.addr) \
      --out-file resign/body.json
   Estimated transaction fee: Coin 517587

Again, recall that we previously swapped the membership and delegation roles,
so ``child-4`` and ``child-5`` are now in the membership group.

Step 3. Distribute the Transaction to The Membership Group
----------------------------------------------------------

We now have an unsigned transaction body which we need our membership group to
sign.

.. code-block:: bash

   $ cardano-cli conway transaction witness \
      --tx-body-file resign/body.json \
      --signing-key-file example-certificates/children/child-4/child-4.skey \
      --out-file resign/child-4.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file resign/body.json \
      --signing-key-file example-certificates/children/child-5/child-5.skey \
      --out-file resign/child-5.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file resign/body.json \
      --signing-key-file orchestrator.skey \
      --out-file resign/orchestrator.witness

Step 4. Assemble and Submit the Transaction
-------------------------------------------

Finally, we can put everything together to submit the transaction:

.. code-block:: bash

   $ cardano-cli conway transaction assemble \
      --tx-body-file resign/body.json \
      --witness-file resign/child-4.witness \
      --witness-file resign/child-5.witness \
      --witness-file resign/orchestrator.witness \
      --out-file resign/tx.json
   $ cardano-cli conway transaction submit --tx-file resign/tx.json
   Transaction successfully submitted.

Step 5. Verify the Resignation On Chain
---------------------------------------

We can see the results of our resignation by querying the committee state from
the node:

.. code-block:: bash

   $ cardano-cli conway query committee-state --cold-script-hash $(cat cold-credential/script.hash)
   {
       "committee": {
           "scriptHash-e04f00fc3676b756fcebeaabeb5b297d9cb6f0589db0893f6d5beb8b": {
               "expiration": 50000,
               "hotCredsAuthStatus": {
                   "contents": {
                       "dataHash": "0a5479805b25fcfd7a35d4016747659f47c1f8558ea17f5aeabb684ed537950d",
                       "url": "https://raw.githubusercontent.com/cardano-foundation/CIPs/master/CIP-0100/example.json"
                   },
                   "tag": "MemberResigned"
               },
               "nextEpochChange": {
                   "tag": "NoChangeExpected"
               },
               "status": "Active"
           }
       },
       "epoch": 210,
       "threshold": 0
   }

Although we are still in the committee, we can no longer authorize hot
credentials nor vote - so we have effectively removed ourselves from the
committee.
