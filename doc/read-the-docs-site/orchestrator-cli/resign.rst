.. _resign:

Resigning from the Committee
============================

.. warning::
   Resigning from the constitutional committee is irreversible. The only way to
   get added back to the committee is via a governance action.

It's time to remove ourselves from the committee.
It's been a long road and we've learned a lot along the way, but our time has come.

Step 1: Creating the assets
---------------------------

We can use ``orchestrator-cli`` to build our transaction assets.
As before, we need an anchor that resolves to a metadata context document documenting our reason for resigning.
For the purposes of demonstration, let's reuse the same one from :ref:`voting <vote>`.

.. code-block:: bash

   $ ANCHOR=https://raw.githubusercontent.com/cardano-foundation/CIPs/master/CIP-0100/example.json
   $ cardano-cli hash anchor-data \
     --text "$(curl -s $ANCHOR)" \
     --out-file anchor.hash
   $ fetch-cold-nft-utxo
   $ orchestrator-cli resign-committee \
     --utxo-file cold-nft.utxo \
     --cold-credential-script-file init-cold/credential.plutus \
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

   diff <(jq 'to_entries | .[0].value.inlineDatum' < cold-nft.utxo) <(jq '.' < resign/datum.json)

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
       "cborHex": "830f8201581c533e8af4515ba4cf6035ac7087ae8978e09692fea68e9466f1683f2882785668747470733a2f2f7261772e67697468756275736572636f6e74656e742e636f6d2f63617264616e6f2d666f756e646174696f6e2f434950732f6d61737465722f4349502d303130302f6578616d706c652e6a736f6e58207b7d4a28a599bbb8c08b239be2645fa82d63a848320bf4760b07d86fcf1aabdc"
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

   $ tx-bundle build \
      --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in $(cardano-cli query utxo --address $(cat init-cold/nft.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-script-file init-cold/nft.plutus \
      --tx-in-inline-datum-present \
      --tx-in-redeemer-file resign/redeemer.json \
      --tx-out "$(cat resign/value)" \
      --tx-out-inline-datum-file resign/datum.json \
      --required-signer-group-name membership \
      --required-signer-group-threshold 1 \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-4.cert) \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-5.cert) \
      --certificate-file resign/resign.cert \
      --certificate-script-file init-cold/credential.plutus \
      --certificate-redeemer-value {} \
      --change-address $(cat orchestrator.addr) \
      --out-file resign/body.txbundle
   Estimated transaction fee: Coin 755398

Again, recall that we previously swapped the membership and delegation roles,
so ``child-4`` and ``child-5`` are now in the membership group.

Step 3. Distribute the Transaction to The Membership Group
----------------------------------------------------------

We now have an unsigned transaction body which we need our membership group to
sign.

.. code-block:: bash

   $ cc-sign -q \
      --tx-bundle-file resign/body.txbundle \
      --private-key-file example-certificates/children/child-4/child-4.private \
      --out-file resign/child-4.witbundle
   $ tx-bundle witness \
      --all \
      --tx-bundle-file resign/body.txbundle \
      --signing-key-file orchestrator.skey \
      --out-file resign/orchestrator.witbundle

Step 4. Assemble and Submit the Transaction
-------------------------------------------

Finally, we can put everything together to submit the transaction:

.. code-block:: bash

   $ tx-bundle assemble \
      --tx-bundle-file resign/body.txbundle \
      --witness-bundle-file resign/child-4.witbundle \
      --witness-bundle-file resign/orchestrator.witbundle \
      --out-file resign/tx.json
   $ cardano-cli conway transaction submit --tx-file resign/tx.json
   Transaction successfully submitted.

Step 5. Verify the Resignation On Chain
---------------------------------------

We can see the results of our resignation by querying the committee state from
the node:

.. code-block:: bash

   $ cardano-cli conway query committee-state --cold-script-hash $(cat init-cold/credential.plutus.hash)
   {
       "committee": {
           "scriptHash-533e8af4515ba4cf6035ac7087ae8978e09692fea68e9466f1683f28": {
               "expiration": 50000,
               "hotCredsAuthStatus": {
                   "contents": {
                       "dataHash": "7b7d4a28a599bbb8c08b239be2645fa82d63a848320bf4760b07d86fcf1aabdc",
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
       "epoch": 1610,
       "threshold": 0
   }

Although we are still in the committee, we can no longer authorize hot credentials nor vote - so we have effectively removed ourselves from the committee.
