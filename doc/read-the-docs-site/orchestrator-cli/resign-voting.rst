.. _resign_voting:

Resigning from the Voting Group
===================================

Members of the voting group are able to voluntarily resign their group membership.

Step 1: Creating the assets
---------------------------

Say ``child-7`` wants to resign from the voting group. To build the
transaction assets to remove them, you can use the following commands:

.. code-block:: bash

   $ fetch-hot-nft-utxo
   $ orchestrator-cli resign-voting \
     --utxo-file hot-nft.utxo \
     --voting-cert example-certificates/child-7.cert \
     --out-dir resign-child-7
   WARNING: delegation group has fewer than 3 members. This allows a single user to sign off on actions. The recommended minimum group size is 3.

As before, let's see what assets were prepared:

.. code-block:: bash

   $ ls resign-child-7 -1
   datum.json
   redeemer.json
   value

We have the familiar ``datum.json``, ``redeemer.json``, and ``value`` files:

.. code-block:: bash

   $ diff <(jq 'to_entries | .[0].value.inlineDatum' < hot-nft.utxo) <(jq '.' < resign-child-7/datum.json)
   7,17d6
   <           "bytes": "c6731b9c6de6bf11d91f08099953cb393505806ff522e5cc3a7574ab"
   <         },
   <         {
   <           "bytes": "e50384c655f9a33cabf64e41df7282e765a242aef182130f1db01bce8859e0aa"
   <         }
   <       ]
   <     },
   <     {
   <       "constructor": 0,
   <       "fields": [
   <         {

In the datum, ``child-7`` has been removed, while the redeemer says to remove
this user.


.. code-block:: bash

   $ cat resign-child-7/redeemer.json
   {
       "constructor": 8,
       "fields": [
           {
               "constructor": 0,
               "fields": [
                   {
                       "bytes": "c6731b9c6de6bf11d91f08099953cb393505806ff522e5cc3a7574ab"
                   },
                   {
                       "bytes": "e50384c655f9a33cabf64e41df7282e765a242aef182130f1db01bce8859e0aa"
                   }
               ]
           }
       ]
   }

Step 2: Create the Transaction
------------------------------

.. code-block:: bash

   $ cardano-cli conway transaction build \
      --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in $(cardano-cli query utxo --address $(cat init-hot/nft.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-script-file init-hot/nft.plutus \
      --tx-in-inline-datum-present \
      --tx-in-redeemer-file resign-child-7/redeemer.json \
      --tx-out "$(cat resign-child-7/value)" \
      --tx-out-inline-datum-file resign-child-7/datum.json \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-7.cert) \
      --change-address $(cat orchestrator.addr) \
      --out-file resign-child-7/body.json
   Estimated transaction fee: Coin 486785

Step 3. Send the Transaction to The Resignee
--------------------------------------------

To build the transaction, we need to get a signature from the resignee.

.. code-block:: bash

   $ cc-sign -q \
      --tx-body-file resign-child-7/body.json \
      --private-key-file example-certificates/children/child-7/child-7.private \
      --out-file resign-child-7/child-7.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file resign-child-7/body.json \
      --signing-key-file orchestrator.skey \
      --out-file resign-child-7/orchestrator.witness

Step 4. Assemble and Submit the Transaction
-------------------------------------------

.. code-block:: bash

   $ cardano-cli conway transaction assemble \
      --tx-body-file resign-child-7/body.json \
      --witness-file resign-child-7/child-7.witness \
      --witness-file resign-child-7/orchestrator.witness \
      --out-file resign-child-7/tx.json
   $ cardano-cli conway transaction submit --tx-file resign-child-7/tx.json
   Transaction successfully submitted.

Step 5. Verify the voting member is removed
-----------------------------------------------

.. code-block:: bash

   $ cardano-cli conway query utxo --address $(cat init-hot/nft.addr) --output-json
   {
       "4532d145dc5d417950879d9316657ffe36a716e512c1f1d679b5a92b6a33d62f#0": {
           "address": "addr_test1wzn8zkvkvaex4nnvften2aejpgt3calqwmgmrzwj95vukcs0map8t",
           "datum": null,
           "inlineDatum": {
               "list": [
                   {
                       "constructor": 0,
                       "fields": [
                           {
                               "bytes": "c6d6ffd8e93b1b8352c297d528c958b982098dc8a08025bbb8d864cf"
                           },
                           {
                               "bytes": "e3340359f5d25c051e4dd160e4cb4d75074c537905f07eb9a2e24db881246ee0"
                           }
                       ]
                   },
                   {
                       "constructor": 0,
                       "fields": [
                           {
                               "bytes": "2faaa04cee79d9abfa3149c814617e860567a8609bbfbd044566a5cd"
                           },
                           {
                               "bytes": "ae8eef56d67350b247ab77be48dad121ae18d473386f59b3fda9fccbd665422a"
                           }
                       ]
                   }
               ]
           },
           "inlineDatumhash": "a01fa0cf5747346e4eac82a18e4acb1bcfda06bbb088823bdb6fe03c546536d7",
           "referenceScript": null,
           "value": {
               "bf3bbf5a8539663eddd53364a9fd90e468c0182fcf6f0642ac16d65f": {
                   "93fdf1b28aefd28ed13b268653c03dd86872063d58434a2c83d68e6c2301": 1
               },
               "lovelace": 5000000
           }
       }
   }
