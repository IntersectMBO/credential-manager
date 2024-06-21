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
     --voting-cert example-certificates/children/child-7/child-7-cert.pem \
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
   <           "bytes": "fb5e0be4801aea73135efe43f4a3a6d08147af523112986dd5e7d13b"
   <         },
   <         {
   <           "bytes": "57f5530e057e20b726b78aa31104d415cb2bce58c669829a44d009c1b1005bcd"
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
                       "bytes": "fb5e0be4801aea73135efe43f4a3a6d08147af523112986dd5e7d13b"
                   },
                   {
                       "bytes": "57f5530e057e20b726b78aa31104d415cb2bce58c669829a44d009c1b1005bcd"
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
      --required-signer-hash $(cat example-certificates/children/child-7/child-7.keyhash) \
      --change-address $(cat orchestrator.addr) \
      --out-file resign-child-7/body.json
   Estimated transaction fee: Coin 486785

Step 3. Send the Transaction to The Resignee
--------------------------------------------

To build the transaction, we need to get a signature from the resignee.

.. code-block:: bash

   $ cardano-cli conway transaction witness \
      --tx-body-file resign-child-7/body.json \
      --signing-key-file example-certificates/children/child-7/child-7.skey \
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
       "86cb189a24ba03931d3879e16172ec4ad6a14e9ac3c7a8d5b69c4bb272020af4#0": {
           "address": "addr_test1wp62yszlxmqvy9xecnevt63sr4escyh55nz95s8zh2x6mdq8dx74g",
           "datum": null,
           "inlineDatum": {
               "list": [
                   {
                       "constructor": 0,
                       "fields": [
                           {
                               "bytes": "a3c6cb93a32b02877c61f64ab1c66c4513f12788bf7c500ead7d941b"
                           },
                           {
                               "bytes": "9923f31c1ce14e2acbd505fa8eebd4ce677d1bcd96c6d71610f810f2008ecc3a"
                           }
                       ]
                   },
                   {
                       "constructor": 0,
                       "fields": [
                           {
                               "bytes": "eda6befbe1a4cb8191752d97b67627a548bcc5f3e4653ecfdba7cdf0"
                           },
                           {
                               "bytes": "ecd64beefcf59f01a975457b0a3623d2b03d5bcf71642a8d8d8275e4668aad31"
                           }
                       ]
                   }
               ]
           },
           "inlineDatumhash": "20003d8b8a9526ab5daf6f9e31ef5f0ac8cfb97832d85492e49c8e1456424ade",
           "referenceScript": null,
           "value": {
               "76edba602a94ee8d0e81a59ff6470bc490cb1649066e0678143b4bf3": {
                   "5c94bfec2d9e8a0e0c536df3384e5001adf6d333216a2b546b6f043a2301": 1
               },
               "lovelace": 5000000
           }
       }
   }
