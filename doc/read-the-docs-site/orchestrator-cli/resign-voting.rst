.. _resign_voting:

Resigning from the Voting Group
===================================

Members of the voting group are able to voluntarily resign their group
membership, just like delegation group members.

Step 1: Creating the assets
---------------------------

Say ``child-7`` wants to resign from the voting group. To build the
transaction assets to remove them, you can use the following commands:

.. code-block:: bash

   $ fetch-hot-nft-utxo
   $ orchestrator-cli hot-nft resign-voting \
     --utxo-file hot-nft.utxo \
     --voting-cert example-certificates/children/child-7/child-7-cert.pem \
     --out-dir resign-child-7

As before, let's see what assets were prepared:

.. code-block:: bash

   $ ls resign-child-7 -1
   datum.json
   redeemer.json
   value

We have the familiar ``datum.json``, ``redeemer.json``, and ``value`` files:

.. code-block:: bash

   $ diff <(jq '.inlineDatum' < hot-nft.utxo) <(jq '.' < resign-child-7/datum.json)
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
       "constructor": 1,
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
      --tx-in $(cardano-cli query utxo --address $(cat hot-nft/script.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-script-file hot-nft/script.plutus \
      --tx-in-inline-datum-present \
      --tx-in-redeemer-file resign-child-7/redeemer.json \
      --tx-out "$(cat resign-child-7/value)" \
      --tx-out-inline-datum-file resign-child-7/datum.json \
      --required-signer-hash $(cat example-certificates/children/child-7/child-7.keyhash) \
      --change-address $(cat orchestrator.addr) \
      --out-file resign-child-7/body.json
   Estimated transaction fee: Coin 410106

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

   $ cardano-cli conway query utxo --address $(cat hot-nft/script.addr) --output-json
   {
       "b98edd2dd6bc3d8cc8c3a73361bdcbda41a985a6b99f36a40a8e30e7303a29c5#0": {
           "address": "addr_test1wqgvl76s9anu0hnnh07unfrzkrx27k4yu0vthn85w8agwxqteatvx",
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
               "e2ab737f528cd043927496dd34e6629beb1e57ee8fe92c582cf76bd0": {
                   "": 1
               },
               "lovelace": 5000000
           }
       }
   }
