.. _resign_membership:

Resigning from the Membership Group
===================================

Members of the membership group are able to voluntarily resign their group membership.

Step 1: Creating the assets
---------------------------

Say ``child-1`` wants to resign from the membership group. To build the
transaction assets to remove them, you can use the following commands:

.. code-block:: bash

   $ fetch-cold-nft-utxo
   $ orchestrator-cli resign-membership \
     --utxo-file cold-nft.utxo \
     --membership-cert example-certificates/children/child-1/child-1-cert.pem \
     --out-dir resign-child-1
   WARNING: membership group has fewer than 3 members. This allows a single user to sign off on actions. The recommended minimum group size is 3.

We see the tool issue a warning that a 2-member group size allows one person to act unilaterally.
In a real world situation, you would want to avoid these situations, but we can ignore it for now.

As before, let's see what assets were prepared:

.. code-block:: bash

   $ ls resign-child-1 -1
   datum.json
   redeemer.json
   value

We have the familiar ``datum.json``, ``redeemer.json``, and ``value`` files:

.. code-block:: bash

   $ diff <(jq 'to_entries | .[0].value.inlineDatum' < cold-nft.utxo) <(jq '.' < resign-child-1/datum.json)
   21,31d20
   <               "bytes": "ff7a6c9f3ebf80ab457cca7813842aa2150d0dad341a7956a334c76d"
   <             },
   <             {
   <               "bytes": "1a82818b488574c156f1fa8941bad9b4b4976ba21cfaede1ab33a30de39f7edd"
   <             }
   <           ]
   <         },
   <         {
   <           "constructor": 0,
   <           "fields": [
   <             {

In the datum, ``child-1`` has been removed, while the redeemer says to remove
this user.


.. code-block:: bash

   cat resign-child-1/redeemer.json
   {
       "constructor": 3,
       "fields": [
           {
               "constructor": 0,
               "fields": [
                   {
                       "bytes": "ff7a6c9f3ebf80ab457cca7813842aa2150d0dad341a7956a334c76d"
                   },
                   {
                       "bytes": "1a82818b488574c156f1fa8941bad9b4b4976ba21cfaede1ab33a30de39f7edd"
                   }
               ]
           }
       ]
   }

Step 2: Create the Transaction
------------------------------

Since we are only spending an input, and not issuing a certificate or casting a
vote, the transaction is somewhat easier to build:

.. code-block:: bash

   $ cardano-cli conway transaction build \
      --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in $(cardano-cli query utxo --address $(cat init-cold/nft.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-script-file init-cold/nft.plutus \
      --tx-in-inline-datum-present \
      --tx-in-redeemer-file resign-child-1/redeemer.json \
      --tx-out "$(cat resign-child-1/value)" \
      --tx-out-inline-datum-file resign-child-1/datum.json \
      --required-signer-hash $(cat example-certificates/children/child-1/child-1.keyhash) \
      --change-address $(cat orchestrator.addr) \
      --out-file resign-child-1/body.json
   Estimated transaction fee: Coin 562307

Step 3. Send the Transaction to The Resignee
--------------------------------------------

To build the transaction, we need to get a signature from the resignee.

.. code-block:: bash

   $ cardano-cli conway transaction witness \
      --tx-body-file resign-child-1/body.json \
      --signing-key-file example-certificates/children/child-1/child-1.skey \
      --out-file resign-child-1/child-1.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file resign-child-1/body.json \
      --signing-key-file orchestrator.skey \
      --out-file resign-child-1/orchestrator.witness

Step 4. Assemble and Submit the Transaction
-------------------------------------------

.. code-block:: bash

   $ cardano-cli conway transaction assemble \
      --tx-body-file resign-child-1/body.json \
      --witness-file resign-child-1/child-1.witness \
      --witness-file resign-child-1/orchestrator.witness \
      --out-file resign-child-1/tx.json
   $ cardano-cli conway transaction submit --tx-file resign-child-1/tx.json
   Transaction successfully submitted.

Step 5. Verify the membership member is removed
-----------------------------------------------

.. code-block:: bash

   $ cardano-cli conway query utxo --address $(cat init-cold/nft.addr) --output-json
   {
       "880167c58ca05f2acab09bb3de42274380befe0eec6f6b20c52d3ae9ded18e5d#0": {
           "address": "addr_test1wpy9h326p4caud25k8qs665ts97uht7pmvlm8hd2d84vsxqjudz4q",
           "datum": null,
           "inlineDatum": {
               "constructor": 0,
               "fields": [
                   {
                       "constructor": 0,
                       "fields": [
                           {
                               "bytes": "09159adec41ce5d48dde24a275a5b2c2e79461c8693ef60af9fc3207"
                           },
                           {
                               "bytes": "0ff1fd44947bcd4cdc6f06841d881ac2a0beb3f15ba5f5e3c08991d92e8ba643"
                           }
                       ]
                   },
                   {
                       "list": [
                           {
                               "constructor": 0,
                               "fields": [
                                   {
                                       "bytes": "c2233827cca3a0cc2c49f91a66276c468be994db855d6b413005fa88"
                                   },
                                   {
                                       "bytes": "3b8536a38eea871cc8b2775deb5861ac4348ef61a84b9e9c643480ae5b88ffc3"
                                   }
                               ]
                           },
                           {
                               "constructor": 0,
                               "fields": [
                                   {
                                       "bytes": "b23a02a308165c702ce00bf760a0eff33b27b12906e1805b7685125f"
                                   },
                                   {
                                       "bytes": "fdf913abfdb8f00997cca5c14ca0b82f3d08781015a061e91444425d6f777ffa"
                                   }
                               ]
                           }
                       ]
                   },
                   {
                       "list": [
                           {
                               "constructor": 0,
                               "fields": [
                                   {
                                       "bytes": "fc6a114db76d31de585793749dcd6ad2d6c02a52ce9226820656bedd"
                                   },
                                   {
                                       "bytes": "7c9d1c732c313066ded1568dc24b1230cc782d331cb65465bc65ad5df6fbe832"
                                   }
                               ]
                           },
                           {
                               "constructor": 0,
                               "fields": [
                                   {
                                       "bytes": "168ff0600f6245812192fb84c1d5a72129ae0445a272acc65dc88fb3"
                                   },
                                   {
                                       "bytes": "c60e20be4ce0fa457a8c65ade01005475e71880e921c2ee40a6b51d42fd95e11"
                                   }
                               ]
                           }
                       ]
                   }
               ]
           },
           "inlineDatumhash": "d4635e4d07c21ca9caa709ce7338cdd6d6772928855dcb95f1aabc4e84e63bff",
           "referenceScript": null,
           "value": {
               "c8aa0de384ad34d844dc479085c3ed00deb1306afb850a2cde6281f4": {
                   "": 1
               },
               "lovelace": 5000000
           }
       }
   }
