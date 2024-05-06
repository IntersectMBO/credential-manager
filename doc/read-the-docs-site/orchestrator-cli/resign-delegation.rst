.. _resign_delegation:

Resigning from the Delegation Group
===================================

Members of the delegation group are able to voluntarily resign their group
membership.

Step 1: Creating the assets
---------------------------

Say ``child-6`` wants to resign from the delegation group. To build the
transaction assets to remove them, you can use the following commands:

.. code-block:: bash

   $ cardano-cli conway query utxo --address $(cat cold-nft/script.addr) --output-json \
     | jq 'to_entries | .[0].value' \
     > cold-nft.utxo
   $ orchestrator-cli cold-nft resign-delegation \
     --utxo-file cold-nft.utxo \
     --delegation-cert example-certificates/children/child-6/child-6-cert.pem \
     --out-dir resign-child-6

As before, let's see what assets were prepared:

.. code-block:: bash

   $ ls resign-child-6 -1
   datum.json
   redeemer.json
   value

We have the familiar ``datum.json``, ``redeemer.json``, and ``value`` files:

.. code-block:: bash

   $ diff <(jq '.inlineDatum' < cold-nft.utxo) <(jq '.' < resign-child-6/datum.json)
   75,85d74
   <         },
   <         {
   <           "constructor": 0,
   <           "fields": [
   <             {
   <               "bytes": "c530a8b72dd72e320e7f4883fcb98d0058e70efcf4e7e0871ce13eb7"
   <             },
   <             {
   <               "bytes": "ce75748d37a55ef1faec7219708059479197965a5927a7f9901c6bc9707eeaa1"
   <             }
   <           ]

In the datum, ``child-6`` has been removed, while the redeemer says to remove
this user.


.. code-block:: bash

   cat resign-child-6/redeemer.json
   {
       "constructor": 2,
       "fields": [
           {
               "constructor": 0,
               "fields": [
                   {
                       "bytes": "c530a8b72dd72e320e7f4883fcb98d0058e70efcf4e7e0871ce13eb7"
                   },
                   {
                       "bytes": "ce75748d37a55ef1faec7219708059479197965a5927a7f9901c6bc9707eeaa1"
                   }
               ]
           }
       ]
   }

.. code-block:: bash

   cat resign-child-6/value
   addr_test1wzq7m97vwjyu43w8snutpnuukhef8nlh7lpx4x53rquzanqmwuk6y+5000000 lovelace + 1 14987a29cf4065e7b38a4cde6bc84b067492ad3ecc8223598a8fe4be

Step 2: Create the Transaction
------------------------------

Since we are only spending an input, and not issuing a certificate or casting a
vote, the transaction is somewhat easier to build:

.. code-block:: bash

   $ cardano-cli conway transaction build \
      --tx-in $(cardano-cli query utxo --address $(cat orchestrator.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-collateral $(cardano-cli query utxo --address $(cat orchestrator.addr) --output-json | jq -r 'keys[0]') \
      --tx-in $(cardano-cli query utxo --address $(cat cold-nft/script.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-script-file cold-nft/script.plutus \
      --tx-in-inline-datum-present \
      --tx-in-redeemer-file resign-child-6/redeemer.json \
      --tx-out "$(cat resign-child-6/value)" \
      --tx-out-inline-datum-file resign-child-6/datum.json \
      --required-signer-hash $(cat example-certificates/children/child-6/child-6.keyhash) \
      --change-address $(cat orchestrator.addr) \
      --out-file resign-child-6.body
   Estimated transaction fee: Coin 442451

The only notable thing about this command compared with previous ones is that
there is only one ``required-signer-hash``. The transaction must be signed by
the resignee.

Step 3. Send the Transaction to The Resignee
--------------------------------------------

To build the transaction, we need to get a signature from the resignee.

.. code-block:: bash

   $ cardano-cli conway transaction witness \
      --tx-body-file resign-child-6.body \
      --signing-key-file example-certificates/children/child-6/child-6.skey \
      --out-file resign-child-6.child-6.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file resign-child-6.body \
      --signing-key-file orchestrator.skey \
      --out-file resign-child-6.orchestrator.witness

Step 4. Assemble and Submit the Transaction
-------------------------------------------

.. code-block:: bash

   $ cardano-cli conway transaction assemble \
      --tx-body-file resign-child-6.body \
      --witness-file resign-child-6.child-6.witness \
      --witness-file resign-child-6.orchestrator.witness \
      --out-file resign-child-6.tx
   $ cardano-cli conway transaction submit --tx-file resign-child-6.tx
   Transaction successfully submitted.

Step 5. Verify the delegation member is removed
-----------------------------------------------

.. code-block:: bash

   $ cardano-cli conway query utxo --address $(cat cold-nft/script.addr) --output-json
   {
       "f496a2dec01ee0ea788c91c80f8a936583e4b96057497169a007cc5dca987ab9#0": {
           "address": "addr_test1wzq7m97vwjyu43w8snutpnuukhef8nlh7lpx4x53rquzanqmwuk6y",
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
                                       "bytes": "ff7a6c9f3ebf80ab457cca7813842aa2150d0dad341a7956a334c76d"
                                   },
                                   {
                                       "bytes": "1a82818b488574c156f1fa8941bad9b4b4976ba21cfaede1ab33a30de39f7edd"
                                   }
                               ]
                           },
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
           "inlineDatumhash": "de77049711cf2b1401a6a5a75b8e92898dff36ad5d9089c79bb4b1f88328acac",
           "referenceScript": null,
           "value": {
               "14987a29cf4065e7b38a4cde6bc84b067492ad3ecc8223598a8fe4be": {
                   "": 1
               },
               "lovelace": 5000000
           }
       }
   }
