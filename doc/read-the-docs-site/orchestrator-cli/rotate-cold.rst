.. _rotate_cold:

Rotating Delegation and Membership Keys
=======================================

Though delegates can remove themselves, they cannot add themselves back to the
delegation group. Neither can an existing delegation group member change their
own credentials. In order to perform either of these tasks, the membership
group needs to get involved in order to **rotate** the keys. A key rotation
completely replaces the current membership and delegation groups with new
members. It is subject to a few limitations:

* There must be at least one member in each group.
* The CA cannot change
* No certificates can be issued in the transaction.

Step 1: Creating the assets
---------------------------

In this example, we are going to swap the roles of the delegation and
membership groups. As usual, use ``orchestrator-cli`` to prepare the
transaction assets:

.. code-block:: bash

   $ fetch-cold-nft-utxo
   $ orchestrator-cli cold-nft rotate \
     --utxo-file cold-nft.utxo \
     --membership-cert example-certificates/children/child-4/child-4-cert.pem \
     --membership-cert example-certificates/children/child-5/child-5-cert.pem \
     --delegation-cert example-certificates/children/child-1/child-1-cert.pem \
     --delegation-cert example-certificates/children/child-2/child-2-cert.pem \
     --delegation-cert example-certificates/children/child-3/child-3-cert.pem \
     --out-dir rotate-cold

As before, let's see what assets were prepared:

.. code-block:: bash

   $ ls rotate-cold -1
   datum.json
   redeemer.json
   value

We have the familiar ``datum.json``, ``redeemer.json``, and ``value`` files:

.. code-block:: bash

   $ diff <(jq '.inlineDatum' < cold-nft.utxo) <(jq '.' < rotate-cold/datum.json)
   21c21
   <               "bytes": "ff7a6c9f3ebf80ab457cca7813842aa2150d0dad341a7956a334c76d"
   ---
   >               "bytes": "fc6a114db76d31de585793749dcd6ad2d6c02a52ce9226820656bedd"
   24c24
   <               "bytes": "1a82818b488574c156f1fa8941bad9b4b4976ba21cfaede1ab33a30de39f7edd"
   ---
   >               "bytes": "7c9d1c732c313066ded1568dc24b1230cc782d331cb65465bc65ad5df6fbe832"
   32c32
   <               "bytes": "c2233827cca3a0cc2c49f91a66276c468be994db855d6b413005fa88"
   ---
   >               "bytes": "168ff0600f6245812192fb84c1d5a72129ae0445a272acc65dc88fb3"
   35c35
   <               "bytes": "3b8536a38eea871cc8b2775deb5861ac4348ef61a84b9e9c643480ae5b88ffc3"
   ---
   >               "bytes": "c60e20be4ce0fa457a8c65ade01005475e71880e921c2ee40a6b51d42fd95e11"
   38c38,42
   <         },
   ---
   >         }
   >       ]
   >     },
   >     {
   >       "list": [
   43c47
   <               "bytes": "b23a02a308165c702ce00bf760a0eff33b27b12906e1805b7685125f"
   ---
   >               "bytes": "ff7a6c9f3ebf80ab457cca7813842aa2150d0dad341a7956a334c76d"
   46c50
   <               "bytes": "fdf913abfdb8f00997cca5c14ca0b82f3d08781015a061e91444425d6f777ffa"
   ---
   >               "bytes": "1a82818b488574c156f1fa8941bad9b4b4976ba21cfaede1ab33a30de39f7edd"
   49,53c53
   <         }
   <       ]
   <     },
   <     {
   <       "list": [
   ---
   >         },
   58c58
   <               "bytes": "fc6a114db76d31de585793749dcd6ad2d6c02a52ce9226820656bedd"
   ---
   >               "bytes": "c2233827cca3a0cc2c49f91a66276c468be994db855d6b413005fa88"
   61c61
   <               "bytes": "7c9d1c732c313066ded1568dc24b1230cc782d331cb65465bc65ad5df6fbe832"
   ---
   >               "bytes": "3b8536a38eea871cc8b2775deb5861ac4348ef61a84b9e9c643480ae5b88ffc3"
   69c69
   <               "bytes": "168ff0600f6245812192fb84c1d5a72129ae0445a272acc65dc88fb3"
   ---
   >               "bytes": "b23a02a308165c702ce00bf760a0eff33b27b12906e1805b7685125f"
   72c72
   <               "bytes": "c60e20be4ce0fa457a8c65ade01005475e71880e921c2ee40a6b51d42fd95e11"
   ---
   >               "bytes": "fdf913abfdb8f00997cca5c14ca0b82f3d08781015a061e91444425d6f777ffa"


In the datum, the existing delegation and membership roles have been swapped.
The redeemer is less interesting, as it takes no arguments:

.. code-block:: bash

   cat rotate-cold/redeemer.json
   {
       "constructor": 3,
       "fields": []
   }

Step 2: Create the Transaction
------------------------------

.. code-block:: bash

   $ cardano-cli conway transaction build \
      --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in $(cardano-cli query utxo --address $(cat cold-nft/script.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-script-file cold-nft/script.plutus \
      --tx-in-inline-datum-present \
      --tx-in-redeemer-file rotate-cold/redeemer.json \
      --tx-out "$(cat rotate-cold/value)" \
      --tx-out-inline-datum-file rotate-cold/datum.json \
      --required-signer-hash $(cat example-certificates/children/child-1/child-1.keyhash) \
      --required-signer-hash $(cat example-certificates/children/child-2/child-2.keyhash) \
      --change-address $(cat orchestrator.addr) \
      --out-file rotate-cold/body.json
   Estimated transaction fee: Coin 453188

Once again, we need signatures from multiple users. To authorize the ``rotate``
action, the transaction must be signed by a majority of the (current)
membership group.

Step 3. Distribute the Transaction to The Membership Group
----------------------------------------------------------

.. code-block:: bash

   $ cardano-cli conway transaction witness \
      --tx-body-file rotate-cold/body.json \
      --signing-key-file example-certificates/children/child-1/child-1.skey \
      --out-file rotate-cold/child-1.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file rotate-cold/body.json \
      --signing-key-file example-certificates/children/child-2/child-2.skey \
      --out-file rotate-cold/child-2.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file rotate-cold/body.json \
      --signing-key-file orchestrator.skey \
      --out-file rotate-cold/orchestrator.witness

Step 4. Assemble and Submit the Transaction
-------------------------------------------

.. code-block:: bash

   $ cardano-cli conway transaction assemble \
      --tx-body-file rotate-cold/body.json \
      --witness-file rotate-cold/child-1.witness \
      --witness-file rotate-cold/child-2.witness \
      --witness-file rotate-cold/orchestrator.witness \
      --out-file rotate-cold/tx.json
   $ cardano-cli conway transaction submit --tx-file rotate-cold/tx.json
   Transaction successfully submitted.

Step 5. Verify the change on chain
----------------------------------

.. code-block:: bash

   $ cardano-cli conway query utxo --address $(cat cold-nft/script.addr) --output-json
   {
       "3ed22ece44abca5368e8d90ca4b16425491d381cbf1c396ed3f4f538b89cf694": {
           "address": "addr_test1wzng9lxemp4skvd9zs3zp4eslldxsg87afcvtm22qm07saqew8uzk",
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
                   }
               ]
           },
           "inlineDatumhash": "50841fe8863d612edd1c29eaceb68fdc5c8016580c509b5e1ff2636b23dc3aec",
           "referenceScript": null,
           "value": {
               "c8aa0de384ad34d844dc479085c3ed00deb1306afb850a2cde6281f4": {
                   "": 1
               },
               "lovelace": 5000000
           }
       }
   }
