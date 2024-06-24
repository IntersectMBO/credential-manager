.. _rotate_cold:

Rotating Delegation and Membership Keys
=======================================

Though users can remove themselves, they cannot add themselves back to a group.
Neither can an existing delegation member change their own credentials.
In order to perform either of these tasks, the membership group needs to get involved in order to **rotate** the keys.
A key rotation completely replaces the current membership and delegation groups with new members.
It is subject to a few limitations:

* There must be at least one member in each group.
* The CA cannot change
* No certificates can be issued in the transaction.
* To add a new user to a group, that user must sign the transaction.

Step 1: Creating the assets
---------------------------

In this example, we are going to swap the roles of the delegation and membership groups.
We are also going to add ``child-1`` back in and add them to the delegation group.
As usual, use ``orchestrator-cli`` to prepare the transaction assets:

.. code-block:: bash

   $ fetch-cold-nft-utxo
   $ orchestrator-cli rotate-cold \
     --utxo-file cold-nft.utxo \
     --membership-cert example-certificates/children/child-4/child-4-cert.pem \
     --membership-cert example-certificates/children/child-5/child-5-cert.pem \
     --delegation-cert example-certificates/children/child-1/child-1-cert.pem \
     --delegation-cert example-certificates/children/child-2/child-2-cert.pem \
     --delegation-cert example-certificates/children/child-3/child-3-cert.pem \
     --out-dir rotate-cold
   WARNING: membership group has fewer than 3 members. This allows a single user to sign off on actions. The recommended minimum group size is 3.

As before, let's see what assets were prepared:

.. code-block:: bash

   $ ls rotate-cold -1
   datum.json
   redeemer.json
   value

We have the familiar ``datum.json``, ``redeemer.json``, and ``value`` files:

.. code-block:: bash

   $ diff <(jq 'to_entries | .[0].value.inlineDatum' < cold-nft.utxo) <(jq '.' < rotate-cold/datum.json)
   21c21
   <               "bytes": "c2233827cca3a0cc2c49f91a66276c468be994db855d6b413005fa88"
   ---
   >               "bytes": "fc6a114db76d31de585793749dcd6ad2d6c02a52ce9226820656bedd"
   24c24
   <               "bytes": "3b8536a38eea871cc8b2775deb5861ac4348ef61a84b9e9c643480ae5b88ffc3"
   ---
   >               "bytes": "7c9d1c732c313066ded1568dc24b1230cc782d331cb65465bc65ad5df6fbe832"
   32c32
   <               "bytes": "b23a02a308165c702ce00bf760a0eff33b27b12906e1805b7685125f"
   ---
   >               "bytes": "168ff0600f6245812192fb84c1d5a72129ae0445a272acc65dc88fb3"
   35c35
   <               "bytes": "fdf913abfdb8f00997cca5c14ca0b82f3d08781015a061e91444425d6f777ffa"
   ---
   >               "bytes": "c60e20be4ce0fa457a8c65ade01005475e71880e921c2ee40a6b51d42fd95e11"
   47c47
   <               "bytes": "fc6a114db76d31de585793749dcd6ad2d6c02a52ce9226820656bedd"
   ---
   >               "bytes": "ff7a6c9f3ebf80ab457cca7813842aa2150d0dad341a7956a334c76d"
   50c50
   <               "bytes": "7c9d1c732c313066ded1568dc24b1230cc782d331cb65465bc65ad5df6fbe832"
   ---
   >               "bytes": "1a82818b488574c156f1fa8941bad9b4b4976ba21cfaede1ab33a30de39f7edd"
   58c58
   <               "bytes": "168ff0600f6245812192fb84c1d5a72129ae0445a272acc65dc88fb3"
   ---
   >               "bytes": "c2233827cca3a0cc2c49f91a66276c468be994db855d6b413005fa88"
   61c61,72
   <               "bytes": "c60e20be4ce0fa457a8c65ade01005475e71880e921c2ee40a6b51d42fd95e11"
   ---
   >               "bytes": "3b8536a38eea871cc8b2775deb5861ac4348ef61a84b9e9c643480ae5b88ffc3"
   >             }
   >           ]
   >         },
   >         {
   >           "constructor": 0,
   >           "fields": [
   >             {
   >               "bytes": "b23a02a308165c702ce00bf760a0eff33b27b12906e1805b7685125f"
   >             },
   >             {
   >               "bytes": "fdf913abfdb8f00997cca5c14ca0b82f3d08781015a061e91444425d6f777ffa"


In the datum, the existing delegation and membership roles have been swapped.
The redeemer is less interesting, as it takes no arguments:

.. code-block:: bash

   cat rotate-cold/redeemer.json
   {
       "constructor": 4,
       "fields": []
   }

Step 2: Create the Transaction
------------------------------

.. code-block:: bash

   $ cardano-cli conway transaction build \
      --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in $(cardano-cli query utxo --address $(cat init-cold/nft.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-script-file init-cold/nft.plutus \
      --tx-in-inline-datum-present \
      --tx-in-redeemer-file rotate-cold/redeemer.json \
      --tx-out "$(cat rotate-cold/value)" \
      --tx-out-inline-datum-file rotate-cold/datum.json \
      --required-signer-hash $(cat example-certificates/children/child-1/child-1.keyhash) \
      --required-signer-hash $(cat example-certificates/children/child-2/child-2.keyhash) \
      --required-signer-hash $(cat example-certificates/children/child-3/child-3.keyhash) \
      --required-signer-hash $(cat example-certificates/children/child-4/child-4.keyhash) \
      --required-signer-hash $(cat example-certificates/children/child-5/child-5.keyhash) \
      --change-address $(cat orchestrator.addr) \
      --out-file rotate-cold/body.json
   Estimated transaction fee: Coin 609554

Once again, we need signatures from multiple users.
To authorize the ``rotate-cold`` action, the transaction must be signed by a majority of the (current) membership group.
In addition to this, any new users in a group must sign the transaction (i.e. users who weren't present in the previous group).
Because we are swapping all the keys, all the users are new in both groups, so everyone needs to sign the transaction.

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
      --signing-key-file example-certificates/children/child-3/child-3.skey \
      --out-file rotate-cold/child-3.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file rotate-cold/body.json \
      --signing-key-file example-certificates/children/child-4/child-4.skey \
      --out-file rotate-cold/child-4.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file rotate-cold/body.json \
      --signing-key-file example-certificates/children/child-5/child-5.skey \
      --out-file rotate-cold/child-5.witness
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
      --witness-file rotate-cold/child-3.witness \
      --witness-file rotate-cold/child-4.witness \
      --witness-file rotate-cold/child-5.witness \
      --witness-file rotate-cold/orchestrator.witness \
      --out-file rotate-cold/tx.json
   $ cardano-cli conway transaction submit --tx-file rotate-cold/tx.json
   Transaction successfully submitted.

Step 5. Verify the change on chain
----------------------------------

.. code-block:: bash

   $ cardano-cli conway query utxo --address $(cat init-cold/nft.addr) --output-json
   {
       "462cdb7a7e73b4084bf97093cff8271fb4222abf37296353b22684d5fbafe426#0": {
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
