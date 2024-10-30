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
     --membership-cert example-certificates/child-4.cert \
     --membership-cert example-certificates/child-5.cert \
     --delegation-cert example-certificates/child-1.cert \
     --delegation-cert example-certificates/child-2.cert \
     --delegation-cert example-certificates/child-3.cert \
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
   <               "bytes": "2b3d02d77ee80b219ca1a20cd3f82b95ff23eb28ca4e886ce3cc039d"
   ---
   >               "bytes": "7c4ce0c3eca1b077d8465cf3b44db18beea87bacf55c05c9b4d0317c"
   24c24
   <               "bytes": "03452838656348992c11f383a3b17f520a2603ab5659d6c77ea650a1675610f4"
   ---
   >               "bytes": "4e42c90371daf9c4a030bd7d161e44364c49f7f94ffe3daaf5843032ffd1c207"
   32c32
   <               "bytes": "95bebd09ef4d125a595ae0bf5f15724731a7537b5fda32927bc7b366"
   ---
   >               "bytes": "a263b5a55cb7b8728a0a97092fad7054117f7695897990bc1ab499b4"
   35c35
   <               "bytes": "c2367d7b1d649be1847bf2224bb33ce7252bc7cfa73bf740ea589b741ee70e0d"
   ---
   >               "bytes": "521a9f8bbf35f0b228b686657e67a1b168e10eb20fb92a0d3203221a5bd6db88"
   47c47
   <               "bytes": "7c4ce0c3eca1b077d8465cf3b44db18beea87bacf55c05c9b4d0317c"
   ---
   >               "bytes": "19c04196cca86fb0fbf09a35e67d55148508acafa321ebc509bc5cd6"
   50c50
   <               "bytes": "4e42c90371daf9c4a030bd7d161e44364c49f7f94ffe3daaf5843032ffd1c207"
   ---
   >               "bytes": "0ab37eb812d864c903dc48ef99dd91eb71b805efe7286b0080cc1228570c5f96"
   58c58
   <               "bytes": "a263b5a55cb7b8728a0a97092fad7054117f7695897990bc1ab499b4"
   ---
   >               "bytes": "2b3d02d77ee80b219ca1a20cd3f82b95ff23eb28ca4e886ce3cc039d"
   61c61,72
   <               "bytes": "521a9f8bbf35f0b228b686657e67a1b168e10eb20fb92a0d3203221a5bd6db88"
   ---
   >               "bytes": "03452838656348992c11f383a3b17f520a2603ab5659d6c77ea650a1675610f4"
   >             }
   >           ]
   >         },
   >         {
   >           "constructor": 0,
   >           "fields": [
   >             {
   >               "bytes": "95bebd09ef4d125a595ae0bf5f15724731a7537b5fda32927bc7b366"
   >             },
   >             {
   >               "bytes": "c2367d7b1d649be1847bf2224bb33ce7252bc7cfa73bf740ea589b741ee70e0d"

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
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-1.cert) \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-2.cert) \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-3.cert) \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-4.cert) \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-5.cert) \
      --change-address $(cat orchestrator.addr) \
      --out-file rotate-cold/body.json
   Estimated transaction fee: Coin 609554

Once again, we need signatures from multiple users.
To authorize the ``rotate-cold`` action, the transaction must be signed by a majority of the (current) membership group.
In addition to this, any new users in a group must sign the transaction (i.e. users who weren't present in the previous group).
Because we are swapping all the keys, all the users are new in both groups, so everyone needs to sign the transaction, and there is no need to use ``tx-bundle``.

Step 3. Distribute the Transaction to The Membership Group
----------------------------------------------------------

.. code-block:: bash

   $ cc-sign -q \
      --tx-body-file rotate-cold/body.json \
      --private-key-file example-certificates/children/child-1/child-1.private \
      --out-file rotate-cold/child-1.witness
   $ cc-sign -q \
      --tx-body-file rotate-cold/body.json \
      --private-key-file example-certificates/children/child-2/child-2.private \
      --out-file rotate-cold/child-2.witness
   $ cc-sign -q \
      --tx-body-file rotate-cold/body.json \
      --private-key-file example-certificates/children/child-3/child-3.private \
      --out-file rotate-cold/child-3.witness
   $ cc-sign -q \
      --tx-body-file rotate-cold/body.json \
      --private-key-file example-certificates/children/child-4/child-4.private \
      --out-file rotate-cold/child-4.witness
   $ cc-sign -q \
      --tx-body-file rotate-cold/body.json \
      --private-key-file example-certificates/children/child-5/child-5.private \
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
       "6179fe944cb70506d0d17ac00553a712036150affc61bb5000f29f395b3f5848#0": {
           "address": "addr_test1wrd2665l5depddaeg9cke7w58de9tc0q0x03recs9cm9deqfkxg0v",
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
                                       "bytes": "7c4ce0c3eca1b077d8465cf3b44db18beea87bacf55c05c9b4d0317c"
                                   },
                                   {
                                       "bytes": "4e42c90371daf9c4a030bd7d161e44364c49f7f94ffe3daaf5843032ffd1c207"
                                   }
                               ]
                           },
                           {
                               "constructor": 0,
                               "fields": [
                                   {
                                       "bytes": "a263b5a55cb7b8728a0a97092fad7054117f7695897990bc1ab499b4"
                                   },
                                   {
                                       "bytes": "521a9f8bbf35f0b228b686657e67a1b168e10eb20fb92a0d3203221a5bd6db88"
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
                                       "bytes": "19c04196cca86fb0fbf09a35e67d55148508acafa321ebc509bc5cd6"
                                   },
                                   {
                                       "bytes": "0ab37eb812d864c903dc48ef99dd91eb71b805efe7286b0080cc1228570c5f96"
                                   }
                               ]
                           },
                           {
                               "constructor": 0,
                               "fields": [
                                   {
                                       "bytes": "2b3d02d77ee80b219ca1a20cd3f82b95ff23eb28ca4e886ce3cc039d"
                                   },
                                   {
                                       "bytes": "03452838656348992c11f383a3b17f520a2603ab5659d6c77ea650a1675610f4"
                                   }
                               ]
                           },
                           {
                               "constructor": 0,
                               "fields": [
                                   {
                                       "bytes": "95bebd09ef4d125a595ae0bf5f15724731a7537b5fda32927bc7b366"
                                   },
                                   {
                                       "bytes": "c2367d7b1d649be1847bf2224bb33ce7252bc7cfa73bf740ea589b741ee70e0d"
                                   }
                               ]
                           }
                       ]
                   }
               ]
           },
           "inlineDatumhash": "fcaf84f8b6ca0b0b3f4dfe5fedf83138ed91a4009cd322f09232af26dc73959f",
           "referenceScript": null,
           "value": {
               "c8aa0de384ad34d844dc479085c3ed00deb1306afb850a2cde6281f4": {
                   "": 1
               },
               "lovelace": 5000000
           }
       }
   }
