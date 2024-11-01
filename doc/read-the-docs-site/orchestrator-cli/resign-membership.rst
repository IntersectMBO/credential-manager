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
     --membership-cert example-certificates/child-1.cert \
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
   <               "bytes": "19c04196cca86fb0fbf09a35e67d55148508acafa321ebc509bc5cd6"
   <             },
   <             {
   <               "bytes": "0ab37eb812d864c903dc48ef99dd91eb71b805efe7286b0080cc1228570c5f96"
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
                       "bytes": "19c04196cca86fb0fbf09a35e67d55148508acafa321ebc509bc5cd6"
                   },
                   {
                       "bytes": "0ab37eb812d864c903dc48ef99dd91eb71b805efe7286b0080cc1228570c5f96"
                   }
               ]
           }
       ]
   }

Step 2: Create the Transaction
------------------------------

Since we are only spending an input, and not issuing a certificate or casting a vote, the transaction is somewhat easier to build.
We can also use ``cardano-cli`` instead of ``tx-bundle``, as there is only one required signer and there is no question of who will have to sign the transaction:

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
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-1.cert) \
      --change-address $(cat orchestrator.addr) \
      --out-file resign-child-1/body.json
   Estimated transaction fee: Coin 562307

Step 3. Send the Transaction to The Resignee
--------------------------------------------

To build the transaction, we need to get a signature from the resignee.
Note the use of ``--tx-body-file`` instead of ``--tx-bundle-file``, as we are signing a regular tx body, not a tx bundle.

.. code-block:: bash

   $ cc-sign -q \
      --tx-body-file resign-child-1/body.json \
      --private-key-file example-certificates/children/child-1/child-1.private \
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
       "a873157af9a562d2b9a4c0b312c5a24df7ce09e9744f7216e67bdfa127199188#0": {
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
                           },
                           {
                               "constructor": 0,
                               "fields": [
                                   {
                                       "bytes": "b381e71db0a8fcf7c6f928ad5d1c7925f8143e7bcd534208406f3325"
                                   },
                                   {
                                       "bytes": "b4d674ccc1423bc429f737b786bae5201394daa208651d25a2339ef55e5ebdf8"
                                   }
                               ]
                           }
                       ]
                   }
               ]
           },
           "inlineDatumhash": "57525734057e042349e4c130fa6e16b0c1046db7987c8c158fd46eb2de5967b7",
           "referenceScript": null,
           "value": {
               "c8aa0de384ad34d844dc479085c3ed00deb1306afb850a2cde6281f4": {
                   "": 1
               },
               "lovelace": 5000000
           }
       }
   }
