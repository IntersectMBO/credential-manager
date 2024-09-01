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

   $ fetch-cold-nft-utxo
   $ orchestrator-cli resign-delegation \
     --utxo-file cold-nft.utxo \
     --delegation-cert example-certificates/child-6.cert \
     --out-dir resign-child-6
   WARNING: delegation group has fewer than 3 members. This allows a single user to sign off on actions. The recommended minimum group size is 3.

We see the tool issue a warning that a 2-member group size allows one person to act unilaterally.
In a real world situation, you would want to avoid these situations, but we can ignore it for now.

As before, let's see what assets were prepared:

.. code-block:: bash

   $ ls resign-child-6 -1
   datum.json
   redeemer.json
   value

We have the familiar ``datum.json``, ``redeemer.json``, and ``value`` files:

.. code-block:: bash

   $ diff <(jq 'to_entries | .[0].value.inlineDatum' < cold-nft.utxo) <(jq '.' < resign-child-6/datum.json)
   64,74d63
   <         },
   <         {
   <           "constructor": 0,
   <           "fields": [
   <             {
   <               "bytes": "b381e71db0a8fcf7c6f928ad5d1c7925f8143e7bcd534208406f3325"
   <             },
   <             {
   <               "bytes": "b4d674ccc1423bc429f737b786bae5201394daa208651d25a2339ef55e5ebdf8"
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
                       "bytes": "b381e71db0a8fcf7c6f928ad5d1c7925f8143e7bcd534208406f3325"
                   },
                   {
                       "bytes": "b4d674ccc1423bc429f737b786bae5201394daa208651d25a2339ef55e5ebdf8"
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
      --tx-in-redeemer-file resign-child-6/redeemer.json \
      --tx-out "$(cat resign-child-6/value)" \
      --tx-out-inline-datum-file resign-child-6/datum.json \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-6.cert) \
      --change-address $(cat orchestrator.addr) \
      --out-file resign-child-6/body.json
   Estimated transaction fee: Coin 570455

The only notable thing about this command compared with previous ones is that
there is only one ``required-signer-hash``. The transaction must be signed by
the resignee.

Step 3. Send the Transaction to The Resignee
--------------------------------------------

To build the transaction, we need to get a signature from the resignee.

.. code-block:: bash

   $ cc-sign -q \
      --tx-body-file resign-child-6/body.json \
      --private-key-file example-certificates/children/child-6/child-6.private \
      --out-file resign-child-6/child-6.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file resign-child-6/body.json \
      --signing-key-file orchestrator.skey \
      --out-file resign-child-6/orchestrator.witness

Step 4. Assemble and Submit the Transaction
-------------------------------------------

.. code-block:: bash

   $ cardano-cli conway transaction assemble \
      --tx-body-file resign-child-6/body.json \
      --witness-file resign-child-6/child-6.witness \
      --witness-file resign-child-6/orchestrator.witness \
      --out-file resign-child-6/tx.json
   $ cardano-cli conway transaction submit --tx-file resign-child-6/tx.json
   Transaction successfully submitted.

Step 5. Verify the delegation member is removed
-----------------------------------------------

.. code-block:: bash

   $ cardano-cli conway query utxo --address $(cat init-cold/nft.addr) --output-json
   {
       "5d8103b8f44283de369688df220abbb02f21fd2de2d83f3dbfeca03602b6efa5#0": {
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
                           }
                       ]
                   }
               ]
           },
           "inlineDatumhash": "862f5108240f4788a43e80bb16682b068ff46681197e7114fd1e2c9fc789d39a",
           "referenceScript": null,
           "value": {
               "c8aa0de384ad34d844dc479085c3ed00deb1306afb850a2cde6281f4": {
                   "": 1
               },
               "lovelace": 5000000
           }
       }
   }
