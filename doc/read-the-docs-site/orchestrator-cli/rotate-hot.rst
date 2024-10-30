.. _rotate_hot:

Rotating Voting Keys
====================

Similarly to how the membership group can rotate the keys in the cold NFT, the delegation group can rotate the voting keys in the hot NFT.

Step 1: Creating the assets
---------------------------

In this example, we are going to add ``child-7``'s key back to the voting group and remove ``child-8``.
As usual, use ``orchestrator-cli`` to prepare the transaction assets:

.. code-block:: bash

   $ fetch-hot-nft-utxo
   $ orchestrator-cli rotate-hot \
     --utxo-file hot-nft.utxo \
     --voting-cert example-certificates/child-7.cert \
     --voting-cert example-certificates/child-9.cert \
     --out-dir rotate-hot

As before, let's see what assets were prepared:

.. code-block:: bash

   $ ls rotate-hot -1
   datum.json
   redeemer.json
   value

We have the familiar ``datum.json``, ``redeemer.json``, and ``value`` files:

.. code-block:: bash

   $ diff <(jq 'to_entries | .[0].value.inlineDatum' < hot-nft.utxo) <(jq '.' < rotate-hot/datum.json)
   7c7
   <           "bytes": "c6d6ffd8e93b1b8352c297d528c958b982098dc8a08025bbb8d864cf"
   ---
   >           "bytes": "c6731b9c6de6bf11d91f08099953cb393505806ff522e5cc3a7574ab"
   10c10
   <           "bytes": "e3340359f5d25c051e4dd160e4cb4d75074c537905f07eb9a2e24db881246ee0"
   ---
   >           "bytes": "e50384c655f9a33cabf64e41df7282e765a242aef182130f1db01bce8859e0aa"

In the datum, ``child-7`` has been added back, and ``child-8`` has been removed.
The redeemer is less interesting, as it takes no arguments:

.. code-block:: bash

   cat rotate-hot/redeemer.json
   {
       "constructor": 9,
       "fields": []
   }

Step 2: Create the Transaction
------------------------------

This is the first instance of the delegation group needing to sign off on a hot NFT script action.
The attentive reader will have noticed that the hot NFT datum does not include the delegation group however, so it would be reasonable to ask how the script knows who is in the delegation group?

Recall that when we initialized the hot NFT script, we provided the cold NFT's policy ID as a parameter.
When it needs the delegation group to sign the transaction, the hot NFT script uses this information to **look for the current cold NFT script output in the transaction's reference inputs**.
It can decode the datum from this reference input to get the current delegation group.
As such, we need to include this input as a reference input when building the transaction.
Otherwise, it is the same as it was for :ref:`cold-nft rotate <rotate_cold>`.

.. code-block:: bash

   $ cardano-cli conway transaction build \
      --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
      --read-only-tx-in-reference $(cardano-cli query utxo --address $(cat init-cold/nft.addr) --output-json | jq -r 'keys[0]') \
      --tx-in $(cardano-cli query utxo --address $(cat init-hot/nft.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-script-file init-hot/nft.plutus \
      --tx-in-inline-datum-present \
      --tx-in-redeemer-file rotate-hot/redeemer.json \
      --tx-out "$(cat rotate-hot/value)" \
      --tx-out-inline-datum-file rotate-hot/datum.json \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash  example-certificates/child-1.cert) \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash  example-certificates/child-2.cert) \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash  example-certificates/child-3.cert) \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash  example-certificates/child-7.cert) \
      --change-address $(cat orchestrator.addr) \
      --out-file rotate-hot/body.json
   Estimated transaction fee: Coin 528607
   $ tx-bundle build \
     --tx-body-file rotate-hot/body.json \
     --group-name delegation \
     --group-threshold 2 \
     --verification-key-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-1.cert) \
     --verification-key-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-2.cert) \
     --verification-key-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-3.cert) \
     --group-name voting-new \
     --group-threshold 1 \
     --verification-key-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-7.cert) \
     --out-file rotate-hot/body.txbundle

Recall that in the previous section, we swapped the membership and delegation roles, so ``child-1`` through ``child-3`` are now in the delegation group.
As before, any added members need to sign the transaction too.
Since we are adding ``child-7`` to the voting group, they need to sign as well.

This is also the first time we've seen ``tx-bundle`` used to create a
transaction with more than one signing group. In this case, two groups of
signatures are needed: any two of the three delegators, and all the new voters
who were added in the rotation.

Step 3. Distribute the Transaction to Signatories
-------------------------------------------------

.. code-block:: bash

   $ cc-sign -q \
      --tx-bundle-file rotate-hot/body.txbundle \
      --private-key-file example-certificates/children/child-1/child-1.private \
      --out-file rotate-hot/child-1.witbundle
   $ cc-sign -q \
      --tx-bundle-file rotate-hot/body.txbundle \
      --private-key-file example-certificates/children/child-2/child-2.private \
      --out-file rotate-hot/child-2.witbundle
   $ cc-sign -q \
      --tx-bundle-file rotate-hot/body.txbundle \
      --private-key-file example-certificates/children/child-7/child-7.private \
      --out-file rotate-hot/child-7.witbundle
   $ tx-bundle witness \
      --all \
      --tx-bundle-file rotate-hot/body.txbundle \
      --signing-key-file orchestrator.skey \
      --out-file rotate-hot/orchestrator.witbundle

Step 4. Assemble and Submit the Transaction
-------------------------------------------

.. code-block:: bash

   $ tx-bundle assemble \
      --tx-bundle-file rotate-hot/body.txbundle \
      --witness-bundle-file rotate-hot/child-1.witbundle \
      --witness-bundle-file rotate-hot/child-2.witbundle \
      --witness-bundle-file rotate-hot/child-7.witbundle \
      --witness-bundle-file rotate-hot/orchestrator.witbundle \
      --out-file rotate-hot/tx.json
   $ cardano-cli conway transaction submit --tx-file rotate-hot/tx.json
   Transaction successfully submitted.

Step 5. Verify the change on chain
----------------------------------

.. code-block:: bash

   $ cardano-cli conway query utxo --address $(cat init-hot/nft.addr) --output-json
   {
       "7c96fe0d24c5bcc398c051569a5a079b1242ec8d6f18eede663f9fd6c4f54eac#0": {
           "address": "addr_test1wzn8zkvkvaex4nnvften2aejpgt3calqwmgmrzwj95vukcs0map8t",
           "datum": null,
           "inlineDatum": {
               "list": [
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
           "inlineDatumhash": "78e128e204031b114f7e3b3b4f4de71b547d5189d6166a3b43370a13bbe9fba5",
           "referenceScript": null,
           "value": {
               "bf3bbf5a8539663eddd53364a9fd90e468c0182fcf6f0642ac16d65f": {
                   "93fdf1b28aefd28ed13b268653c03dd86872063d58434a2c83d68e6c2301": 1
               },
               "lovelace": 5000000
           }
       }
   }
