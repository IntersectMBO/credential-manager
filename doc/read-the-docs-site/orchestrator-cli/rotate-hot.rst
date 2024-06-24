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
     --voting-cert example-certificates/children/child-7/child-7-cert.pem \
     --voting-cert example-certificates/children/child-9/child-9-cert.pem \
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
   <           "bytes": "a3c6cb93a32b02877c61f64ab1c66c4513f12788bf7c500ead7d941b"
   ---
   >           "bytes": "fb5e0be4801aea73135efe43f4a3a6d08147af523112986dd5e7d13b"
   10c10
   <           "bytes": "9923f31c1ce14e2acbd505fa8eebd4ce677d1bcd96c6d71610f810f2008ecc3a"
   ---
   >           "bytes": "57f5530e057e20b726b78aa31104d415cb2bce58c669829a44d009c1b1005bcd"



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
      --required-signer-hash $(cat example-certificates/children/child-1/child-1.keyhash) \
      --required-signer-hash $(cat example-certificates/children/child-2/child-2.keyhash) \
      --required-signer-hash $(cat example-certificates/children/child-7/child-7.keyhash) \
      --change-address $(cat orchestrator.addr) \
      --out-file rotate-hot/body.json
   Estimated transaction fee: Coin 528607

Recall that in the previous section, we swapped the membership and delegation roles, so ``child-1`` and ``child-2`` are now in the delegation group.
As before, any added members need to sign the transaction too.
Since we are adding ``child-7`` to the voting group, they need to sign as well.

Step 3. Distribute the Transaction to Signatories
-------------------------------------------------

.. code-block:: bash

   $ cardano-cli conway transaction witness \
      --tx-body-file rotate-hot/body.json \
      --signing-key-file example-certificates/children/child-1/child-1.skey \
      --out-file rotate-hot/child-1.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file rotate-hot/body.json \
      --signing-key-file example-certificates/children/child-2/child-2.skey \
      --out-file rotate-hot/child-2.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file rotate-hot/body.json \
      --signing-key-file example-certificates/children/child-7/child-7.skey \
      --out-file rotate-hot/child-7.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file rotate-hot/body.json \
      --signing-key-file orchestrator.skey \
      --out-file rotate-hot/orchestrator.witness

Step 4. Assemble and Submit the Transaction
-------------------------------------------

.. code-block:: bash

   $ cardano-cli conway transaction assemble \
      --tx-body-file rotate-hot/body.json \
      --witness-file rotate-hot/child-1.witness \
      --witness-file rotate-hot/child-2.witness \
      --witness-file rotate-hot/child-7.witness \
      --witness-file rotate-hot/orchestrator.witness \
      --out-file rotate-hot/tx.json
   $ cardano-cli conway transaction submit --tx-file rotate-hot/tx.json
   Transaction successfully submitted.

Step 5. Verify the change on chain
----------------------------------

.. code-block:: bash

   $ cardano-cli conway query utxo --address $(cat init-hot/nft.addr) --output-json
   {
       "93724fe4224b26cb728f411ce8c444ae67e0b460603e375b01bc972b787b3f34#0": {
           "address": "addr_test1wr4kx7wd9e5fmjpxlnuznhcy585jv7mc39vu0thll565zmgpu2jpe",
           "datum": null,
           "inlineDatum": {
               "list": [
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
           "inlineDatumhash": "c76a8897910eae665c54b888ad9ac64aa555478349af5f2322c5cb06a6b373c0",
           "referenceScript": null,
           "value": {
               "76edba602a94ee8d0e81a59ff6470bc490cb1649066e0678143b4bf3": {
                   "4844bfe98f124abc1d2203fc586a46140168d38777f46abd8c393c482301": 1
               },
               "lovelace": 5000000
           }
       }
   }
