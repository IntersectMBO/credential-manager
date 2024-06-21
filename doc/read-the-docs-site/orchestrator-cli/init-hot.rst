.. _init_hot:

Initializing the Hot Credential Scripts
===================================================

The next step is to initialize the hot credential scripts, which will be very similar to initializing the cold NFT scripts.

Step 1: Obtaining the X.509 certificate files
---------------------------------------------

Follow the steps listed in :ref:`init_cold`.
``child-7`` through ``child-9`` will belong to the voting role.

Step 2(a): Creating the assets
------------------------------

We can now use ``orchestrator-cli`` to initialize the script:

.. code-block:: bash

   $ orchestrator-cli init-hot \
       --seed-input "$(get-orchestrator-ada-only | jq -r '.key')" \
       --testnet \
       --cold-nft-policy-id "$(cat init-cold/minting.plutus.hash)" \
       --cold-nft-token-name "$(cat init-cold/nft-token-name)" \
       --voting-cert example-certificates/children/child-7/child-7-cert.pem \
       --voting-cert example-certificates/children/child-8/child-8-cert.pem \
       --voting-cert example-certificates/children/child-9/child-9-cert.pem \
       -o init-hot

Let's see what assets were created.

.. code-block:: bash

   $ ls init-hot -1
   credential.plutus
   credential.plutus.hash
   minting.plutus
   minting.plutus.hash
   mint.redeemer.json
   nft.addr
   nft.datum.json
   nft.plutus
   nft.plutus.hash
   nft-token-name

The files are very similar to what was covered in :ref:`init_cold`, so we will skip looking at most of them as the contents are not that interesting.

``datum.json`` contains the initial datum for the script.

.. code-block:: bash

   $ cat init-hot/nft.datum.json
   {
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
   }

We can, and should, sanity check that this datum contains the correct values:

.. code-block:: bash

   $ diff \
      <(cat example-certificates/children/child-7/child-7.keyhash) \
      <(cat init-hot/nft.datum.json | jq -r '.list[0].fields[0].bytes')
   $ diff \
      <(cat example-certificates/children/child-7/child-7-cert.hash) \
      <(cat init-hot/nft.datum.json | jq -r '.list[0].fields[1].bytes')
   $ diff \
      <(cat example-certificates/children/child-8/child-8.keyhash) \
      <(cat init-hot/nft.datum.json | jq -r '.list[1].fields[0].bytes')
   $ diff \
      <(cat example-certificates/children/child-8/child-8-cert.hash) \
      <(cat init-hot/nft.datum.json | jq -r '.list[1].fields[1].bytes')
   $ diff \
      <(cat example-certificates/children/child-9/child-9.keyhash) \
      <(cat init-hot/nft.datum.json | jq -r '.list[2].fields[0].bytes')
   $ diff \
      <(cat example-certificates/children/child-9/child-9-cert.hash) \
      <(cat init-hot/nft.datum.json | jq -r '.list[2].fields[1].bytes')

Step 2(b): Creating the assets (custom NFT lock script)
-------------------------------------------------------

See :ref:`init_cold` - the process is nearly identical with the ``init-hot`` command.

Step 3: Assemble and submit the transaction
-------------------------------------------

Using the assets created in step 2, create a transaction that sends the datum
to the script address:

.. code-block:: bash

   $ cardano-cli conway transaction build \
     --change-address $(cat orchestrator.addr) \
     --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
     --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
     --tx-out "$(cat init-hot/nft.addr) + 5000000 + 1 $(cat init-hot/minting.plutus.hash).$(cat init-hot/nft-token-name)" \
     --tx-out-inline-datum-file init-hot/nft.datum.json \
     --mint "1 $(cat init-hot/minting.plutus.hash).$(cat init-hot/nft-token-name)" \
     --mint-script-file init-hot/minting.plutus \
     --mint-redeemer-file init-hot/mint.redeemer.json \
     --out-file init-hot/body.json
   Estimated transaction fee: Coin 375278
   $ cardano-cli conway transaction sign \
     --signing-key-file orchestrator.skey \
     --tx-body-file init-hot/body.json \
     --out-file init-hot/tx.json
   $ cardano-cli conway transaction submit --tx-file init-hot/tx.json
   Transaction successfully submitted.

We can query the script address to verify the UTxO is there:

.. code-block:: bash

   $ cardano-cli conway query utxo --address $(cat init-hot/nft.addr) --output-json
   {
       "6af542d6152060d2baf351c0e4fda7f5160b74dd535be1ec8485fd40cdb9e0b7#0": {
           "address": "addr_test1wr4jyutztpjge37c2n377qzsnrkv67xp86lmahty4dhv5wqf2l99t",
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
           "inlineDatumhash": "4601cb700ba4288f35fd71eb1d86ddd8207d16b195d5a1fa0e6a4897c21edf79",
           "referenceScript": null,
           "value": {
               "76edba602a94ee8d0e81a59ff6470bc490cb1649066e0678143b4bf3": {
                   "5c94bfec2d9e8a0e0c536df3384e5001adf6d333216a2b546b6f043a2301": 1
               },
               "lovelace": 5000000
           }
       }
   }
