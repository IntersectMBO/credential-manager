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
       --voting-cert example-certificates/child-7.cert \
       --voting-cert example-certificates/child-8.cert \
       --voting-cert example-certificates/child-9.cert \
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
                       "bytes": "c6d6ffd8e93b1b8352c297d528c958b982098dc8a08025bbb8d864cf"
                   },
                   {
                       "bytes": "e3340359f5d25c051e4dd160e4cb4d75074c537905f07eb9a2e24db881246ee0"
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
   }

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
       "fca0a026d079b72f4870b794d2dbfb68f141ccd5049a15481086b6ee72700d36#0": {
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
                               "bytes": "c6d6ffd8e93b1b8352c297d528c958b982098dc8a08025bbb8d864cf"
                           },
                           {
                               "bytes": "e3340359f5d25c051e4dd160e4cb4d75074c537905f07eb9a2e24db881246ee0"
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
           "inlineDatumhash": "bcaef52050aed8a9720c1854860aee8625f3655fdeb8697a5650e257d7c56fc9",
           "referenceScript": null,
           "value": {
               "bf3bbf5a8539663eddd53364a9fd90e468c0182fcf6f0642ac16d65f": {
                   "93fdf1b28aefd28ed13b268653c03dd86872063d58434a2c83d68e6c2301": 1
               },
               "lovelace": 5000000
           }
       }
   }
