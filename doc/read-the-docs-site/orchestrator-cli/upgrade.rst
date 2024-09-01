.. _upgrade:

Upgrading the NFT locking scripts
=================================

Suppose there is a new version of the lock scripts which offers enhanced security or new functionality.
We are able to send the NFTs to arbitrary scripts given the correct signatures.

Step 1: Upgrading the Hot NFT
-----------------------------

.. warning::
   If you need to upgrade both scripts, you should upgrade the hot script before you upgrade the cold script.
   If the cold script datum format changes, you won't be able to upgrade the hot script because it won't be able to decode the datum to get the delegation group.
   Therefore, upgrading the cold script usually involved upgrading the hot script to a version that can decode the new cold datum format, and this must be done first.

First we need a script to send it to. For demonstration purposes, we're going to send it to an always true native script.

.. code-block:: bash

   $ cat << EOF > alwaysTrue.native
   {
      "type": "all",
      "scripts": []
   }
   EOF
   $ cardano-cli conway transaction policyid --script-file alwaysTrue.native > alwaysTrue.hash
   $ cardano-cli conway address build --testnet-magic 0 --payment-script-file alwaysTrue.native > alwaysTrue.addr

Once more, we will use ``orchestrator-cli`` to prepare the transaction assets:

.. code-block:: bash

   $ fetch-hot-nft-utxo
   $ jq 'to_entries | .[0].value.inlineDatum' < hot-nft.utxo > hot-datum-backup.json
   $ orchestrator-cli upgrade-hot \
     --new-script-hash "$(cat alwaysTrue.hash)" \
     --out-dir upgrade-hot


As before, let's see what assets were prepared:

.. code-block:: bash

   $ ls upgrade-hot -1
   redeemer.json

This time we just get redeemer that tells the script what the upgrade destination script is.

.. code-block:: bash

   $ cat upgrade-hot/redeemer.json
   {
       "constructor": 11,
       "fields": [
           {
               "bytes": "d441227553a0f1a965fee7d60a0f724b368dd1bddbc208730fccebcf"
           }
       ]
   }

Step 2: Create the Hot Upgrade Transaction
------------------------------------------

We need to send the NFT to the new script and require the delegation group to sign.

.. code-block:: bash

   $ cardano-cli conway transaction build \
      --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
      --read-only-tx-in-reference $(cardano-cli query utxo --address $(cat init-cold/nft.addr) --output-json | jq -r 'keys[0]') \
      --tx-in $(cardano-cli query utxo --address $(cat init-hot/nft.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-script-file init-hot/nft.plutus \
      --tx-in-inline-datum-present \
      --tx-in-redeemer-file upgrade-hot/redeemer.json \
      --tx-out "$(cat alwaysTrue.addr)+5000000 + 1 $(cat init-hot/minting.plutus.hash).$(cat init-hot/nft-token-name)" \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-1.cert) \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-2.cert) \
      --change-address $(cat orchestrator.addr) \
      --out-file upgrade-hot/body.json
   Estimated transaction fee: Coin 501866
   $ cc-sign -q \
      --tx-body-file upgrade-hot/body.json \
      --private-key-file example-certificates/children/child-1/child-1.private \
      --out-file upgrade-hot/child-1.witness
   $ cc-sign -q \
      --tx-body-file upgrade-hot/body.json \
      --private-key-file example-certificates/children/child-2/child-2.private \
      --out-file upgrade-hot/child-2.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file upgrade-hot/body.json \
      --signing-key-file orchestrator.skey \
      --out-file upgrade-hot/orchestrator.witness
   $ cardano-cli conway transaction assemble \
      --tx-body-file upgrade-hot/body.json \
      --witness-file upgrade-hot/child-1.witness \
      --witness-file upgrade-hot/child-2.witness \
      --witness-file upgrade-hot/orchestrator.witness \
      --out-file upgrade-hot/tx.json
   $ cardano-cli conway transaction submit --tx-file upgrade-hot/tx.json
   Transaction successfully submitted.

Step 3. Verify the change on chain
----------------------------------

.. code-block:: bash

   $ cardano-cli conway query utxo --address $(cat init-hot/nft.addr)
                              TxHash                                 TxIx        Amount
   --------------------------------------------------------------------------------------
   $ cardano-cli conway query utxo --address $(cat alwaysTrue.addr)
                              TxHash                                 TxIx        Amount
   --------------------------------------------------------------------------------------
   9c771fabd3898b8101980c8ac49d499f46c66b9bc0b7e2d87c333cbc667daa3b     0        5000000 lovelace + 1 bf3bbf5a8539663eddd53364a9fd90e468c0182fcf6f0642ac16d65f.93fdf1b28aefd28ed13b268653c03dd86872063d58434a2c83d68e6c 2301 + TxOutDatumNone


Step 4. Send the NFT back to the hot lock script
------------------------------------------------

To continue this guide, we need the NFT to be held in the original lock script, so let's build a transaction that sends it back.

.. code-block:: bash

   $ cardano-cli conway transaction build \
      --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in $(cardano-cli query utxo --address $(cat alwaysTrue.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-script-file alwaysTrue.native \
      --tx-out "$(cat init-hot/nft.addr)+5000000 + 1 $(cat init-hot/minting.plutus.hash).$(cat init-hot/nft-token-name)" \
      --tx-out-inline-datum-file hot-datum-backup.json \
      --change-address $(cat orchestrator.addr) \
      --out-file restore-hot-body.json
   Estimated transaction fee: Coin 181561
   $ cardano-cli conway transaction sign \
     --signing-key-file orchestrator.skey \
     --tx-body-file restore-hot-body.json \
     --out-file restore-hot-tx.json
   $ cardano-cli conway transaction submit --tx-file restore-hot-tx.json
   Transaction successfully submitted.
   $ cardano-cli conway query utxo --address $(cat init-hot/nft.addr) --output-json
   {
       "42e381fc2d8e4cf65d4564be1545ce891cb80c952c2f51c60fa0460d66ff11ce#0": {
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

Step 5: Upgrading the Cold NFT
------------------------------

Once more, we will use ``orchestrator-cli`` to prepare the transaction assets:

.. code-block:: bash

   $ fetch-cold-nft-utxo
   $ jq 'to_entries | .[0].value.inlineDatum' < cold-nft.utxo > cold-datum-backup.json
   $ orchestrator-cli upgrade-cold \
     --new-script-hash "$(cat alwaysTrue.hash)" \
     --out-dir upgrade-cold


Step 6: Create the cold Upgrade Transaction
-------------------------------------------

We need to send the NFT to the new script and require the membership group to sign.

.. code-block:: bash

   $ cardano-cli conway transaction build \
      --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in $(cardano-cli query utxo --address $(cat init-cold/nft.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-script-file init-cold/nft.plutus \
      --tx-in-inline-datum-present \
      --tx-in-redeemer-file upgrade-cold/redeemer.json \
      --tx-out "$(cat alwaysTrue.addr)+5000000 + 1 $(cat init-cold/minting.plutus.hash).$(cat init-cold/nft-token-name)" \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-4.cert) \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-5.cert) \
      --change-address $(cat orchestrator.addr) \
      --out-file upgrade-cold/body.json
   Estimated transaction fee: Coin 534756
   $ cc-sign -q \
      --tx-body-file upgrade-cold/body.json \
      --private-key-file example-certificates/children/child-4/child-4.private \
      --out-file upgrade-cold/child-4.witness
   $ cc-sign -q \
      --tx-body-file upgrade-cold/body.json \
      --private-key-file example-certificates/children/child-5/child-5.private \
      --out-file upgrade-cold/child-5.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file upgrade-cold/body.json \
      --signing-key-file orchestrator.skey \
      --out-file upgrade-cold/orchestrator.witness
   $ cardano-cli conway transaction assemble \
      --tx-body-file upgrade-cold/body.json \
      --witness-file upgrade-cold/child-4.witness \
      --witness-file upgrade-cold/child-5.witness \
      --witness-file upgrade-cold/orchestrator.witness \
      --out-file upgrade-cold/tx.json
   $ cardano-cli conway transaction submit --tx-file upgrade-cold/tx.json
   Transaction successfully submitted.

Step 7. Verify the change on chain
----------------------------------

.. code-block:: bash

   $ cardano-cli conway query utxo --address $(cat init-cold/nft.addr)
                              TxHash                                 TxIx        Amount
   --------------------------------------------------------------------------------------
   $ cardano-cli conway query utxo --address $(cat alwaysTrue.addr)
                              TxHash                                 TxIx        Amount
   --------------------------------------------------------------------------------------
   d5df9c7067f9e085bd6003c513cf7b15e43779dd6df1f7cffef69a6726b98ec0     0        5000000 lovelace + 1 c8aa0de384ad34d844dc479085c3ed00deb1306afb850a2cde6281f4 + TxOutDatumNone


Step 8. Send the NFT back to the cold lock script
-------------------------------------------------

To continue this guide, we need the NFT to be held in the original lock script, so let's build a transaction that sends it back.

.. code-block:: bash

   $ cardano-cli conway transaction build \
      --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in $(cardano-cli query utxo --address $(cat alwaysTrue.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-script-file alwaysTrue.native \
      --tx-out "$(cat init-cold/nft.addr)+5000000 + 1 $(cat init-cold/minting.plutus.hash).$(cat init-cold/nft-token-name)" \
      --tx-out-inline-datum-file cold-datum-backup.json \
      --change-address $(cat orchestrator.addr) \
      --out-file restore-cold-body.json
   Estimated transaction fee: Coin 192473
   $ cardano-cli conway transaction sign \
     --signing-key-file orchestrator.skey \
     --tx-body-file restore-cold-body.json \
     --out-file restore-cold-tx.json
   $ cardano-cli conway transaction submit --tx-file restore-cold-tx.json
   Transaction successfully submitted.
   $ cardano-cli conway query utxo --address $(cat init-cold/nft.addr) --output-json
   {
       "2939bdc7d195642f31cc0b4ce9e61deb8748989a662a775f0385eeeb769358e4#0": {
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
