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
      --required-signer-hash $(cat example-certificates/children/child-1/child-1.keyhash) \
      --required-signer-hash $(cat example-certificates/children/child-2/child-2.keyhash) \
      --change-address $(cat orchestrator.addr) \
      --out-file upgrade-hot/body.json
   Estimated transaction fee: Coin 501866
   $ cardano-cli conway transaction witness \
      --tx-body-file upgrade-hot/body.json \
      --signing-key-file example-certificates/children/child-1/child-1.skey \
      --out-file upgrade-hot/child-1.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file upgrade-hot/body.json \
      --signing-key-file example-certificates/children/child-2/child-2.skey \
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
   514e368aed2218a9a3a4ca64955112382af1dfc2fcd4efd464047985689eca44     0        5000000 lovelace + 1 76edba602a94ee8d0e81a59ff6470bc490cb1649066e0678143b4bf3.4844bfe98f124abc1d2203fc586a46140168d38777f46abd8c393c482301 + TxOutDatumNone

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
       "f6bbf7757b4dafaade2a943e2dfa2fcc174eb5b79f8a7ebd014d89a621725e28#0": {
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
      --required-signer-hash $(cat example-certificates/children/child-4/child-4.keyhash) \
      --required-signer-hash $(cat example-certificates/children/child-5/child-5.keyhash) \
      --change-address $(cat orchestrator.addr) \
      --out-file upgrade-cold/body.json
   Estimated transaction fee: Coin 534756
   $ cardano-cli conway transaction witness \
      --tx-body-file upgrade-cold/body.json \
      --signing-key-file example-certificates/children/child-4/child-4.skey \
      --out-file upgrade-cold/child-4.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file upgrade-cold/body.json \
      --signing-key-file example-certificates/children/child-5/child-5.skey \
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
   5f63a38edcec6e0b33995419aab10a4ea8bce91e0bd331b7e0d428f8e1c506f3     0        5000000 lovelace + 1 c8aa0de384ad34d844dc479085c3ed00deb1306afb850a2cde6281f4 + TxOutDatumNone


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
       "946249cbf47b7f09be605280e6358a32a15505353185184cb576da6e7a9b9b07#0": {
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
