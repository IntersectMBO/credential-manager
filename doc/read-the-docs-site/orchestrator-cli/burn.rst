.. _burn:

Burning the NFTs
==================

Now that we're no longer in the committee, there's just one piece of cleanup left to do: burning the NFTs.

Step 1: Burning the Hot NFT
---------------------------

.. warning::
   You must burn the hot NFT before you burn the cold NFT.
   If you burn the cold NFT first, you won't be able to burn the hot NFT because the delegation group needs to authorize the burn.

Once more, we will use ``orchestrator-cli`` to prepare the transaction assets:

.. code-block:: bash

   $ fetch-hot-nft-utxo
   $ orchestrator-cli burn-hot \
     --utxo-file hot-nft.utxo \
     --out-dir burn-hot


As before, let's see what assets were prepared:

.. code-block:: bash

   $ ls burn-hot -1
   mint.redeemer.json
   nft.redeemer.json

This time we just get a minting redeemer and an NFT lock script redeemer.
There is no NFT output so no datum or value file is written.

.. code-block:: bash

   $ cat burn-hot/mint.redeemer.json
   {
       "constructor": 1,
       "fields": [
           {
               "constructor": 0,
               "fields": [
                   {
                       "constructor": 0,
                       "fields": [
                           {
                               "bytes": "f6bbf7757b4dafaade2a943e2dfa2fcc174eb5b79f8a7ebd014d89a621725e28"
                           }
                       ]
                   },
                   {
                       "int": 0
                   }
               ]
           }
       ]
   }
   $ cat burn-hot/nft.redeemer.json
   {
       "constructor": 10,
       "fields": []
   }

The minting redeemer instructs the minting script to burn (``"constructor": 1``) all tokens in input ``f6bbf7757b4dafaade2a943e2dfa2fcc174eb5b79f8a7ebd014d89a621725e28#0``.

The NFT redeemer contains no interesting information.

Step 2: Create the Hot Burn Transaction
------------------------------------------

Initially, it seems that we should be able to burn both NFTs in a single transaction.
However, if we try do submit a transaction that does so, we get the following error code: ``BabbageNonDisjointRefInputs``.
This is because the cold NFT input must be spent to burn the cold NFT, but it also must be included as a reference input to burn the hot NFT (as with ``rotate``, the ``burn`` action requires signatures from the delegation group for the hot NFT).
There is a ledger rule that requires the set of reference inputs to be disjoint from the set of inputs in a transaction.
So, we have to do it in two transactions.

.. code-block:: bash

   $ cardano-cli conway transaction build \
      --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
      --read-only-tx-in-reference $(cardano-cli query utxo --address $(cat init-cold/nft.addr) --output-json | jq -r 'keys[0]') \
      --tx-in $(cardano-cli query utxo --address $(cat init-hot/nft.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-script-file init-hot/nft.plutus \
      --tx-in-inline-datum-present \
      --tx-in-redeemer-file burn-hot/nft.redeemer.json \
      --mint "-1 $(cat init-hot/minting.plutus.hash).$(cat init-hot/nft-token-name)" \
      --mint-script-file init-hot/minting.plutus \
      --mint-redeemer-file burn-hot/mint.redeemer.json \
      --required-signer-hash $(cat example-certificates/children/child-1/child-1.keyhash) \
      --required-signer-hash $(cat example-certificates/children/child-2/child-2.keyhash) \
      --change-address $(cat orchestrator.addr) \
      --out-file burn-hot/body.json
   Estimated transaction fee: Coin 667729
   $ cardano-cli conway transaction witness \
      --tx-body-file burn-hot/body.json \
      --signing-key-file example-certificates/children/child-1/child-1.skey \
      --out-file burn-hot/child-1.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file burn-hot/body.json \
      --signing-key-file example-certificates/children/child-2/child-2.skey \
      --out-file burn-hot/child-2.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file burn-hot/body.json \
      --signing-key-file orchestrator.skey \
      --out-file burn-hot/orchestrator.witness
   $ cardano-cli conway transaction assemble \
      --tx-body-file burn-hot/body.json \
      --witness-file burn-hot/child-1.witness \
      --witness-file burn-hot/child-2.witness \
      --witness-file burn-hot/orchestrator.witness \
      --out-file burn-hot/tx.json
   $ cardano-cli conway transaction submit --tx-file burn-hot/tx.json
   Transaction successfully submitted.

Step 3. Verify the change on chain
----------------------------------

.. code-block:: bash

   $ cardano-cli conway query utxo --address $(cat init-hot/nft.addr)
                              TxHash                                 TxIx        Amount
   --------------------------------------------------------------------------------------

Step 4: Burning the Cold NFT
---------------------------

Once more, we will use ``orchestrator-cli`` to prepare the transaction assets:

.. code-block:: bash

   $ fetch-cold-nft-utxo
   $ orchestrator-cli burn-cold \
     --utxo-file cold-nft.utxo \
     --out-dir burn-cold


As before, let's see what assets were prepared:

.. code-block:: bash

   $ ls burn-cold -1
   mint.redeemer.json
   nft.redeemer.json

The outputs are similar to the ones for ``burn-hot``.

Step 5: Create the Cold Burn Transaction
----------------------------------------

This will proceed similar to ``burn-hot``, except the membership group needs to sign.

.. code-block:: bash

   # If using the real minting script (i.e. you are not following the guide in a local testnet)
   $ cardano-cli conway transaction build \
      --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in $(cardano-cli query utxo --address $(cat init-cold/nft.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-script-file init-cold/nft.plutus \
      --tx-in-inline-datum-present \
      --tx-in-redeemer-file burn-cold/nft.redeemer.json \
      --mint "-1 $(cat init-cold/minting.plutus.hash).$(cat init-cold/nft-token-name)" \
      --mint-script-file init-cold/minting.plutus \
      --mint-redeemer-file burn-cold/mint.redeemer.json \
      --required-signer-hash $(cat example-certificates/children/child-4/child-4.keyhash) \
      --required-signer-hash $(cat example-certificates/children/child-5/child-5.keyhash) \
      --change-address $(cat orchestrator.addr) \
      --out-file burn-cold/body.json
   Estimated transaction fee: Coin 667729
   # If using the custom minting script provided in this guide for the local testnet setup.
   $ cardano-cli conway transaction build \
      --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in $(cardano-cli query utxo --address $(cat init-cold/nft.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-script-file init-cold/nft.plutus \
      --tx-in-inline-datum-present \
      --tx-in-redeemer-file burn-cold/nft.redeemer.json \
      --mint "-1 $COLD_POLICY_ID" \
      --mint-script-file coldMint.native \
      --required-signer-hash $(cat example-certificates/children/child-4/child-4.keyhash) \
      --required-signer-hash $(cat example-certificates/children/child-5/child-5.keyhash) \
      --change-address $(cat orchestrator.addr) \
      --out-file burn-cold/body.json
   Estimated transaction fee: Coin 529119
   $ cardano-cli conway transaction witness \
      --tx-body-file burn-cold/body.json \
      --signing-key-file example-certificates/children/child-4/child-4.skey \
      --out-file burn-cold/child-4.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file burn-cold/body.json \
      --signing-key-file example-certificates/children/child-5/child-5.skey \
      --out-file burn-cold/child-5.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file burn-cold/body.json \
      --signing-key-file orchestrator.skey \
      --out-file burn-cold/orchestrator.witness
   # If using the custom minting script provided in this guide for the local testnet setup.
   $ cardano-cli conway transaction witness \
      --tx-body-file burn-cold/body.json \
      --signing-key-file coldMint.skey \
      --out-file burn-cold/coldMint.witness
   # If using the real minting script (i.e. you are not following the guide in a local testnet)
   $ cardano-cli conway transaction assemble \
      --tx-body-file burn-cold/body.json \
      --witness-file burn-cold/child-4.witness \
      --witness-file burn-cold/child-5.witness \
      --witness-file burn-cold/orchestrator.witness \
      --out-file burn-cold/tx.json
   # If using the custom minting script provided in this guide for the local testnet setup.
   $ cardano-cli conway transaction assemble \
      --tx-body-file burn-cold/body.json \
      --witness-file burn-cold/child-4.witness \
      --witness-file burn-cold/child-5.witness \
      --witness-file burn-cold/orchestrator.witness \
      --witness-file burn-cold/coldMint.witness \
      --out-file burn-cold/tx.json
   $ cardano-cli conway transaction submit --tx-file burn-cold/tx.json
   Transaction successfully submitted.

Step 6. Verify the change on chain
----------------------------------

.. code-block:: bash

   $ cardano-cli conway query utxo --address $(cat init-cold/nft.addr)
                              TxHash                                 TxIx        Amount
   --------------------------------------------------------------------------------------

This concludes the guide to using ``orchestrator-cli``.
