.. _unlock:

Unlocking the NFTs
==================

Now that we're no longer in the committee, there's just one piece of cleanup
left to do: recovering the NFTs.

.. danger::
   Unlocking The NFTs is risky when the credentials are still active. The
   scripts don't check where they are sent. It's important to be extra cautious
   and double check everything when preparing a transaction that unlocks one of
   the NFTs.

.. info::
   Recovering the NFTs after the credentials are inactive is not the only use
   case for unlocking. For example, you can upgrade the locking script by
   unlocking an NFT and sending it to a new script address.

Step 1: Creating the assets
---------------------------

Once more, we will use ``orchestrator-cli`` to prepare the transaction assets:

.. code-block:: bash

   $ orchestrator-cli cold-nft unlock -o unlock-cold
   $ orchestrator-cli hot-nft unlock -o unlock-hot

As before, let's see what assets were prepared:

.. code-block:: bash

   $ ls unlock-cold -1
   redeemer.json
   $ ls unlock-hot -1
   redeemer.json

We just have a redeemer in each folder.

.. code-block:: bash

   $ cat unlock-cold/redeemer.json
   {
       "constructor": 4,
       "fields": []
   }
   $ cat unlock-hot/redeemer.json
   {
       "constructor": 3,
       "fields": []
   }

And they have no interesting information in them. The ``unlock`` commands are
actually a bit redundant - they are there for completeness, but we could easily
have written these redeemers by hand. Nonetheless, let's prepare the
transaction with them to send the NFTs back to the orchestrator:

Step 2: Create the Cold Unlock Transaction
------------------------------------------

Initially, it seems that we should be able to unlock both NFTs in a single
transaction. However, if we try do submit a transaction that does so, we get
the following error code: ``BabbageNonDisjointRefInputs``. This is because the
cold NFT input must be spent to unlock the cold NFT, but it also must be
included as a reference input to unlock the hot NFT (as with ``rotate``, the
``unlock`` action requires signatures from the delegation group for the hot
NFT). There is a ledger rule that requires the set of reference inputs to be
disjoint from the set of inputs in a transaction. So, we have to do it in two
transactions:

.. code-block:: bash

   $ cardano-cli conway transaction build \
      --tx-in $(cardano-cli query utxo --address $(cat orchestrator.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-collateral $(cardano-cli query utxo --address $(cat orchestrator.addr) --output-json | jq -r 'keys[0]') \
      --read-only-tx-in-reference $(cardano-cli query utxo --address $(cat cold-nft/script.addr) --output-json | jq -r 'keys[0]') \
      --tx-in $(cardano-cli query utxo --address $(cat hot-nft/script.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-script-file hot-nft/script.plutus \
      --tx-in-inline-datum-present \
      --tx-in-redeemer-file unlock-hot/redeemer.json \
      --tx-out "$(cat orchestrator.addr)+5000000 + 1 $HOT_POLICY_ID" \
      --required-signer-hash $(cat example-certificates/children/child-1/child-1.keyhash) \
      --required-signer-hash $(cat example-certificates/children/child-2/child-2.keyhash) \
      --change-address $(cat orchestrator.addr) \
      --out-file unlock-hot.body
   Estimated transaction fee: Coin 430081
   $ cardano-cli conway transaction witness \
      --tx-body-file unlock-hot.body \
      --signing-key-file example-certificates/children/child-1/child-1.skey \
      --out-file unlock-hot.child-1.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file unlock-hot.body \
      --signing-key-file example-certificates/children/child-2/child-2.skey \
      --out-file unlock-hot.child-2.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file unlock-hot.body \
      --signing-key-file orchestrator.skey \
      --out-file unlock-hot.orchestrator.witness
   $ cardano-cli conway transaction assemble \
      --tx-body-file unlock-hot.body \
      --witness-file unlock-hot.child-1.witness \
      --witness-file unlock-hot.child-2.witness \
      --witness-file unlock-hot.orchestrator.witness \
      --out-file unlock-hot.tx
   $ cardano-cli conway transaction submit --tx-file unlock-hot.tx
   Transaction successfully submitted.
   $ cardano-cli conway transaction build \
      --tx-in "$(cardano-cli conway transaction txid --tx-body-file unlock-hot.body)#1" \
      --tx-in-collateral "$(cardano-cli conway transaction txid --tx-body-file unlock-hot.body)#1" \
      --tx-in $(cardano-cli query utxo --address $(cat cold-nft/script.addr) --output-json | jq -r 'keys[0]') \
      --tx-in-script-file cold-nft/script.plutus \
      --tx-in-inline-datum-present \
      --tx-in-redeemer-file unlock-cold/redeemer.json \
      --tx-out "$(cat orchestrator.addr)+5000000 + 1 $COLD_POLICY_ID" \
      --required-signer-hash $(cat example-certificates/children/child-4/child-4.keyhash) \
      --change-address $(cat orchestrator.addr) \
      --out-file unlock-cold.body
   Estimated transaction fee: Coin 409311
   $ cardano-cli conway transaction witness \
      --tx-body-file unlock-cold.body \
      --signing-key-file example-certificates/children/child-4/child-4.skey \
      --out-file unlock-cold.child-4.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file unlock-cold.body \
      --signing-key-file orchestrator.skey \
      --out-file unlock-cold.orchestrator.witness
   $ cardano-cli conway transaction assemble \
      --tx-body-file unlock-cold.body \
      --witness-file unlock-cold.child-4.witness \
      --witness-file unlock-cold.orchestrator.witness \
      --out-file unlock-cold.tx
   $ cardano-cli conway transaction submit --tx-file unlock-cold.tx
   Transaction successfully submitted.

.. warning::
    You need to unlock the hot NFT first! If not, you won't be able to pass the
    cold NFT output to the hot NFT unlock transaction as a reference input (or
    at least doing so will become very awkward).

Step 3. Verify the change on chain
----------------------------------

.. code-block:: bash

   $ cardano-cli conway query utxo --address $(cat cold-nft/script.addr)
                              TxHash                                 TxIx        Amount
   --------------------------------------------------------------------------------------
   $ cardano-cli conway query utxo --address $(cat hot-nft/script.addr)
                              TxHash                                 TxIx        Amount
   --------------------------------------------------------------------------------------
   $ cardano-cli conway query utxo --address $(cat orchestrator.addr)
                              TxHash                                 TxIx        Amount
   --------------------------------------------------------------------------------------
   a6735bb188d9c12d6c47badc5971ef1045f2310cc2005ba48bef642fdded101a     0        5000000 lovelace + 1 63ac965b8bab57dc91f302dad97d1d70e979e8cae8d3514c7ad6f86f + TxOutDatumNone
   200f3890f041e19137755e617285a083a24f81a9475c43a42219f088a909ac03     0        5000000 lovelace + 1 14987a29cf4065e7b38a4cde6bc84b067492ad3ecc8223598a8fe4be + TxOutDatumNone
   200f3890f041e19137755e617285a083a24f81a9475c43a42219f088a909ac03     1        398983356769 lovelace + TxOutDatumNone
