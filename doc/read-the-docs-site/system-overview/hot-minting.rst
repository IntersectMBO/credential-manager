.. _hot_minting_script:

Hot NFT minting script
=======================

Plutus Version
--------------

Plutus V2.

Rules
-----

The script enforces the following rules unconditionally:

* The purpose of the script execution must be `Minting`.

* The tokens being minted or burned must be in the transaction's mint value (this will always be the case when the script is invoked by a properly implemented ledger layer).

The script enforces the following additional rules depending on the redeemer:

Rules for ``Mint`` actions
~~~~~~~~~~~~~~~~~~~~~~~~~~

If the redeemer is a ``Mint`` action:

* The transaction mints only one token name.
* The quantity of tokens minted is one.
* The name of the minted token is the following concatenation of bytes from the seed input in the redeemer:

  * The last 28 bytes of the tx ID.
  * The byte ``35`` (ascii for ``'#'``)
  * The tx ix converted to a byte.

* The tx ix from the seed input must fit within a byte (i.e. ``< 256``).
* The transaction must consume the seed input in the redeemer.
* The NFT must be sent to an address with a script payment credential matching the value provided in the redeemer.
* The output containing the NFT must contain a valid hot lock datum as an inline datum.

  * The voting group must not be empty
  * The voting group must not contain duplicate public key hashes.

* No other outputs may be sent to the destination script.

Rules for ``Burn`` actions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the redeemer is a ``Burn`` action:

* The transaction must consume the input specified in the redeemer.
* The consumed input must contain at least one token with the minting policy's policy ID.
* For every token in the input:

  * The token must be present in the transaction mint value.
  * The quantity minted must be the negation of the quantity in the input.

* For every token in the transaction mint value:

  * The token must be present in the transaction input.

Datum
-----

Minting scripts do not have a datum.

Redeemer
--------

Main schema
~~~~~~~~~~~

* **Type**: constructor
* **Valid constructor indexes**:
    0. * **Haskell Name**: ``Mint``
       * **Description**: Require the transaction to mint one token
       * **Fields**:
          * Field 1:
              * **Type**: TxOutRef from Plutus V2.
              * **Description**: The seed input from which the token name is derived.
          * Field 2:
              * **Type**: ScriptHash from Plutus V2.
              * **Description**: The hash of the script the NFT must be sent to.
    1. * **Haskell Name**: ``Burn``
       * **Description**: Require the transaction to burn all tokens in the specified input.
       * **Fields**:
          * Field 1:
              * **Type**: TxOutRef from Plutus V2.
              * **Description**: The input containing the tokens which must be burned.
