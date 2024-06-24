.. _hot_nft_locking_script:

Hot NFT locking script
=======================

Cold NFT asset class parameter
------------------------------

The hot NFT locking script requires the NFT asset class of the cold credential to function.
This asset class gets inlined as a parameter into the code of the script directly when it is compiled and initialized. 

NFT asset class parameter
-------------------------

The hot NFT locking script requires its own NFT asset class to function.
This asset class gets inlined as a parameter into the code of the script directly when it is compiled and initialized. 

Hot committee credential parameters
------------------------------------

The hot NFT locking script requires a specific hot committee credential to function.
This hot committee credential gets inlined as a parameter in the code of the hot NFT locking script directly when it is compiled and initialized. 

Rules
-----

The script enforces the following rules unconditionally:

* The purpose of the script execution must be `Spending`.
* The UTXO being spent must be in the transaction's inputs; this will always be the case when a properly implemented ledger layer invokes the script.

The script enforces the following additional rules depending on the redeemer:

Rules for ``Vote`` actions
~~~~~~~~~~~~~~~~~~~~~~~~~~

If the redeemer is a ``Vote`` action, the transaction:

* Produces an output that is identical to the input being authorized.
* Does not send any other outputs to an address with the script's own payment credential.
* Has been signed by a majority of users from the **voting group** defined in the datum.
* Does not contain votes from other voters.
* Contains the following committee voting procedure:

  * The hot credential performing the vote is the same credential provided to the script as a parameter.
  * The voting procedure casts one or more votes.

Rules for ``ResignVoting`` actions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the redeemer is a ``ResignVoting`` action:

* The transaction sends an output that meets the following criteria:

  * The address matches the address of the input being authorized.
  * The value of the output is the same as the value of the input being authorized.
  * The datum is unchanged with the exception that the resignee specified in the redeemer is removed from the **voting group**.
  * The reference script in the output is the same if present, otherwise it is not set.

* The transaction does not send any other outputs to an address with the script's own payment credential.
* The transaction has been signed by the resignee.
* The resignee is a member of the **voting group** defined by the datum.
* The resignee is not the last member of the **voting group**, which would leave the group empty.
* The transaction does not contain any votes.

Rules for ``RotateHot`` actions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the redeemer is a ``RotateHot`` action:

* The transaction sends an output that meets the following criteria:

  * The address matches the address of the input being authorized.
  * The value of the output is the same as the value of the input being authorized.
  * the **voting group** in the output datum is not empty.
  * The output does not contain a reference script.

* The transaction does not send any other outputs to an address with the script's own payment credential.
* The transaction includes a reference input that holds the cold NFT.
* The reference input contains a valid ``ColdLockDatum``.
* The transaction has been signed by a majority of users from the **delegation group** defined in the reference input's datum.
* The transaction has been signed by all members of the **voting group** in the output datum that are not in the input datum.
* The transaction does not contain any votes.

Rules for ``BurnHot`` actions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the redeemer is an ``BurnHot`` action:

* The transaction includes a reference input that holds the cold NFT.
* The reference input contains a valid ``ColdLockDatum``.
* The transaction has been signed by a majority of users from the **delegation group** defined in the reference input's datum.
* All outputs from the transaction do not contain the hot NFT.
* The transaction does not contain any votes.

Datum
-----

Main schema
~~~~~~~~~~~

* **Type**: constructor
* **Valid constructor indexes**:
    0. * **Haskell Name**: ``HotLockDatum``
       * **Fields**:
          * Field 1:
              * **Type**: List of :ref:`Identities <identity_schema>`
              * **Haskell Name** ``votingUsers``
              * **Description**: The public key hashes and certificate hashes of the users in the **voting group**.

Redeemer
--------

Main schema
~~~~~~~~~~~

* **Type**: constructor
* **Valid constructor indexes**:
    7. * **Haskell Name**: ``Vote``
       * **Description**: Require the transaction to cast a committee vote.
    8. * **Haskell Name**: ``ResignVoting``
       * **Description**: Require the transaction to remove a user from the **voting group**.
       * **Fields**:
          * Field 1:
              * **Type**: :ref:`Identity <identity_schema>`
              * **Description**: The resignee.
    9. * **Haskell Name**: ``RotateHot``
       * **Description**: Allow the transaction to change the members of the **voting group**.
    10. * **Haskell Name**: ``BurnHot``
        * **Description**: Require the transaction to burn the NFT.
    11. * **Haskell Name**: ``UpgradeHot``
        * **Description**: Require the transaction to send the NFT to a new script address.
        * **Fields**:
           * Field 1:
               * **Type**: ScriptHash from Plutus V3.
               * **Description**: The script that will receive the NFT.

.. warning::
   The **delegation group** has full control over the hot NFT, and consequently the hot credential itself. 
   **delegation group** members should safeguard their keys as if they were keys for the hot credential itself.

