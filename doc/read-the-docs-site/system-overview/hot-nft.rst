.. _hot_nft_locking_script:

Hot NFT Locking Script
=======================

Parameters
----------

The hot NFT locking script is parameterized by the cold NFT minting policy ID
and the hot committee credential.

Rules
-----

The script enforces the following rules unconditionally:

* The purpose of the script execution must be `Spending`
* The UTxO being spent must be in the transaction's inputs (this will always be
  the case when the script is invoked by a properly implemented ledger layer).

The script enforces additional rules depending on the redeemer:

If the redeemer is a ``Vote`` action:

* The transaction produces an output that is identical to the input being
  authorized.
* The transaction does not send any other outputs to an address with the the
  script's own payment credential.
* The transaction has been signed by a majority of users from the
  **voting group** defined in the datum.
* The transaction contains a committee voting procedure:
  * The hot credential performing the vote is the same credential provided to
    the script as a parameter.
  * The ID of the Governance action being voted on matches the ID in the
    redeemer.
  * The vote matches the vote in the redeemer redeemer.
* The transaction does not contain any other votes.

If the redeemer is a ``ResignVoting`` action:

* The transaction sends an output that meets the following criteria:
  * The address matches the address of the input being authorized
  * The value of the output is the same as the value of the input being
    authorized.
  * The datum is unchanged with the exception that the resignee specified in
    the redeemer is removed from the **voting group**.
  * The reference script in the output is the same if present, otherwise not
    set.
* The transaction does not send any other outputs to an address with the the
  script's own payment credential.
* The transaction has been signed by the resignee.
* The resignee is a member of the **voting group** defined by the datum.
* The resignee is not the last member of the **voting group**, which would
  leave the group empty.
* The transaction does not contain any votes.

If the redeemer is a ``RotateHot`` action:

* The transaction sends an output that meets the following criteria:
  * The address matches the address of the input being authorized
  * The value of the output is the same as the value of the input being
    authorized.
  * the **voting group** in the output datum is not empty.
  * The output does not contain a reference script.
* The transaction does not send any other outputs to an address with the the
  script's own payment credential.
* The transaction includes a reference input that holds the cold NFT
  * The reference input contains a valid ``ColdLockDatum``.
* The transaction has been signed by a majority of users from the
  **delegation group** defined in the reference input's datum.
* The transaction does not contain any votes.

If the redeemer is an ``UnlockHot`` action:

* The transaction includes a reference input that holds the cold NFT
  * The reference input contains a valid ``ColdLockDatum``.
* The transaction has been signed by a majority of users from the
  **delegation group** defined in the reference input's datum.

Datum
-----

Main schema:

* **Type**: constructor
* **Valid constructor indexes**:
    0. * **Haskell Name**: ``HotLockDatum``
       * **Fields**:
          * Field 1:
              * **Type**: List of :ref:`Identities <identity_schema>`
              * **Haskell Name** ``votingUsers``
              * **Description**: The public key hashes and certificate hashes
                of the users in the **voting group**.

Redeemer
--------

Main schema

* **Type**: constructor
* **Valid constructor indexes**:
    0. * **Haskell Name**: ``Vote``
       * **Description**: Require the transaction to cast a committee vote.
       * **Fields**:
          * Field 1:
              * **Type**: GovernanceActionId from Plutus V3.
              * **Description**: The governance action to vote on.
          * Field 2:
              * **Type**: Vote from Plutus V3.
              * **Description**: Hot to vote.
    1. * **Haskell Name**: ``ResignVoting``
       * **Description**: Require the transaction to remove a user from the
         **voting group**.
       * **Fields**:
          * Field 1:
              * **Type**: :ref:`Identity <identity_schema>`
              * **Description**: The resignee.
    2. * **Haskell Name**: ``RotateHot``
       * **Description**: Allow the transaction to change the members of the
         **voting group**.
    3. * **Haskell Name**: ``UnlockHot``
       * **Description**: Allow the transaction to spend the NFT freely.

See :ref:`Note on UnlockCold <unlock_cold>` for comments also applicable to ``UnlockHot``.

.. warning::
   The **delegation group** has full control over the hot NFT, and consequently
   the hot credential its self. **delegation group** members should safeguard
   their keys as if they were keys for the hot credential its self.
