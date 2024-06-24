.. _cold_nft_locking_script:

Cold NFT locking script
=======================

NFT asset class parameter
-------------------------

The cold NFT locking script requires a specific NFT asset class to function.
This asset class gets inlined as a parameter into the code of the script directly when it is compiled and initialized. 

Cold committee credential parameters
------------------------------------

The cold NFT locking script requires a specific cold committee credential to function.
This cold committee credential gets inlined as a parameter in the code of the cold NFT locking script directly when it is compiled and initialized. 

Rules
-----

The script enforces the following rules unconditionally:

* The purpose of the script execution must be `Spending`.

* The UTXO being spent must be in the transaction's inputs (this will always be the case when the script is invoked by a properly implemented ledger layer).

The script enforces the following additional rules depending on the redeemer:

Rules for ``AuthorizeHot`` actions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the redeemer is an ``AuthorizeHot`` action:

* The transaction produces an output that is identical to the input being authorized.
* The transaction does not send any other outputs to an address with the script's own payment credential.
* The transaction has been signed by a majority of users from the **delegation group** defined in the datum.
* The transaction contains a hot committee credential authorization certificate. 

  * The cold credential contained in the certificate is the same credential provided to the script as a parameter.
  * The hot credential contained in the certificate is the same credential provided in the redeemer.

* The transaction does not contain any other certificates.

Rules for ``ResignMembership`` actions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the redeemer is a ``ResignMembership`` action:

* The transaction sends an output that meets the following criteria:

  * The address matches the address of the input being authorized.
  * The value of the output is the same as the value of the input being authorized.
  * The datum is unchanged except that the resignee specified in the redeemer is removed from the **membership group**.
  * The reference script in the output is the same if present, otherwise it is not set.

* The transaction does not send any other outputs to an address with the script's own payment credential.
* The transaction has been signed by the resignee.
* The resignee is a member of the **membership group** defined by the datum.
* The resignee is not the last member of the **membership group**, which would leave the group empty.
* The transaction does not contain any certificates.

Rules for ``ResignDelegation`` actions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the redeemer is a ``ResignDelegation`` action:

* The transaction sends an output that meets the following criteria:

  * The address matches the address of the input being authorized.
  * The value of the output is the same as the value of the input being authorized.
  * The datum is unchanged except that the resignee specified in the redeemer is removed from the **delegation group**.
  * The reference script in the output is the same if present, otherwise it is not set.

* The transaction does not send any other outputs to an address with the script's own payment credential.
* The transaction has been signed by the resignee.
* The resignee is a member of the **delegation group** defined by the datum.
* The resignee is not the last member of the **delegation group**, which would leave the group empty.
* The transaction does not contain any certificates.

Rules for ``ResignCold`` actions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the redeemer is a ``ResignCold`` action:

* The transaction produces an output that is identical to the input being authorized.
* The transaction does not send any other outputs to an address with the script's own payment credential.
* The transaction has been signed by a majority of users from the **membership group** defined in the datum.
* The transaction contains a cold committee resignation certificate.

  * The cold credential contained in the certificate is the same credential provided to the script as a parameter.

* The transaction does not contain any other certificates.

Rules for ``RotateCold`` actions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the redeemer is a ``RotateCold`` action:

* The transaction sends an output that meets the following criteria:

  * The address matches the address of the input being authorized.
  * The value of the output is the same as the value of the input being authorized.
  * The **certificate authority** is the same in the output datum as it is in the input datum.
  * The **membership group** in the output datum is not empty.
  * The **delegation group** in the output datum is not empty.
  * The output does not contain a reference script.

* The transaction does not send any other outputs to an address with the script's own payment credential.
* The transaction has been signed by a majority of users from the **membership group** defined in the input datum.
* The transaction has been signed by all members of the **membership group** in the output datum that are not in the input datum.
* The transaction has been signed by all members of the **delegation group** in the output datum that are not in the input datum.
* The transaction does not contain any certificates.

Rules for ``BurnCold`` actions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the redeemer is an ``BurnCold`` action:

* The transaction has been signed by a majority of users from the **membership group** defined in the input datum.
* All outputs from the transaction do not contain the cold NFT.
* The transaction does not contain any certificates.

Rules for ``UpgradeCold`` actions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the redeemer is an ``UpgradeCold`` action:

* The transaction has been signed by a majority of users from the **membership group** defined in the input datum.
* 1 cold NFT is sent to the upgrade destination script.
* No other outputs contain the cold NFT.
* The transaction does not contain any certificates.

Datum
-----

Main schema
~~~~~~~~~~~

* **Type**: constructor
* **Valid constructor indexes**:
    0. * **Haskell Name**: ``ColdLockDatum``
       * **Fields**:
          * Field 1:
              * **Type**: :ref:`Identity <identity_schema>`
              * **Haskell Name** ``certificateAuthority``
              * **Description**: The public key hash and certificate hash of the certificate authority which issued all child X.509 certificates in the datum.
          * Field 2:
              * **Type**: List of :ref:`Identities <identity_schema>`
              * **Haskell Name** ``membershipUsers``
              * **Description**: The public key hashes and certificate hashes of the users in the **membership group**.
          * Field 3:
              * **Type**: List of :ref:`Identities <identity_schema>`
              * **Haskell Name** ``delegationUsers``
              * **Description**: The public key hashes and certificate hashes of the users in the **delegation group**.

.. _identity_schema:

``Identity`` schema:

* **Type**: constructor
* **Valid constructor indexes**:
    0. * **Haskell Name**: ``Identity``
       * **Fields**:
          * Field 1:
              * **Type**: ByteString
              * **Haskell Name** ``pubKeyHash``
              * **Description**: A hash of the user's public key.
          * Field 2:
              * **Type**: ByteString
              * **Haskell Name** ``certificateHash``
              * **Description**: A SHA-256 hash of the user's X.509 certificate
                PEM file.

Redeemer
--------

Main schema
~~~~~~~~~~~

* **Type**: constructor
* **Valid constructor indexes**:
    0. * **Haskell Name**: ``AuthorizeHot``
       * **Description**: Require the transaction to authorize a hot credential.
       * **Fields**:
          * Field 1:
              * **Type**: HotCommitteeCredential from Plutus V3.
              * **Description**: The hot credential being authorized.
    1. * **Haskell Name**: ``ResignCold``
       * **Description**: Require the transaction to resign from the committee.
    2. * **Haskell Name**: ``ResignMembership``
       * **Description**: Require the transaction to remove a user from the **membership group**.
       * **Fields**:
          * Field 1:
              * **Type**: :ref:`Identity <identity_schema>`
              * **Description**: The resignee.
    3. * **Haskell Name**: ``ResignDelegation``
       * **Description**: Require the transaction to remove a user from the **delegation group**.
       * **Fields**:
          * Field 1:
              * **Type**: :ref:`Identity <identity_schema>`
              * **Description**: The resignee.
    4. * **Haskell Name**: ``RotateCold``
       * **Description**: Allow the transaction to change the members of the **membership group** and **delegation group**.
    5. * **Haskell Name**: ``BurnCold``
       * **Description**: Require the transaction to burn the NFT.
    6. * **Haskell Name**: ``UpgradeCold``
       * **Description**: Require the transaction to send the NFT to a new script address.
       * **Fields**:
          * Field 1:
              * **Type**: ScriptHash from Plutus V3.
              * **Description**: The script that will receive the NFT.

.. warning::
   The **membership group** has full control over the cold NFT, and consequently the cold credential itself. 
   **membership group** members should safeguard their keys as if they were keys for the cold credential itself.
