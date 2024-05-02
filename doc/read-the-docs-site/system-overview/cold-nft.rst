.. _cold_nft_locking_script:

Cold NFT Locking Script
=======================

Parameters
----------

The cold NFT locking script is parameterized by the cold committee credential.

Rules
-----

The script enforces the following rules unconditionally:

* The purpose of the script execution must be `Spending`
* The UTxO being spent must be in the transaction's inputs (this will always be
  the case when the script is invoked by a properly implemented ledger layer).

The script enforces additional rules depending on the redeemer:

If the redeemer is an ``AuthorizeHot`` action:

* The transaction produces an output that is identical to the input being
  authorized.
* The transaction does not send any other outputs to an address with the the
  script's own payment credential.
* The transaction has been signed by a majority of users from the
  **delegation group** defined in the datum.
* The transaction contains a hot committee credential authorization certificate
  * The cold credential contained in the certificate is the same credential
    provided to the script as a parameter.
  * the hot credential contained in the certificate is the same credential
    provided in the redeemer.
* The transaction does not contain any other certificates.

If the redeemer is a ``ResignDelegation`` action:

* The transaction sends an output that meets the following criteria:
  * The address matches the address of the input being authorized
  * The value of the output is the same as the value of the input being
    authorized.
  * The datum is unchanged with the exception that the resignee specified in
    the redeemer is removed from the **delegation group**.
  * The reference script in the output is the same if present, otherwise not
    set.
* The transaction does not send any other outputs to an address with the the
  script's own payment credential.
* The transaction has been signed by the resignee.
* The resignee is a member of the **delegation group** defined by the datum.
* The resignee is not the last member of the **delegation group**, which would
  leave the group empty.
* The transaction does not contain any certificates.

If the redeemer is a ``ResignCold`` action:

* The transaction produces an output that is identical to the input being
  authorized.
* The transaction does not send any other outputs to an address with the the
  script's own payment credential.
* The transaction has been signed by a majority of users from the
  **membership group** defined in the datum.
* The transaction contains a cold committee resignation certificate.
  * The cold credential contained in the certificate is the same credential
    provided to the script as a parameter.
* The transaction does not contain any other certificates.

If the redeemer is a ``RotateCold`` action:

* The transaction sends an output that meets the following criteria:
  * The address matches the address of the input being authorized
  * The value of the output is the same as the value of the input being
    authorized.
  * The **certificate authority** is the same in the output datum as it is in
    the input datum.
  * the **membership group** in the output datum is not empty.
  * the **delegation group** in the output datum is not empty.
  * The output does not contain a reference script.
* The transaction does not send any other outputs to an address with the the
  script's own payment credential.
* The transaction has been signed by a majority of users from the
  **membership group** defined in the input datum.
* The transaction does not contain any certificates.

If the redeemer is an ``UnlockCold`` action:

* The transaction has been signed by a majority of users from the
  **membership group** defined in the input datum.

Datum
-----

Main schema:

* **Type**: constructor
* **Valid constructor indexes**:
    0. * **Haskell Name**: ``ColdLockDatum``
       * **Fields**:
          * Field 1:
              * **Type**: :ref:`Identity <identity_schema>`
              * **Haskell Name** ``certificateAuthority``
              * **Description**: The public key hash and certificate hash of the
                certificate authority which issued all child X.509 certificates in
                the datum.
          * Field 2:
              * **Type**: List of :ref:`Identities <identity_schema>`
              * **Haskell Name** ``membershipUsers``
              * **Description**: The public key hashes and certificate hashes
                of the users in the **membership group**.
          * Field 3:
              * **Type**: List of :ref:`Identities <identity_schema>`
              * **Haskell Name** ``delegationUsers``
              * **Description**: The public key hashes and certificate hashes
                of the users in the **delegation group**.

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
    2. * **Haskell Name**: ``ResignDelegation``
       * **Description**: Require the transaction to remove a user from the
         **delegation group**.
       * **Fields**:
          * Field 1:
              * **Type**: :ref:`Identity <identity_schema>`
              * **Description**: The resignee.
    3. * **Haskell Name**: ``RotateCold``
       * **Description**: Allow the transaction to change the members of the
         **membership group** and **delegation group**.
    4. * **Haskell Name**: ``UnlockCold``
       * **Description**: Allow the transaction to spend the NFT freely.

.. _unlock_cold:

Note on ``UnlockCold``
----------------------

It is reasonable to ask why, if the membership group can spend the NFT without
restriction anyway, is it necessary to include more specific actions such as
``RotateCold`` or ``ResignCold``? The answer is that the unlock action
is very dangerous, as it does not check anything beyond that the transaction is
signed. If a transaction does something unintended with the NFT while unlocking
it, it could render the cold credential unusable or worse, give that control to
someone else. So the more restrictive actions are available to cover known use
cases and provide additional safety guarantees not provided by the unlock
action.

It would also be reasonable to ask why the unlock action is available if it is
so dangerous? The answer is that not including it is also dangerous. Consider
what would happen if a security flaw was found in the cold NFT locking script.
If the unlock action wasn't available, there would be no way to send the NFT to
a patched version of the script, because all other actions require the NFT to
be sent back to the address from which it originated. The only way to prevent
the security flaw from being exploited would be to resign from the committee,
which is irrecoverable without an election, a process beyond the ability of the
committee member to control.

.. warning::
   As mentioned before, the **membership group** has full control over the cold
   NFT, and consequently the cold credential its self. Refer to the warning in
   :ref:`system_overview` for guidelines to mitigate this risk.
