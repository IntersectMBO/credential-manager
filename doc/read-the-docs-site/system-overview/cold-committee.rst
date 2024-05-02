.. _cold_credential_script:

Cold Committee Credential Script
================================

Parameters
----------

A script parameter is a value which gets inlined into the code of the script
directly when it is compiled / initialized.

The cold committee credential script is parameterized by the minting policy ID
of an NFT.

Rules
-----

The script enforces two rules:

* The purpose of the script execution must be `Certifying`
* The transaction must consume a UTxO which contains a token with the given NFT
  minting policy ID.

.. warning::
  Note that it does not check:

  * That the certificate being authorized is a valid certificate for a cold
    committee credential to authorized. This check is unnecessary, as the ledger
    rules would not ask a cold committee credential to authorize any other type
    of certificate.
  * That the NFT is actually an NFT, as it doesn't have access to sufficient
    information to determine this. For that matter, it doesn't check that only
    one token is spent - in fact the minting policy ID could be the ADA minting
    policy ID (though the tools we provide to build the credential would not
    allow this, the script its self doesn't forbid it). It is the responsibility
    of the credential creator to choose an appropriate NFT.

Datum
-----

Committee script credentials do not have a datum.

Redeemer
--------

Untyped, not used. The most efficient is to use the integer ``0`` for the
redeemer.
