.. _cold_credential_script:

Cold committee credential script
================================

NFT asset class parameter
-------------------------

The cold committee credential script requires a specific NFT asset class to function.
This asset class gets inlined as a parameter into the code of the script directly when it is compiled and initialized. 

Rules
-----

The cold committee credential script enforces two rules:

1. The purpose of the script execution must be `Certifying`.
2. The transaction must consume a UTXO which contains the the given NFT.

.. warning::
  Note that the cold committee credential script does *not* check:

  * That the certificate being authorized is a valid certificate for a cold committee credential to be authorized. 
    This check is unnecessary, as the ledger rules would not ask a cold committee credential to authorize any other type of certificate.
  * That the NFT is actually an NFT, as it doesn't have access to sufficient information to determine this. 
    For that matter, it doesn't check that only one token is spent --- in fact, the minting asset class could be the ada asset class.
    Although the tools we provide to build the credential would not allow this, the script itself doesn't forbid it. 
    It is the responsibility of the credential creator to choose an appropriate NFT.

Datum
-----

Committee script credentials do not have a datum.

Redeemer
--------

Untyped, not used. 
The most efficient is to use the integer ``0`` for the redeemer.
