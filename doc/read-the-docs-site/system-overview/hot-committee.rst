.. _hot_credential_script:

Hot committee credential script
================================

NFT minting policy ID parameter 
-------------------------------

The hot committee credential script requires a specific NFT minting policy ID to function.
This minting policy ID is directly inlined as a parameter into the script's code when it is compiled and initialized. 

Rules
-----

The hot committee credential script enforces two rules: 

1. The purpose of the script execution must be `Voting`
2. The transaction must consume a UTXO containing a token with the given NFT minting policy ID.

.. warning::
  Note that the hot committee credential script does *not* check:

  * Details of the vote, including the identity of the voter.
    This check is unnecessary, as the ledger rules would not ask a hot committee credential to authorize votes by other voters.
  * That the NFT is actually an NFT, as it doesn't have access to sufficient information to determine this. 
    For that matter, it doesn't check that only one token is spent --- in fact the minting policy ID could be the ada minting policy ID (though the tools we provide to build the credential would not allow this, the script its self doesn't forbid it). 
    It is the responsibility of the credential creator to choose an appropriate NFT.

Datum
-----

Committee script credentials do not have a datum.

Redeemer
--------

Untyped, not used. The most efficient is to use the integer ``0`` for the redeemer.
