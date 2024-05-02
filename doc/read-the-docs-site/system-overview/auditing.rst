.. _auditing:

Auditing Transactions
=====================

This system makes it possible to audit transactions authorized by the
organization. Anyone with access to an accurate snapshot of the certificate
hierarchy at the time the transaction was published will be able to verify that
authorization was given by duly appointed representatives of the organization.

Obtaining the Certificate Hierarchy
-----------------------------------

The first step to verification is to obtain the certificate files from the
organization. It is up to them how they wish to publish / distribute these.


Verifying the Transaction Signatories
-------------------------------------

The next step is to extract the public keys from the transaction witness set
and find the certificates which identify them in the certificate hierarchy.
Note there will be at least one public key hash not in the certificate
hierarchy, belonging to the orchestrator. This can be ignored. Once the
corresponding certificate is found, the PEM file can be hashed with the
``sha-256`` algorithm to obtain a hash digest. If this hash digest matches the
certificate hash found alongside the public key hash in the datum, the
signatory was properly authorized to act on behalf of the organization.

Verifying the CA Hash
---------------------

Following the chain of certificates to the root self-signed certificate, hash
the certificate PEM file with ``sha-256``. If it matches the hash in the
`certificateAuthority` field of the cold lock datum that was in effect when the
transaction occurred, the integrity of the entire hierarchy can be verified.
