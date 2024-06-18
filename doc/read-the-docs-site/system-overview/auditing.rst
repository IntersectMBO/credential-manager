.. _auditing:

Auditing transactions
=====================

The credential manager system supports the ability to audit the organization's approved transactions. 
By having an accurate record of the organization's certificate structure at the time the transaction was made, you can verify that the transaction was authorized by the proper representatives from the organization.

.. tip::

   **Summary**

   - **Step 1** --- Get the certificate files from the organization.
   - **Step 2** --- Extract public keys, identify certificates, hash the PEM file, and match the hashes to verify the signatories.
   - **Step 3** --- Hash the root certificate PEM file and match it to the `certificateAuthority` field to verify the integrity.

To verify the transaction signatories
-------------------------------------

Step 1. Obtain the certificate hierarchy
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

First, you need to obtain the certificate files from the organization. 
Each organization decides how they want to publish and distribute these certificates.

Step 2. Verify the transaction signatories
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Verifying the transaction signatories focuses on validating that the right individuals authorized the transaction.

- **Extract public keys** --- From the transaction witness set, extract the public keys.

- **Identify certificates** --- Find the certificates that correspond to these public keys in the certificate hierarchy. 

   * Note: One public key hash will not be found in the certificate hierarchy because it belongs to the orchestrator. You can ignore this key.

- **Hash the PEM file** --- Use the ``sha-256`` algorithm to hash the PEM (privacy-enhanced mail) file to get a hash digest.

- **Match the hash digest** --- Check if the hash digest matches the certificate hash found alongside the public key hash in the datum. 

If the hashes match, it confirms that the signatory was authorized to act on behalf of the organization.

Step 3. Verify the CA hash
~~~~~~~~~~~~~~~~~~~~~~~~~~

Verifying the certificate authority hash focuses on validating that the structure that upholds these certificates is genuine and has not been altered.

- **Hash the certificate PEM file** --- Follow the chain of certificates up to the root self-signed certificate. Hash this certificate PEM file using the ``sha-256`` algorithm.
- **Match the hash** --- Compare this hash to the one in the `certificateAuthority` field of the cold lock datum that was in effect when the transaction occurred. 

If the hashes match, this verifies the integrity of the entire certificate hierarchy.
