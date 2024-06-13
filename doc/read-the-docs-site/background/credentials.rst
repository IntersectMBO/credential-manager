.. _cardano_credentials:

Public key credentials versus script credentials 
================================================

On Cardano, anyone can verify the legitimacy of a transaction on the blockchain by using a credential. 
Credentials are used for many purposes in the Conway era, including:

* Consuming UTXOs
* Withdrawing staking rewards
* Publishing governance certificates
* Voting.

There are two types of credentials: 

1. Public key credentials, and 
2. Script credentials.

Public key credentials
------------------------

Public key credentials use `public-key cryptography <https://en.wikipedia.org/wiki/Public-key_cryptography>`_ to prove that the appropriate party or parties have authorized a transaction. Public-key cryptography involves a pair of keys: 

* A **public key**, which can be shared openly, and 
* A **private key**, which must be kept secret.

When a transaction needs to be authorized by someone who holds a **public key credential**, that person must sign the transaction by generating a cryptographic signature using their private key and attaching that signature to the transaction. 

A signature is a cryptographic digest of the transaction that is produced using the public key's associated private key. This digest, or signature, uniquely represents the transaction and the private key used to sign it.

The signature attached to the transaction proves that the transaction was authorized by the owner of the public key credential. Because the private key is kept secret, only the owner of the corresponding private key could have generated the signature.

Anyone can use the public key to verify the signature. During verification, the public key is used to decrypt the signature, producing a digest. If this digest matches a freshly generated digest of the transaction, it confirms that the signature was created using the corresponding private key and that the transaction has not been tampered with. 

Advantages of public key credentials
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Public key credentials offer several advantages:

* Easy to create
* High security if generated with a secure source of randomness and managed properly
* Cheap to verify and small in size, reducing transaction fees compared to script credentials
* Based on the mature and standard Ed25519 cryptographic algorithm.

Disadvantages of public key credentials
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

However, they also have drawbacks:

* Single points of failure. If the private key is lost or stolen, the
  legitimate owner cannot authorize transactions or prevent theft. 
* Insufficient for scenarios requiring authorization from multiple individuals.
* Opaque on their own, lacking identifiable information. This can be an advantage for privacy but a disadvantage when transparency is needed.
* Difficult to change if compromised. All UTXOs owned by the compromised key must be sent to a new one, incurring ada fees. Other credentials, like constitutional committee cold credentials, are even harder to change. 

All of these drawbacks make public key credentials generally inappropriate for organizations that 
distribute and delegate authority to staff or other types of trustees.

Script credentials
-------------------

Script credentials are executable programs that encode custom authorization rules. 
These are also informally referred to as 'smart contracts.'
To authorize a transaction, the smart contract receives a summary of the transaction it is authorizing and executes it. 
If the program terminates without raising an exception, it is considered to have authorized the transaction. 

Native scripts and Plutus scripts 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Though many scripting languages target Cardano, there are only two types of script credentials: native scripts and Plutus scripts. 

* **Native scripts**. With native scripts, you can set up simple rules like requiring signatures, setting deadlines, and combining conditions using 'all of' or 'any of' options. 

* **Plutus scripts**. Plutus scripts are programs compiled to Untyped Plutus Core, a low-level variant of lambda calculus, that serves as a compilation target for other higher-level scripting languages such as PlutusTx or Aiken.

Description of a Plutus script example 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As an example, a Plutus script can be used to control how a UTXO is spent. 

The UTXO is held by an address containing the script's payment credential, which is a cryptographic hash of the script's serialized representation. 
It is also held alongside a piece of data (the *datum*) which can be used to represent the 'state' of the script. 

To spend the UTXO, the spending transaction must include the actual script bytes, either directly or via a reference to a script stored elsewhere on chain. 
The transaction can also provide an argument to the script, called a *redeemer*. 
The script is then executed with the datum held in the UTXO, the redeemer provided in the spending transaction, and a summary of the spending transaction called the *script context*. 

If it executes without raising an exception, the UTxO is authorized for being spent.

Benefits of script credentials
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Script credentials offer many benefits over public key credentials:

* They can encode sophisticated security requirements, such as requiring multiple signatures, limiting asset withdrawal amounts, and enforcing time limits.
* They can be associated with supplemental information via the datum, which can be used for identity verification.
* They can include failsafe options to allow credential recovery if auxiliary data, such as private keys, are compromised.

Disadvangates of script credentials
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

However, they also have several disadvantages:

* They are complicated and expensive to create and use compared with public key credentials.
* If not carefully written and tested, they can have unintended edge cases or bugs that compromise security.
* They consume more space in transactions and are more expensive to validate due to the script bytes, datum, and redeemer. This increases transaction fees.
* They require significant upfront investment, especially for high-risk applications, and are costly to operate, necessitating higher transaction fees and technical expertise, either from trained staff or commercial service providers who may charge additional fees.

Ultimately, script credentials can solve many of the shortcomings of public key
credentials, but they do so at a cost. They can require significant upfront
investment to create, especially if they are used for high-risk applications,
which they often are. 

They are also expensive to operate, requiring both higher
transaction fees and an operator with significant technical knowledge (either a
trained staff member or a commercial service provider which may charge
additional fees).
