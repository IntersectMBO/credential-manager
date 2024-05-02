.. _cardano_credentials:

Cardano Credentials
===================

On Cardano, a credential is a piece of information that anyone can use to
verify the legitimacy of changes to the blockchain described by a transaction.
Credentials are used for many purposes in the Coway era, including:

* Consuming UTxOs (Payment credentials)
* Withdrawing staking rewards (staking credentials)
* Publishing governance certificates (DRep, Cold Committee credentials)
* Voting (DRep, Hot Committee credentials)

There are two types of credentials: public key credentials and script
credentials.

Public Key Credentials
----------------------

Public Key Credentials use
`public-key cryptography <https://en.wikipedia.org/wiki/Public-key_cryptography>`_
to prove that transactions were authorized by the appropriate parties. If a
transaction requires authorization from the bearer of a public key credential,
it must be **signed** by them. To sign a transaction, the owner of the public
key attaches a **signature** to the transaction. A signature is a cryptographic
digest of the transaction produced using the public key's associated **private
key**. The signature proves that the transaction was witnessed by the
credential owner, and the public key can be used by anyone to verify the
signature. There are several advantages to public key credentials:

* They are easy to create
* Provided that a secure source of randomness was used to generate the key
  pair, and that the private key is managed securely, they provide quite good
  security without much room for exploitation.
* They are cheap to verify and small in size, all of which lowers transaction
  fees significantly compared with script credentials.
* They are based on a mature and standard cryptographic algorithm (Ed25519).

However, they also have drawbacks:

* They are single points of failure. If the private key is lost or stolen, the
  legitimate owner of the credential will be unable to authorize transactions,
  nor will they be able to stop theives from impersonating them.
* They are not sufficient for setups requiring authorization from multiple
  individuals.
* They are opaque on their own - they do not contain identifiable information.
  Depending on the application, this can also be an advantage, but not when
  transparency is desired.
* They may be difficult to change if a credential is compromised. For example,
  all UTxOs owned by a payment key would need to be sent to the new one,
  costing the owner ADA in fees. Other types of credentials, particularly
  constitutional committee cold credentials, are far more difficult to change.

All of these drawbacks make public key credentials generaly inappropriate for
organizations which distribute and delegate authority to staff or other
types of trustees.

Script Credentials
------------------

Script credentials are executable programs which encode custom authorization
rules. These are also informally referred to as "smart contracts." To authorize
a transaction, the program receives a summary of the transaction it is
authorizing and is executed. If the program terminates without raising an
exception, it is considered to have authorized the transaction. Though there
are many scripting languages which target Cardano, fundamentally there are only
two types of script credentials: native scripts and Plutus scripts. Native
scripts are able to enforce simple rules such as requiring signatures,
enforcing time limits, and conjunctive and disjunctive combinations thereof,
i.e. "all of" and "any of" combinations, respectively. Plutus scripts are
programs compiled to Untyped Plutus Core, a low-level variant of lambda
calculus which serves as a compilation target for other higher level scripting
languages such as PlutusTx or Aiken.

As an example, a script can be used to control how a UTxO is spent. The UTxO
is held by an address containing the script's payment credential, which is a
cryptographic hash of the script's serialized representation. It is also held
alongside a piece of data which can be used to represent the "state" of the
script. To spend the UTxO, the spending transaction must include the actual
script bytes, either directly or via a reference to a script stored elsewhere
on chain. The transaction can also provide an argument to the script, called a
*redeemer*. The script is then executed with the datum held in the UTxO, the
redeemer provided in the spending transaction, and a summary of the spending
transaction, called the *script context*. If it executes without raising an
exception, the UTxO is authorized for being spent.

Script credentials offer many benefits over public key credentials:

* They can encode arbitrarily sophisticated security requirements, such as
  requiring multiple signatures, limiting the amount of an asset that is
  withdrawn, enforcing time limits, and much more.
* Via the datum, they can be associated with supplemental information, which
  can be used for identity verification.
* They can have failsaves built into them to allow recovery of the credential
  if auxilliary data such as private keys are compromised.

However, they also have several disadvantages:

* They are complicated and expensive to create and used compared with public
  key credentials.
* If they are not carefully written and tested, there can be unintended edge
  cases or bugs which compromise their security.
* Between the script bytes, the datum and the redeemer, they consume a lot of
  space in transactions, and are expensive to validate compared with public
  keys and signatures. This makes them more expensive, as these factors
  increase transaction fees.

Ultimately, script credentials can solve many of the shortcomings of public key
credentials, but they do so at a cost. They can require significant upfront
investment to create, especially if they are used for high-risk applications,
which they often are. They are also expensive to operate, requiring both higher
transaction fees and an operator with significant technical knowledge (either a
trained staff member or a commercial service provider which may charge
additional fees).
