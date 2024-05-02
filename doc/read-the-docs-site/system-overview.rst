.. _system_overview:

System Overview
###############

The obvious job of the credential management system is to manage credentials.
But what are the credentials being managed in this context? The term
"credentials" is not referring to private information required to access secure
data like passwords or private cryptography keys. The system is *not* a
password manager, nor is it a repository of private keys. Rather, it is a
collection of smart contracts that are used to create and manage the use of
*Constitutional Committee credentials*. Constitutional committee credentials
define who is a member of the constitutional committee and control who is able
to act as that member and cast a vote on a governance action.

Constitutional Committee Credentials
************************************

On Cardano, there are two types of credential associated with an individual
member of the constitutional committee: the **cold credential** and the **hot
credential**.

The cold credential is used to identify committee members, and can be thought
of as a sort of "primary" credential. The cold credential is what you see if
you query the current committee state from a Cardano node:

.. code-block:: bash

   $ cardano-cli conway query committee-state
   {
      "committee": {
         "keyHash-5e86313210aef13297187c38c98abb632678906230f555a1bf0a647a": {
               "expiration": 100,
               "hotCredsAuthStatus": {
                  "tag": "MemberNotAuthorized"
               },
               "nextEpochChange": {
                  "tag": "NoChangeExpected"
               },
               "status": "Active"
         }
      },
      "epoch": 16,
      "threshold": 0.67
   }

This example shows a committee with one member identified by the cold
credential ``keyHash-5e86313210aef13297187c38c98abb632678906230f555a1bf0a647a``.
The cold credential is able to resign the member from the committee, as well as
authorize a hot credential.

A cold credential can be added to the committee in one of two ways:

1. A governance action proposing to add it to the committee is ratified and
   enacted.
2. It is hard-coded in the Conway Genesis configuration file.

The hot credential is used to vote as a committee member. It is the "workhorse"
credential, in that it is expected to be used frequently for everyday business.
This is in contrast to the cold credential, which is expected to be used only
in exceptional circumstances. A hot credential must be authorized by the cold
credential, and also appears in the committee state queried from a Cardano
node:

.. code-block:: bash

   $ cardano-cli conway query committee-state
   {
      "committee": {
         "keyHash-5e86313210aef13297187c38c98abb632678906230f555a1bf0a647a": {
               "expiration": 100,
               "hotCredsAuthStatus": {
                  "contents": {
                     "keyHash": "e394a160b9f1345b7b84cd25f0a3f1d12cb0c9835a4167f7dc5b52ca"
                  },
                  "tag": "MemberAuthorized"
               },
               "nextEpochChange": {
                  "tag": "NoChangeExpected"
               },
               "status": "Active"
         }
      },
      "epoch": 16,
      "threshold": 0.67
   }

This example shows the same committee as before, only this time the single
member has authorized a hot credential, namely
``{ "keyHash": "e394a160b9f1345b7b84cd25f0a3f1d12cb0c9835a4167f7dc5b52ca" }``.

The names "hot" and "cold" come from the terms "hot key" and "cold key". A cold
key is a key that is only used to authorize a hot key to act on its behalf, but
is otherwise kept in "cold storage" (e.g. on a computer physically separated
from the internet, or a non-digital medium) for security purposes. The hot
credential is used to conduct everyday business, and may be kept in "hot" or
"live" storage so that it is easier to access. The benefit of such a setup is
that the cold key can be used to replace the hot key if it becomes comprimised,
which parallels the way a CC cold credential can replace a hot credential by
authorizing a new one.


Credential Types
****************

Because Cardano is a programmable blockchain, constitutional committee member
credentials (and other credentials in general) are not limited to cryptographic
key pairs; they can also be executable scripts. When a transaction requires
authorization from a credential, the author of that transaction must include a
proof of authorization from the owner of the credential. This proof is called a
witness. The common example of this is spending a UTxO. In order to spend a
UTxO owned by an address, the transaction must be authorized by the address's
payment credential. In the case of a CC credentials, a transaction must be
authorized by the cold credential if it contains a hot credential authorization
or committee resignation certificate, or by the hot credential if it contains a
vote from the committee member.

A key credential is a simple, fixed-functionality credential. To authorize a
transaction with a key credential, the private key is used to produce a
signature for the transaction. The signature can be publicly verified using the
corresponding public key. While key pairs can be used for CC credentials, doing
so presents several security and organizational problems:

* It is a single point of failure. If the private key is stolen or lost, the
  committee member will lose control of their committee membership and may be
  impersonated by malicious actors.
* Key pairs are opaque and not tied to a verifiable identity.
* A key cannot be easily changed, particularly the cold key, which requires a
  vote from DReps and SPOs to change.
* It is not compatible with enterprise setups where the responsibilities of the
  committee member may be delegated to a group of individuals, the makeup of
  which may change as a result of organization changes.

For this reason, script credentials are preferred to key credentials for the
constitutional committee. A script credential authorizes a transaction by being
executed with access to information about the transaction. If it executes
without producing an error, the transaction is authorized. This system uses
scripts to enforce several custom rules, including:

* A transaction must be signed by multiple key holders (multi-signature).
* The group of keys that must sign the transaction depends on what the
  transaction is doing (separation of responsibilities).
* Keys can be rotated (changed) if necessary.
* Keys must be accompanied by the hash of an X.509 certificate which provides
  verifiable identity.

All of which address the shortcomings of key pair credentials listed above.

The Committee Credential Scripts
********************************

The system produces a cold credential script and a hot credential script.
Hashes of these scripts are used to produce the cold and hot committee
credentials. While it is tempting to embed the access rules directly in these
scripts, doing so presents a few problems:

* Unlike payment credential scripts, other types of scripts cannot be
  accompanied by a datum. This means that all configuration must be hard-coded
  into the script, making features like key rotation imposible.
* It sets the control logic in stone. If, for example, a security vulnerability
  was found in the script, it would be impossible to patch and upgrade it.

To address this issue, a layer of indirection is employed: each script checks
that a specific NFT is spent in the transaction being authorized. If the
transaction is able to spend the NFT, that is sufficient to satisfy the
credential script. The control logic is instead implemented by another pair of
scripts, which act as payment credentials for the NFT. The effect of this
indirection is to transform a committee credential authorization into a payment
credential authorization, which is more flexibile.

The NFT Locking Scripts
***********************

The system produces two companion scripts: one to lock the cold credential NFT,
and another to lock the hot credential NFT.

Cold NFT Locking Script
=======================

The cold locking script requires the NFT to be accompanied by a datum which
contains:

* The public key hash and certificate hash of the certificate authority (CA)
  used to sign all the other certificates.
* A list of public key and certificate hashes that form the *membership* group.
* A list of public key and certificate hashes that form the *delegation* group.

The CA hashes have no effect on on-chain logic, they are used to verify the
identities of all other users listed in the datum. The *membership*  and
*delegation* groups are required to sign transactions that spend the NFT under
different circumstances. Namely:

* Transactions that authorize a hot credential must be signed by the
  *delegation* group.
* Transactions that resign from the committee must be signed by the *membership*
  group.
* Transactions that rotate membership and delegation keys must be signed by the
  *membership* group.
* Individual *delegation* group members may sign a transaction on their own to
  remove themselves from the delegation group.
* The *membership* group can sign a transaction to completely unlock the cold
  NFT, allowing it to be spent without restrictions.

It is reasonable to ask why, if the membership group can spend the NFT without
restriction anyway, is it necessary to include more specific actions such as
key rotation or committee resignation? The answer is that the unlock action
is very dangerous, as it does not check anything beyond that the transaction is
signed. If a transaction does something unintended with the NFT while unlocking
it, it could render the cold credential unusable or worse, give that control to
someone else. So the more restricive actions are available to cover known use
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

Hot NFT Locking Script
======================

The hot locking script requires the NFT to be accompanied by a datum which
contains a list of public key and certificate hashes that form the *voting*
group. Additionally, if the transaction must be signed by the *delegation*
group, the transaction must include the current output of the cold NFT locking
script as a reference input.

The script requires the following:

* Transactions that cast votes must be signed by the *voting* group.
* Transactions that rotate voting keys must be signed by the *delegation* group.
* Individual *voting* group members may sign a transaction on their own to
  remove themselves from the voting group.
* The *delegation* group can sign a transaction to completely unlock the hot
  NFT, allowing it to be spent without restrictions.

A Note On Key Security
======================

The astute reader will notice that the role of the *voting* group mirrors that
of the *delegation* group in the cold NFT locking script, and the role of the
*delegation* group mirrors that of the *membership* group. The *membership*
group is said to own the cold NFT and delegates to the *delegation* group. The
*delegation* group in turn owns the hot NFT and delegates to the *voting*
group. As a consequence, the *membership* group is the group with the most
power over the committee member's actions. It is their keys which present the
greatest security risk to the whole system. If over half of the *membership*
group lose control of their signing keys, control over the entire membership in
the committee is jeopardized. This should not come as a surprise - the
*membership* group fully controls the cold credential, which is the primary
committee membership credential. It is therefore imperative that the
*membership* group **keeps their keys secure, preferably in cold storage**.
Luckily, the *membership* group has very little they need to do on a day-to-day
basis compared with the *delegation* and particularly *voting* groups, so it is
not as important that their keys be readily accessible.
