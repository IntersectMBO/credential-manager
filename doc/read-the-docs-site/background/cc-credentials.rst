.. _cc_credentials:

Constitutional Committee Credentials
====================================

Having covered credentials in general, let's take a closer look at
Constitutional Committee credentials. Members of the constitutional committee
have two credentials to manage: a cold credential and a hot credential.

Cold Credentials
----------------

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
The cold credential is used to authorize transactions that publish two types of
certificates:

1. Hot credential authorization certificates, which have the effect of
   registering a hot credential to vote on behalf of the cold credential.
2. Committee resignation certificates, which have the effect of removing the
   member from the committee.

Both of these are exceptional events which should occur infrequently, if at all
(a hot credential must be authorized at least once to allow for voting).

A cold credential can be added to the committee in one of two ways:

1. A governance action proposing to add it to the committee is ratified and
   enacted.
2. It is hard-coded in the Conway Genesis configuration file.

Both of these processes are beyond the control of the bearer of the cold
credential. In the first case, it is up to the electorate (DReps and SPOs) of
Cardano whether or not the governance action is ratified. In the second case,
there is only one Conway Genesis file, and it cannot be altered (though in
theory new committee members *could* be injected in a subsequent hard-fork
event, though this is highly unlikely, and would still be subject to approval
from the DReps and SPOs).

This makes the cold credential extremely security critical, as losing control
over it can only be remedied by the member being voted out of the committee in
the best case, or the entire committee being disbanded via a no confidence
governance action in the worst case. Either way, the member is unlikely to be
re-elected after such a display of negligence. One of the purposes of this
credential management system is to provide additional layers of security which
help prevent this sort of situation and give better options for recovery if
necessary.

Hot Credentials
---------------

The hot credential is required to vote as a committee member. It is the
"workhorse" credential, in that it is expected to be used frequently for
everyday business. This is in contrast to the cold credential, which is expected
to be used only in exceptional circumstances. A hot credential must be
authorized by the cold credential, and also appears in the committee state
queried from a Cardano node:

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

This example shows the same committee member as before, only this time they
have authorized a hot credential, namely
``{ "keyHash": "e394a160b9f1345b7b84cd25f0a3f1d12cb0c9835a4167f7dc5b52ca" }``.

Compared with the cold credential, a hot credential is relatively cheap to
replace: when a new hot credential is authorized by the cold credential, it
invalidates the existing hot credential, taking its place. This is entirely
within the control of the bearer of the cold credential, and comes into effect
as soon as the authorization certificate is published on chain via a
transaction. That notwithstanding, it is still undesirable to replace the hot
credential if it can be avoided. This is because doing so requires using the
cold credential, which is meant to be used sparingly - ideally never after the
hot credential is authorized. Replacing hot credentials frequently is also
highly visible activity and may negatively impact the reputation of the
committee member within the community - remember, the community has the power
to depose individual committee members or the whole committee if they lose
faith in them. This system also offers greater flexibility for managing the hot
credential without having to replace it.

Note on Terminology
-------------------

The names "hot" and "cold" come from the terms "hot key" and "cold key". A cold
key is a key that is only used to authorize a hot key to act on its behalf, but
is otherwise kept in "cold storage" (e.g. on a computer physically separated
from the internet, or a non-digital medium) for security purposes. The hot
credential is used to conduct everyday business, and may be kept in "hot" or
"live" storage so that it is easier to access. The benefit of such a setup is
that it offers many of the security benefits of keeping keys in cold storage
while mitigating the drawbacks of doing so - namely it is not convenient to use
the cold key frequently.

Analogously, the cold committee credential is used only for hot credential
authorization and committee resignation, both of which are exceptional
activities, while the hot credential is used for everyday LOB work. Indeed, if
public key credentials are used for both, the analogy is perfect. However, the
analogy breaks down somewhat when script credentials are used, as there is no
notion of keeping private keys in hot or cold storage with script credentials.
The concept is still applicable though: The cold credential is used rarely, the
hot credential is used often. Therefore, any private data needed to activate
the cold credential should be treated like a cold private key and kept in cold
storage. For example, if the cold credential requires the transaction to be
signed by multiple private keys, as is the case with this system, those private
keys should be treated like cold keys, because that is what they are.
