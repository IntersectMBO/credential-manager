.. _cc_credentials:

Balancing security and accessibility with cold and hot credentials
==================================================================

Constitutional Committee members maintain operational integrity by managing two credentials: a cold credential and a hot credential.

One of the purposes of this credential management system is to provide multiple layers of security to help committee members prevent losing control over a cold credential and to give good options for recovery if necessary. 

Before looking more closely at details, let's briefly put this discussion into a practical context. 

Querying the committee state
-------------------------------------------

Querying the current committee state from a Cardano node is a standard procedure for those involved in the governance and maintenance of the Cardano network. 
It is technical step performed to interact with and verify the state of the blockchain regarding committee membership and credential status. 

The purpose of querying the committee state includes the following: 

- **Checking membership.** By querying the committee state, you can verify which members are currently part of the committee by checking their cold credentials.
- **Confirming credential status.** It allows you to check the status of both hot and cold credentials, such as whether a hot credential is authorized to vote on behalf of a cold credential.
- **Validating changes.** After performing certain actions, like authorizing a hot credential or submitting a resignation, querying the state helps confirm that these changes have taken effect on-chain.
- **Monitoring expiration.** The query can show you the expiration of credentials, which is crucial for maintaining active participation in the committee's activities.

Typically done by the Orchestrator role
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Typically, someone in the orchestrator role would be querying the current committee state. 
The Orchestrator is one of the operational roles within the Constitutional Committee Credential Management System. 

The orchestrator role requires a deep understanding of the credential management system and the ability to execute commands using the Cardano CLI. 

Cold credentials
----------------

Given that context, let's take a closer look at some details about the cold credential. 

Think of the cold credential as a "primary" credential that's used for identifying committee members. 
The cold credential is what the Orchestrator sees from the command line when querying the current committee state from a Cardano node:

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

This example shows a committee with one member identified by the cold credential ``keyHash-5e86313210aef13297187c38c98abb632678906230f555a1bf0a647a``.
Note the tag: ``MemberNotAuthorized``.

Authorizing transactions that publish two types of certificates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The cold credential is used to authorize transactions that publish two types of certificates:

1. Hot credential authorization certificates, which have the effect of registering a hot credential to vote on behalf of the cold credential.
2. Committee resignation certificates, which have the effect of removing the member from the committee.

Both of these are exceptional events which should occur infrequently, if at all, although a hot credential must be authorized at least once to allow for voting.

Two ways to add a cold credential to a committee 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A cold credential can be added to the committee in one of two ways, both of which are beyond the control of the one who holds the cold credential. 

1. Ratifying and enacting a governance action that proposes to add a cold credential to the committee. The electorate (DReps and SPOs) of Cardano determine whether or not to ratify governance actions. 

2. Hard-coding a cold credential in the Conway Genesis configuration file. There is only one Conway Genesis file, and it cannot be altered. 

Cold credential is extremely security critical
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This makes the cold credential extremely security critical, as losing control over it can only be remedied by the member being voted out of the committee in the best case, or the entire committee being disbanded via a no confidence governance action in the worst case. 
Either way, the member is unlikely to be re-elected after such a display of negligence. 

Hot credentials
---------------

For a committee member to be authorized to vote, they must have a hot credential. 
It is the "workhorse" credential, in that it is expected to be used frequently for everyday business. 

This is in contrast to the cold credential, which is expected to be used infrequently and only in exceptional circumstances. 

A hot credential must be authorized by the cold credential. 
It also appears in the committee state queried from a Cardano node:

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

This example shows the same committee member as before, only this time they have authorized a hot credential, namely
``{ "keyHash": "e394a160b9f1345b7b84cd25f0a3f1d12cb0c9835a4167f7dc5b52ca" }``. 
Note the tag: ``MemberAuthorized``.

Replaceable 
~~~~~~~~~~~~

Compared with the cold credential, a hot credential is relatively cheap to replace: when a new hot credential is authorized by the cold credential, it invalidates the existing hot credential, taking its place. 
This is entirely within the control of the bearer of the cold credential, and comes into effect as soon as the authorization certificate is published on chain via a transaction. 

Drawbacks of replacing hot credentials
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Replacing a hot credential requires using the cold credential, which is meant to be used sparingly --- ideally never after the hot credential is authorized. 

Replacing hot credentials frequently is a highly visible activity and may negatively impact the reputation of the committee member within the community. 
Remember, the community has the power to depose individual committee members or the whole committee if they lose faith in them. 

This system also offers greater flexibility for managing the hot credential without having to replace it.
