.. _roles:

Roles
*****

The system recognizes and distinguishes between two types of roles: 

1. user roles, which are part of the X.509 certificate hierarchy 
2. operational roles, which are not.

User roles
==========

User roles are defined by being included in the X.509 certificate hierarchy. 
Specifically, they are defined by the leaf certificates.
There are three types of user roles in the system, each with distinct responsibilities: 

* membership role
* delegation role 
* voting role.

Membership role controls the cold credential 
--------------------------------------------

The membership role collectively controls the cold credential. 
They are able to perform the following actions:

* Resign from the committee
* Rotate the membership and delegation keys in the cold NFT datum
* Unlock the cold NFT and spend it arbitrarily.

They are also indirectly capable of authorizing any action the delegation and voting roles can authorize, because they can decide who is in the delegation role, which in turn has the power to decide who is in the voting role.

As such, it is imperative that this responsibility be entrusted to individuals who are trusted by the organization, and who have the knowledge, skills, and equipment necessary to securely safeguard their keys. 

If they lack these qualifications, they SHOULD entrust management of their keys to a qualified key custodian.

Delegation role controls the hot credential 
-------------------------------------------

The delegation role collectively controls the hot credential. 
They are able to perform the following actions:

* Authorize new hot credentials on behalf of the cold credential
* Resign from the delegation role
* Rotate the voting keys in the hot NFT datum
* Unlock the hot NFT and spend it arbitrarily.

They are also indirectly capable of authorizing any action the voting roles can authorize, because they can decide who is in the voting role.

The delegation role also has a significant responsibility to safeguard their credentials; however, they are supervised by the membership role, so there is more of a safety net in place for this role compared with the membership role.

Voting role authorizes votes
----------------------------

The voting role authorizes votes. 
This is the main working group in the system --- they are the ones with the responsibility to deliberate and decide how to vote on governance actions, write and publish rationale documents, and coordinate with the orchestrator to publish votes. 
They are able to perform the following actions:

* Vote on governance actions
* Resign from the voting role.

Operational roles
=================

Operational roles are not explicitly included in the organization's certificate hierarchy. 
They are implicit roles that describe tasks which must be managed by the organization somehow, but the system does not explicitly keep track of them the way it does with the user roles.

.. _head_of_security:

Head of security
----------------

The head of security is responsible for managing the X.509 certificate hierarchy, which includes signing certificate signing requests from user roles, maintaining the organization's store of certificates, and managing the higher levels of certificates in the organization's hierarchy. 
They also maintain the certificate revocation list. 
This system does not provide tooling for the head of security --- tooling for managing X.509 certificate hierarchies already exists, for example ``openssl``.

Orchestrator
------------

The orchestrator is responsible for operating the system. 
Their responsibilities include:

* Minting or procuring the NFTs
* Initializing and maintaining the scripts
* Building transactions as directed by the user groups
* Distributing transactions to the user groups for signing
* Covering transaction fees
* Submitting transactions.

Orchestrators use the credential manager's CLI tool to prepare and create the various transactions needed to operate the system. 
