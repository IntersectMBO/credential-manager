.. _system_overview:

System overview
===============

This system implements constitutional committee credentials that are backed by an X.509 certificate tree, composed of multiple sub-credentials. Its goals are:

* Distribute responsibility between different groups of users
* Allow user groups to be updated (rotated) without changing the committee credentials
* Tie the individual's credential to publicly verifiable identity via X.509 certificate chains for transparency and audit compliance.

At a high level, it works by coordinating four script credentials:

* A :ref:`cold committee script credential <cold_credential_script>`
* A :ref:`hot committee script credential <hot_credential_script>`
* A :ref:`cold NFT payment script credential <cold_nft_locking_script>`
* A :ref:`hot NFT payment script credential <hot_nft_locking_script>`.

The two committee credential scripts are registered as the actual hot and cold committee credentials on-chain. 
However, they do not contain the majority of the system's logic for two reasons:

* Committee credentials cannot be associated with a datum, so any information they depend on, such as lists of users, must be hard-coded in the script.
* If the system needs to be updated --- for example, to apply security patches --- the committee credentials would need to be replaced, which is the situation the system is intended to avoid.

Instead, they both employ a layer of indirection to delegate the logic to payment credentials, which do not suffer from these two problems. 
To do this, they require only that a known NFT is spent by the transaction. 
The payment credential that holds this NFT implements the system's real logic.

.. warning::
   This system replaces control of a private key with control of an NFT. 
   The loss of control of either NFT is equivalent to losing control of either committee credential. 
   To mitigate the risks involved, it is imperative to follow these guidelines, neither of which are enforced by the system:

   * The cold and hot credentials must be associated with different minting policy IDs
   * The NFTs must be true, meaning their minting policy must guarantee they can never be minted again (for example, by requiring that a specific UTXO is spent to mint them)
   * There must only ever be one token minted with each policy ID. 
     **The scripts do not check token names; they only check minting policy IDs**. 
     So an NFT is not safe to use if other tokens have been minted with the same policy, even if they have different token names.
   * The signing keys used by the user groups to spend these NFTs must be managed according to established
     `security best practices <https://developers.cardano.org/docs/get-started/secure-workflow>`_.
   * The above is especially true for users in the **membership** group, who have the power to spend the cold NFT arbitrarily. 
     It is recommended that the members of this group are technically skilled, trustworthy individuals who have the knowledge, skills, and equipment required to manage keys securely.
   * In addition to secure key management, it is equally important to use `Cryptographically secure sources of entropy <https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator>`_ to generate these keys, such as `/dev/urandom` on Unix-based systems and it is important to instruct users how to do this properly.

.. toctree::
   :maxdepth: 1
   :titlesonly:

   roles
   cold-committee
   hot-committee
   cold-nft
   hot-nft
   auditing
