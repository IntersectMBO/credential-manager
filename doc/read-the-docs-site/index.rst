Constitutional Committee Credential Management System user guide
================================================================

Introduction
------------

The Constitutional Committee credential management system is a framework designed to manage Cardano constitutional committee credentials within the Cardano blockchain ecosystem. It is a suite of Plutus scripts and tools for managing credentials with an X.509 certificate chain, ensuring secure access and operations within the Cardano blockchain for key management and security best practices. 

Purpose and goals
~~~~~~~~~~~~~~~~~

- Implements constitutional committee credentials with an X.509 certificate tree.
- Distributes responsibility among different user groups.
- Allows updating of user groups without changing committee credentials.
- Ties individual credentials to a publicly verifiable identity.

Functionality
~~~~~~~~~~~~~

- Manages four script credentials: cold committee, hot committee, cold NFT payment, and hot NFT payment.
- Uses NFTs to implement system logic through indirection.

Components
~~~~~~~~~~

- Roles include user roles (membership, delegation, voting) and operational roles (head of security, orchestrator).
- Scripts with parameters, rules, datum, and redeemers for minting, committee credentials, and NFT locking scripts.
- Processes for auditing, obtaining certificates, and verifying signatories and certificate authority hash.

Operational process
~~~~~~~~~~~~~~~~~~~

- Orchestrator CLI setup steps: install Nix, clone repository, enter Nix shell.
- Steps for assembling and submitting transactions, and verifying on-chain changes.

This documentation site includes an overview of the system, a user manual for each of the end-user roles, and a reference manual that documents every option for every tool.

.. toctree::
   :maxdepth: 1

   background/index.rst
   system-overview/index.rst
   orchestrator-cli/index
   cc-sign/index
   tx-bundle/index
