Constitutional Committee Credential Management System user guide
================================================================

Introduction
------------

The Constitutional Committee Credential Management System is a suite of Plutus
scripts and tools for managing Cardano constitutional committee credentials
with an X.509 certificate chain. It provides the following features:

* Separation of capabilities between user roles
* Multi-signature authorization of on-chain governance transactions
* Key rotation without modifying registered committee credentials
* Publicly verifiable authorization of on-chain activity via certificates

This documentation site includes an overview of the system, a user manual for
each of the end-user roles, and a reference manual that documents every option
for every tool.

.. toctree::
   :maxdepth: 1

   background/index.rst
   system-overview/index.rst
   orchestrator-cli/index
