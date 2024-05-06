.. _orchestrator_cli:

Orchestrator CLI
================

The orchestrator CLI is a command-line tool for the orchestrator to use to
help them prepare transactions with Cardano CLI. It includes commands which
prepare the various script assets that need to be included in transactions to
operate the system. It is designed to work alongside Cardano CLI, does not
require an internet connection, and does not have any environment dependencies.
As such, it is extremely flexible, designed to adapt to the workflow of the
user instead of imposing one.

This user guide will walk through building each transaction type with CLI
tools in an approximate sequence.

.. toctree::
   :maxdepth: 1
   :titlesonly:

   init-cold-committee
   init-cold-nft
   init-hot-committee
   init-hot-nft
   vote
   resign-delegation
   resign-voting
   rotate-cold
