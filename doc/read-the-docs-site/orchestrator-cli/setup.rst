.. _setup:

Setup
=====

This page will walk you through the steps involved in setting up your
environment to follow along with this guide from scratch.

Step 1: Install Nix
-------------------

This repository uses the Nix package manager to manage dependencies. You will
need to install Nix on your machine to setup the environment. Instructions for
doing so can be found |nix_install|.

Step 2: Clone this repository
-----------------------------

.. code-block:: bash

   $ mkdir ~/repos
   $ cd ~/repos
   $ git clone git@github.com:IntersectMBO/credential-manager.git

Step 3: Enter the Nix Shell
---------------------------

**Option 1**: To enter the nix shell using the ``nix`` CLI, navigate to the
root of the git repo and run ``nix develop``:

.. code-block:: bash

   $ cd credential-manager
   $ nix develop

**Option 2**: The repository includes a ``.envrc`` file for |direnv| integration.
If you want to use direnv, navigate to the root of the git repo and run
``direnv allow``:

.. code-block:: bash

   $ cd credential-manager
   $ direnv allow

.. |nix_install| raw:: html

   <a href="https://nixos.org/download" target="_blank">here</a>

.. |direnv| raw:: html

   <a href="https://direnv.net/" target="_blank">direnv</a>

Step 4: Start the local Cardano testnet
---------------------------------------

The nix shell includes a command to spin up a local Cardano testnet running in
the Conway era. It will initialize the network configuration files and run 3
SPO block producing nodes.

.. note::
   If you have previously started a testnet and shut it down and want to spin
   up a new one, you will have to run the command ``purge-local-testnet`` first
   to get rid of the old network's working directory.

.. code-block:: bash

   $ deploy-local-testnet

Step 5: Open a new shell
------------------------

Our previous shell is now running the testnet in the foreground, so open a new
shell, navigate back to the repo root and enter the nix shell again.

Step 6: Initialize the Orchestrator's wallet
--------------------------------------------

We need a wallet to work with. Run the command:

.. code-block:: bash

   $ setup-orchestrator

This will write three files to the repo root directory:

* ``orchestrator.vkey`` the orchestrator's verification key file
* ``orchestrator.skey`` the orchestrator's signing key file
* ``orchestrator.addr`` the orchestrator's address file, which holds 600,000 ADA

Check the balance with the command

.. code-block:: bash

   $ get-orchestrator-utxo
                           TxHash                                 TxIx        Amount
   --------------------------------------------------------------------------------------
   256e2f832a760e670eee1eb35aced5fe02b6db489f0980783e9fed401e67aa3b     0        600000000000 lovelace + TxOutDatumNone

Or alternatively to have it output JSON:

.. code-block:: bash

   $ get-orchestrator-utxo --output-json
   {
       "256e2f832a760e670eee1eb35aced5fe02b6db489f0980783e9fed401e67aa3b#0": {
           "address": "addr_test1vpwg5qy3ku0dfhu0m72nmp79aklvm32gr73xzztnzr35tcgnn0m79",
           "datum": null,
           "datumhash": null,
           "inlineDatum": null,
           "referenceScript": null,
           "value": {
               "lovelace": 600000000000
           }
       }
   }

Other useful commands for querying the UTxO include ``get-output-by-policy-id``
and ``get-orchestrator-ada-only``.

Step 7: Mint Two NFTs
---------------------

The following pages will require you to have minted 2 NFTs to use for the cold
and hot credentials. We have provided a convenience command to mint these NFTs.

.. warning::
   Don't use this command in production to mint your NFTs.

.. code-block:: bash

   $ mint-tokens
   Minting cold NFT, minting policy: 40c80aff033eea853403adab3d29ebdaad9c4757a3cee9bfdff4a7cc
   Estimated transaction fee: Coin 329128
   Minting hot NFT, minting policy: abd6e46e50b70e8b7bcc66bbe35ad8e7393bd9fb704cbbed84797841
   Estimated transaction fee: Coin 329128
   Sending cold NFT to orchestrator...
   Transaction successfully submitted.
   Sending hot NFT to orchestrator...
   Transaction successfully submitted.
   This will write the following files to the repo root directory:

This command will write four files to the repo root directory:

* ``coldMint.plutus`` - the minting policy for the cold NFT as a plutus script.
* ``coldMint.pol`` - the minting policy ID (script hash) of the cold NFT.
* ``hotMint.plutus`` - the minting policy for the hot NFT as a plutus script.
* ``hotMint.pol`` - the minting policy ID (script hash) of the hot NFT.

Additionally, it will mint the two NFTs and send them to the orchestrator along
with 5 ADA each:

.. code-block:: bash

   $ get-orchestrator-utxo
                              TxHash                                 TxIx        Amount
   --------------------------------------------------------------------------------------
   256e2f832a760e670eee1eb35aced5fe02b6db489f0980783e9fed401e67aa3b     0        600000000000 lovelace + TxOutDatumNone
   678779930f025f96e568be011a8b1c9aa11b885437c9131db4ce6f808a37b3c1     0        5000000 lovelace + 1 40c80aff033eea853403adab3d29ebdaad9c4757a3cee9bfdff4a7cc + TxOutDatumNone
   6d5f0892e3cfd3b2f5b58dc2965df7ef797125d3192327379ba3db6064e0048d     0        5000000 lovelace + 1 abd6e46e50b70e8b7bcc66bbe35ad8e7393bd9fb704cbbed84797841 + TxOutDatumNone

Step 8: Setup a DRep
--------------------

In order to add ourselves to the committee, we will need to ratify a governance
action. Doing this requires a DRep to be registered on chain with voting power
delegated to them. This is a somewhat involved process, so the nix shell has
another script that takes care of this:

.. code-block:: bash

   $ setup-drep
   Creating DRep wallet and funding...
   Estimated transaction fee: Coin 171661
   Transaction successfully submitted.
   Registering DRep staking address...
   Estimated transaction fee: Coin 180505
   Transaction successfully submitted.
   Registering DRep...
   Estimated transaction fee: Coin 180725
   Transaction successfully submitted.
   Delegating to DRep...
   Estimated transaction fee: Coin 181869
   Transaction successfully submitted.
