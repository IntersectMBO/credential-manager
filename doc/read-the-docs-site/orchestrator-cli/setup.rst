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

.. note::
   You should also configure your nix installation to use the IOG and NixOS
   binary caches so you don't end up building everything locally, which will
   consume a lot of disk space and take a very long time. Edit your nix config
   and make sure the following options are set:

   .. code-block::

      experimental-features = nix-command flakes fetch-closure
      trusted-users = <your user name>
      substituters = https://cache.iog.io https://cache.nixos.org/
      trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=

   Where ``<your user name>`` is your username (run the command ``whoami`` to
   print this if you are unsure). This file is normally located at
   ``/etc/nix/nix.conf``. If it doesn't exist, create it and paste the above
   contents (replace ``<your user name>`` with your real user name).

   You will need to restart your nix daemon (or alternatively, reboot your
   machine) after editing this config file.

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
* ``orchestrator-stake.vkey`` a staking verification key file for gov action deposit returns.
* ``orchestrator-stake.skey`` a signing key file for the staking verification key.
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
   Minting cold NFT
   Estimated transaction fee: Coin 174961
   Sending cold NFT to orchestrator...
   Transaction successfully submitted.
   Minting hot NFT
   Estimated transaction fee: Coin 174961
   Sending hot NFT to orchestrator...
   Transaction successfully submitted.

This command will mint two NFTs and send them to the orchestrator along
with 5 ADA each:

.. code-block:: bash

   $ get-orchestrator-utxo
                              TxHash                                 TxIx        Amount
   --------------------------------------------------------------------------------------
   256e2f832a760e670eee1eb35aced5fe02b6db489f0980783e9fed401e67aa3b     0        600000000000 lovelace + TxOutDatumNone
   678779930f025f96e568be011a8b1c9aa11b885437c9131db4ce6f808a37b3c1     0        5000000 lovelace + 1 c8aa0de384ad34d844dc479085c3ed00deb1306afb850a2cde6281f4 + TxOutDatumNone
   6d5f0892e3cfd3b2f5b58dc2965df7ef797125d3192327379ba3db6064e0048d     0        5000000 lovelace + 1 e2ab737f528cd043927496dd34e6629beb1e57ee8fe92c582cf76bd0 + TxOutDatumNone

The policy IDs for these two tokens are in the environment variables
``$COLD_POLICY_ID`` and ``$HOT_POLICY_ID``.
