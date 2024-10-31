.. _tx-bundle:

Tx Bundle Tool
==============

The Tx Bundle Tool is a command-line tool for building and interacting with tx bundle files.
A tx bundle file contains information of groups of extra signatories required by Plutus contracts bundled alongside a template transaction body.
When building a transaction with Plutus scripts that check for a signature, the exact set of extra witnesses must be specified in advance.
This is because such scripts introduce a cyclic dependency between the transaction body and the witness set.
In order to create the transaction body, the Plutus scripts need to be validated, execution units need to be measured, and the fee needs to be set.
All of these change the transaction body bytes.
In order to validate the scripts however, they need to know who has signed the transaction.
However in order to sign a transaction, its body must be known.
In practice, this means that you must forward declare the extra witnesses that a plutus script requires.
If a Plutus Script requires, say 4 out of a group of 7 signatories to sign, the exact 4 signatories must be known in advance before anyone can sign, even though in principle, any 4 could sign.

The approach tx bundles take to solve this problem is to record information about who may sign, and how many signatures are needed in the bundle file.
Given this information, it is possible to enumerate every possible variation of the transaction that could be built for a given group of signers.
When one of the signatories signs a bundle, they produce a witness bundle.
A witness bundle contains a signature for every transaction in the transaction bundle for which the signatory could provide a signature.
To build the final transaction, a sufficient set of witness bundles can be combined with the original tx bundle to produce a signed transaction.

Installation
============

Currently, the only way to install ``tx-bundle`` is via Nix

.. code-block:: bash

NixOS
-----

The repository is a Nix Flake. Add it to your config flake to install it:

.. code-block:: nix

   # flake.nix
   {
     inputs = {
       nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
       credential-manager.url = "github:IntersectMBO/credential-manager";
     };
     outputs = { self, nixpkgs, ... }@inputs: {
       nixosConfigurations.my-machine = nixpkgs.lib.nixosSystem {
         system = "x86_64-linux";
         specialArgs = inputs;
         modules = [ ./configuration.nix ];
       };
     };
   }

   # configuration.nix
   { config, lib, nixpkgs, credential-manager, ... }: {
     environment.systemPackages = [
       credential-manager.packages.x86_64-linux.tx-bundle;
     ];
   }

Via Nix (non-NixOS)
-------------------

.. code-block:: bash

   nix profile install github:IntersectMBO/credential-manager#tx-bundle

Usage
=====

Building a transaction bundle
-----------------------------

First, create a template transaction (e.g. by using ``cardano-cli transaction build``).
Note that the modified transactions only alter the extra witness keys and do not adjust script execution units or fees, nor do they validate the scripts.
Consequently, it is important that the template transaction contains conservative upper-bound values for execution units and fees.
For example, if you have a Plutus Script that requires at least 4 out of 7 signatories to sign the transaction, specify all 7 signatories when building the template transaction.
This will ensure that the execution units, tx size, and fees that are calculated will be sufficient to cover all possible transactions.

Say our transaction file is ``myTx.txbody``, and it must be signed by 4 out of 7 signatories.
The command to build a transaction bundle file would be:

.. code-block:: bash

   $ tx-bundle build \
     --tx-body-file myTx.txbody \
     --required-signer-group-name my-group \
     --required-signer-group-threshold 4 \
     --required-signer 19c04196cca86fb0fbf09a35e67d55148508acafa321ebc509bc5cd6 \
     --required-signer 2b3d02d77ee80b219ca1a20cd3f82b95ff23eb28ca4e886ce3cc039d \
     --required-signer 95bebd09ef4d125a595ae0bf5f15724731a7537b5fda32927bc7b366 \
     --required-signer 7c4ce0c3eca1b077d8465cf3b44db18beea87bacf55c05c9b4d0317c \
     --required-signer a263b5a55cb7b8728a0a97092fad7054117f7695897990bc1ab499b4 \
     --required-signer b381e71db0a8fcf7c6f928ad5d1c7925f8143e7bcd534208406f3325 \
     --required-signer c6731b9c6de6bf11d91f08099953cb393505806ff522e5cc3a7574ab \
     --out-file myTx.txbundle

You can specify more than one group if there are multiple signing groups:

.. code-block:: bash

   $ tx-bundle build \
     --tx-body-file myTx.txbody \
     --required-signer-group-name my-group-1 \
     --required-signer-group-threshold 2 \
     --required-signer 19c04196cca86fb0fbf09a35e67d55148508acafa321ebc509bc5cd6 \
     --required-signer 2b3d02d77ee80b219ca1a20cd3f82b95ff23eb28ca4e886ce3cc039d \
     --required-signer 95bebd09ef4d125a595ae0bf5f15724731a7537b5fda32927bc7b366 \
     --required-signer-group-name my-group-2 \
     --required-signer-group-threshold 3 \
     --required-signer 7c4ce0c3eca1b077d8465cf3b44db18beea87bacf55c05c9b4d0317c \
     --required-signer a263b5a55cb7b8728a0a97092fad7054117f7695897990bc1ab499b4 \
     --required-signer b381e71db0a8fcf7c6f928ad5d1c7925f8143e7bcd534208406f3325 \
     --required-signer c6731b9c6de6bf11d91f08099953cb393505806ff522e5cc3a7574ab \
     --out-file myTxMultiGroup.txbundle

Alternatively, ``tx-bundle build`` can create a tx bundle using the same parameters as ``cardano-cli build``.
Indeed, under the hood it calls the same code used by ``cardano-cli build`` to do so.
The only differences are that you must specify ``required-signer-group-name`` and ``required-signer-group-threshold`` as well as ``required-signer-hash``.
You must also specify at least one signing group (otherwise, why are you using ``tx-bundle``?)

Additionally, it only supports ``--out-file``, not ``--calculate-plutus-script-cost``.

Here is an example, taken from the ``orchestrator-cli`` documentation

.. code-block:: bash

   $ tx-bundle build \
      --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
      --tx-in $(jq -r 'keys[0]' hot-nft.utxo) \
      --tx-in-script-file init-hot/nft.plutus \
      --tx-in-inline-datum-present \
      --tx-in-redeemer-file vote/redeemer.json \
      --tx-out "$(cat vote/value)" \
      --tx-out-inline-datum-file vote/datum.json \
      --required-signer-group-name voting \
      --required-signer-group-threshold 2 \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-7.cert) \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-8.cert) \
      --required-signer-hash $(orchestrator-cli extract-pub-key-hash example-certificates/child-9.cert) \
      --vote-file vote/vote \
      --vote-script-file init-hot/credential.plutus \
      --vote-redeemer-value {} \
      --change-address $(cat orchestrator.addr) \
      --out-file vote/body.txbundle
   Estimated transaction fee: Coin 702241

Inspecting a transaction bundle
-------------------------------

The ``info`` command displays information from a tx bundle file.
By default it shows group and signatory count and transaction count.

.. code-block:: bash

   $ tx-bundle info --tx-bundle-file myTx.txbundle
   Transaction era: Conway
   Signatory group count: 1
   Signatory count: 7
   Total possible transaction count: 64

   $ tx-bundle info --tx-bundle-file myTxMultiGroup.txbundle
   Transaction era: Conway
   Signatory group count: 2
   Signatory count: 7
   Total possible transaction count: 20

Or about a specific group with the ``--group`` flag:

.. code-block:: bash

   $ tx-bundle info --group my-group-1 --tx-bundle-file myTxMultiGroup.txbundle
   Group size: 3
   Group threshold: 2
   Group members:
     19c04196cca86fb0fbf09a35e67d55148508acafa321ebc509bc5cd6
     2b3d02d77ee80b219ca1a20cd3f82b95ff23eb28ca4e886ce3cc039d
     95bebd09ef4d125a595ae0bf5f15724731a7537b5fda32927bc7b366

Or a specific signatory:

.. code-block:: bash

   $ tx-bundle info --signatory 19c04196cca86fb0fbf09a35e67d55148508acafa321ebc509bc5cd6 --tx-bundle-file myTxMultiGroup.txbundle
   Signatory index: 0
   Signatory groups: 0
   Total possible transactions: 15

Finally, you can render the template transaction with the ``--tx`` flag, and optionally the ``--yaml`` format flag.
This will output the same thing as ``cardano-cli transaction view`` on the template tx body.

Signing a transaction bundle
----------------------------

Signing a transaction bundle creates a witness bundle.
The command for this is ``witness``:

.. code-block:: bash

   $ tx-bundle witness --tx-bundle-file myTxMultiGroup.txbundle --signing-key-file my-signing-key.skey --out-file myTx.witbundle

It is also possible to sign without being a member of a signing group.
This is required if you need to sign a transaction for another reason other than running a Plutus Script, for example, to spend a tx output.
To do this, specify the ``--all`` flag.
This will sign all possible transactions that could be generated for the bundle.

.. code-block:: bash

   $ tx-bundle witness --all --tx-bundle-file myTxMultiGroup.txbundle --signing-key-file my-signing-key.skey --out-file myTx.witbundle

Inspecting a witness bundle
---------------------------

Similar to the ``info`` command, the ``witness-info`` command displays information about a witness bundle file.
By default it shows the verification key and transaction count.

.. code-block:: bash

   $ tx-bundle witness-info --witness-bundle-file myTx.witbundle
   Verification key: f45a406629dd00bc39d4b4c3834f16eedeade3c191386b9ef6900995dbb26fd8
   Signature count: 20

You can list all the transaction IDs with the ``--ls`` flag:

.. code-block:: bash

   $ tx-bundle witness-info --witness-bundle-file myTx.witbundle --ls
   10e69fadfe8d2c8863e3de0f09602d407f154ecdbf5f090da1ed07fc89d33fcb
   14d122ccfff8c69bc0d5cbc32a7a5308aee0ba13779143e0f01d9489a7e8d1df
   1a65a0755d4a6223a28dc862fb673fc354bdfa39616c6088c357fe199a761a31
   2891d3760e8ef3268e39359c44fb0164bcbc38414e04b0b38ef5947b5da4f694
   2973d2e9815d9a4f365aa3dd3280580991cde97c3690240f3a524ce0397d33bf
   364d1881508799685c07c83ab6e325d2ec01fc38eb0eb7061a0b915e192bbf65
   379d71a2e6f022e28d48958d4050e40f60f7d304bc675fa6761d999e2b95d522
   38328efd0c15cf5ebb6b649755dad949149eb020c880413b30944863c9459f4d
   3cd544baed0b9849991d09caa9bf5d427d067e65681782d0c198409b8cc89097
   49e59d3fe9669b5340c1773de57162574eaaa29d290ef0afa33930d2fbfd65be
   49f877317691873bd961adccf3dd1ac7150e2aac98bd045d5ae2bfd9cc77d87b
   4f71cd2f6e66841ca56df4d412ee4a6db317d0871a095b51224bee14278d336a
   5af9f3ee7c16f359eb9237613a07b9656b38ea4d178a9e27a5a8474348bd9c63
   6058ede380c202eb8de702e1d448c62c6abda270d4bed91a13ee7a58d39c9974
   7cf30e09030b5f947eb0f1d8a3c1fc826e1f630d570eee6495cfd122f3d34b38
   857d942e3459faa8c0da5ed393eed80c72f463eeb1427bec2bfd36327b3e51f4
   8e0b1b8d14ca48201383fbc848c1eb06647bdee6b24cac20ae2d43b4111bb6df
   9e0ce43ed80bdcc7d71b257edba761cf55d2b05172a9cddf57a70586186bdb84
   b72c36c6d6a429dcc980c32dea7975818f1275b66d99dea4c21811a7db702a1a
   c8f55963e6d83a169be20150bc1aabec7a1e9eb494ddaa630ae2142228b5fa4f

and display the signature for a transaction with the ``--tx`` flag:

.. code-block:: bash

   $ tx-bundle witness-info --witness-bundle-file myTx.witbundle --tx 10e69fadfe8d2c8863e3de0f09602d407f154ecdbf5f090da1ed07fc89d33fcb
   fa6840a72471a21a046e901aa2fb17974b9441f186916101653393ef2b001f2c326608a40cba9f5534f7f0cdb658aadef1d6547bb90fb95b02409e48ae75f104

Assembling a transaction
------------------------

Finally, several witness bundles can be used to assemble a signed transaction using the ``assemble``
command.

.. code-block:: bash

   $ tx-bundle assemble \
     --tx-bundle-file myTxMultiGroup.txbundle \
     --witness-bundle-file myTx.witbundle \
     --witness-bundle-file myTx2.witbundle \
     --witness-bundle-file myTx4.witbundle \
     --witness-bundle-file myTx5.witbundle \
     --witness-bundle-file myTx6.witbundle \
     --out-file myTx.tx

Note that if you do not provide enough witness bundles to satisfy all groups,
this command will fail:

.. code-block:: bash

   $ tx-bundle assemble \
     --tx-bundle-file myTxMultiGroup.txbundle \
     --witness-bundle-file myTx.witbundle \
     --witness-bundle-file myTx4.witbundle \
     --witness-bundle-file myTx5.witbundle \
     --witness-bundle-file myTx6.witbundle \
     --out-file myTx.tx
   Too few signatures
