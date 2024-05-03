.. _init_cold_committee:

Initializing the Cold Committee Credential Script
=================================================

Before anything else can be done, the cold committee credential script needs to
be initialized.

Step 1: Choosing an NFT
-----------------------

The committee cold credential script needs to be given a minting policy ID to
look for when validating a transaction. If the transaction spends the given
policy ID, it will validate the transaction. As such, a minting policy ID must
be chosen. ``orchestrator-cli`` does not enforce any requirements on the choice
of minting policy ID, though it does not allow the empty ADA pseudo-minting
policy ID. However, there are some important requirements to ensure safe
operation of the system:

1. The minting policy should be **non-fungible**, for example by requiring a
   specific transaction output to be spent in the minting transaction.
2. There should be one and only one token minted with the chosen minting policy.
   It is important to note that the script does not require a specific token
   name, so NFT minting policies that were used to mint multiple tokens are not
   safe to use, even if each one has a unique token name.
3. Finally, the orchestrator must be able to actually spend the chosen NFT.

Let's say we choose the NFT policy
``14987a29cf4065e7b38a4cde6bc84b067492ad3ecc8223598a8fe4be``.

.. code-block:: bash

   $ COLD_POLICY_ID=14987a29cf4065e7b38a4cde6bc84b067492ad3ecc8223598a8fe4be

Step 2: Creating the assets
---------------------------

Once the minting policy is chosen, we can use ``orchestrator-cli`` to
initialize the script:

.. code-block:: bash

   $ orchestrator-cli cold-credential init -p "$COLD_POLICY_ID" -o cold-credential

The ``-p`` option (or ``--policy-id`` in long form) specifies the minting
policy ID to use, and the ``-o`` option (or ``--out-dir`` in long form)
specifies a directory to write the output assets to. This directory will be
created if it doesn't already exist. Every other command has an ``-o`` option
and it will not be explained elsewhere.

Let's see what assets were created.

.. code-block:: bash

   $ ls cold-credential -1
   script.hash
   script.plutus

The output only contains two files. ``script.plutus`` contains the compiled
script bytes wrapped in a text envelope:

.. code-block:: bash

   $ cat cold-credential/script.plutus
   {
      "type": "PlutusScriptV3",
      "description": "",
      "cborHex": "59040c5904090101003323232323222259323293232325333573466e1d200000218009919192999ab9a3370e9000001099191919191919191919191919194004c068dd61aba100f9aba100e9aba100d9aba100c9aba100b9aba100a9aba10099aba10089aba10079aba10069aba10059aba10049aba10039aba10029aba10019aba135744002357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae880044c03d2401035054310035573c0046aae74004dd51aba10019919192999ab9a3370e90000010c00cd5d0800854ccd5cd19b874800800860126ae840042a666ae68cdc3a400800430073574200215333573466e1d200600218009aba10019aba13574400215333573466e1d200800218059aba10010a999ab9a3370e90050010c014d5d0800cd5d09aba200109807a481035054310035573c0046aae74004dd51aba1357440021300c491035054310035573c0046aae74004dd5001c8a400644600b200922222222222222220104a005228014a00528014a0041804912c800c6005223219003914800c8888c01c00c300d225900189803001c886400a44a666ae68cdc78010058c0004c01800c1bae0038c0022600e920103505435001802111999aab9f00128001400cc8c8c94ccd5cd19b874800000860026ae84006646464a666ae68cdc3a400000426464650013232325333573466e1d2000002180098081aba10019980891919192999ab9a3370e90000010c004c050d5d0800854ccd5cd19b87480080084ca0066eb4d5d08014dd69aba10019bad357426ae880046ae880044c0592401035054310035573c0046aae74004dd50009aba135744002130124901035054310035573c0046aae74004dd51aba1004998009800bad357420073232325333573466e1d200000218000a999ab9a3370e90010010c014dd71aba10010a999ab9a3370e90020010c00cd5d080084c049241035054310035573c0046aae74004dd51aba1002998073ae357426ae88008464460046eac004c04088cccd55cf800940008ca007001375c6aae74006600a6aae7800530043574400635742005000357440026ae880044c0312401035054310035573c0046aae74004dd51aba13574400213009491035054310035573c0046aae74004dd51aba100298019aba200240008c8c8c94ccd5cd19b874800000860026eb8d5d0800854ccd5cd19b874800800860066eb8d5d080084c0192401035054310035573c0046aae74004dd5000911919192999ab9a3370e90010010c00854ccd5cd19b87480000086002600a6ae840042600c921035054310035573c0046aae74004dd5000919319ab9c001800119180080091198019801001000a611e581c14987a29cf4065e7b38a4cde6bc84b067492ad3ecc8223598a8fe4be0001"
   }


In which we can see our policy embedded near the end (try searching for
occurrences of it on this page with Ctrl-f).

``script.hash`` contains a hash of the script bytes as hexadecimal text:

.. code-block:: bash

   $ cat cold-credential/script.hash
   7b34b13a4751b7f66f0b3375f48257d4e616103fccd1f8bd0a8aa4ed

This hash will be our cold committee credential.

Step 3: Add the Credential to The Committee
-------------------------------------------

This is the only step that cannot be achieved without some kind of special
power over the Cardano network. There are two ways to add a cold credential to
the committee:

1. Create an `update-committee` governance action that proposes to add the
   credential to the committee and have it ratified and enacted at epoch
   boundaries.
2. Hard-code it in the Conway genesis file. The only members who will be added
   this way will be members of the interim constitutional committee for the
   Chang hard fork event. If this applies to your organization, instructions
   will be communicated about submitting this credential.

So either you will need to have authorial control over the conway genesis file
on your network, or you will need to control a sufficient coalition of DReps
and SPOs to vote yourself in. Instructions on how to do this are out of scope
for this guide.

If you want to get added to the committee on a public testnet like SanchoNet,
you should first introduce yourself on the appropriate online channels and make
your case for being elected to the testnet committee.

Alternatively, you can run a private testnet locally using something like
`Cardano Testnet <https://github.com/IntersectMBO/cardano-node/tree/master/cardano-testnet>`_
which will give you complete control over the operation of the network.
Relevant guides that will help you with this process include:

* `Register a DRep <https://sancho.network/tutorials/drep-registration>`_
* `Delegate votes to a DRep <https://sancho.network/tutorials/delegate-to-drep>`_
* `Creating update-committee governance actions <https://sancho.network/tutorials/actions>#update-committee-actions`_
* `Voting on actions <https://sancho.network/tutorials/vote-action>`_
   * To ratify an ``update-committee`` action, you will have to vote as both
    DReps and SPOs. SPO cold keys are in the directory structure created by 
    ``cardano-testnet``
   * You will have to wait for an epoch boundary before the action is ratified,
     and it takes another epoch before an ``update-committee`` action is
     enacted. Configuring your private testnet to have short epoch lengths is
     recommended for testing purposes.

However you proceed, once you have been added to the committee, you should be
able to check on your status:

.. code-block:: bash

   $ cardano-cli conway query committee-state --cold-script-hash $(cat cold-credential/script.hash)
   {
      "committee": {
         "scriptHash-7b34b13a4751b7f66f0b3375f48257d4e616103fccd1f8bd0a8aa4ed": {
               "expiration": 50000,
               "hotCredsAuthStatus": {
                  "tag": "MemberNotAuthorized"
               },
               "nextEpochChange": {
                  "tag": "NoChangeExpected"
               },
               "status": "Active"
         }
      },
      "epoch": 61,
      "threshold": 0
   }

