.. _init_hot_committee:

Initializing the Hot Committee Credential Script
================================================

Now it is time to initialize the hot committee credential script. This will be
very similar to initializing the cold credential script.

Step 1: Choosing an NFT
-----------------------

Just as with the cold credential script, the hot committee script requires an
NFT for initialization. The same guidelines which applied there also apply
here.

If you have been following along with this guide, you will have already setup
the NFTs.

Step 2: Creating the assets
---------------------------

Once the minting policy is chosen, we can use ``orchestrator-cli`` to
initialize the script:

.. code-block:: bash

   $ orchestrator-cli hot-credential init -p "$HOT_POLICY_ID" -o hot-credential

Here are the assets:

.. code-block:: bash

   $ ls hot-credential -1
   script.hash
   script.plutus

This is the same as we had when initializing the cold credential script.

.. code-block:: bash

   $ cat hot-credential/script.plutus
   {
       "type": "PlutusScriptV3",
       "description": "",
       "cborHex": "59040c5904090101003323232323222259323293232325333573466e1d200000218009919192999ab9a3370e9000001099191919191919191919191919194004c068dd61aba100f9aba100e9aba100d9aba100c9aba100b9aba100a9aba10099aba10089aba10079aba10069aba10059aba10049aba10039aba10029aba10019aba135744002357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae88004d5d10009aba2001357440026ae880044c03d2401035054310035573c0046aae74004dd51aba10019919192999ab9a3370e90000010c00cd5d0800854ccd5cd19b874800800860126ae840042a666ae68cdc3a400800430073574200215333573466e1d200600218009aba10019aba13574400215333573466e1d200800218059aba10010a999ab9a3370e90050010c014d5d0800cd5d09aba200109807a481035054310035573c0046aae74004dd51aba1357440021300c491035054310035573c0046aae74004dd5001c8a4006450029400a450029400a5002918024801c88888888888888880401804912c800c6005223219003914800c8888c01c00c300d225900189803001c886400a44a666ae68cdc78010058c0004c01800c1bae0038c0022600e920103505435001802111999aab9f00128001400cc8c8c94ccd5cd19b874800000860026ae84006646464a666ae68cdc3a400000426464650013232325333573466e1d2000002180098081aba10019980891919192999ab9a3370e90000010c004c050d5d0800854ccd5cd19b87480080084ca0066eb4d5d08014dd69aba10019bad357426ae880046ae880044c0592401035054310035573c0046aae74004dd50009aba135744002130124901035054310035573c0046aae74004dd51aba1004998009800bad357420073232325333573466e1d200000218000a999ab9a3370e90010010c014dd71aba10010a999ab9a3370e90020010c00cd5d080084c049241035054310035573c0046aae74004dd51aba1002998073ae357426ae88008464460046eac004c04088cccd55cf800940008ca007001375c6aae74006600a6aae7800530043574400635742005000357440026ae880044c0312401035054310035573c0046aae74004dd51aba13574400213009491035054310035573c0046aae74004dd51aba100298019aba200240008c8c8c94ccd5cd19b874800000860026eb8d5d0800854ccd5cd19b874800800860066eb8d5d080084c0192401035054310035573c0046aae74004dd5000911919192999ab9a3370e90010010c00854ccd5cd19b87480000086002600a6ae840042600c921035054310035573c0046aae74004dd5000919319ab9c001800119180080091198019801001000a611e581ce2ab737f528cd043927496dd34e6629beb1e57ee8fe92c582cf76bd00001"
   }
   $ cat hot-credential/script.hash
   c5ce2386d5fee41a026feb39814e8a0e4185917bfbcd6f1c553d738a

This hash will be our hot committee credential.

Step 3: Create the Assets For Authorizing the Hot Credential
------------------------------------------------------------

Now that we have a hot credential, the next step is to authorize it via a
certificate. Technically, this requires the delegation group to sign the
transaction, so as the orchestrator, we can't do this by ourselves. If you have
been following along with the example certificates however, you have access to
all the signing keys anyway, so we can pretend we belong to the user roles.

To publish the certificate, we need to add it to a transaction. Our cold
credential will be required to authorize the transaction, which means that we
need to spend the cold NFT in the transaction. To spend the cold NFT, our cold
NFT locking script will have to authorize the transaction. Before we can build
the transaction, we need to prepare a new output for the cold NFT, as well as a
redeemer to spend it. ``orchestrator-cli`` has a command that can help with
this, but it in turn requires the current cold NFT output as a JSON file, which
we can query and save with a little bit of ``jq`` manipulation:

.. code-block:: bash

   $ cardano-cli conway query utxo --address $(cat cold-nft/script.addr) --output-json \
     | jq 'to_entries | .[0].value' \
     > cold-nft.utxo

Alternatively, the nix shell provides a command to make this easier:

.. code-block:: bash

   $ fetch-cold-nft-utxo

Now we can prepare the assets for the transaction with ``orchestrator-cli``:

.. code-block:: bash

   $ orchestrator-cli cold-nft authorize \
     -u cold-nft.utxo \
     --cold-credential-script-file cold-credential/script.plutus \
     --hot-credential-script-file hot-credential/script.plutus \
     --out-dir authorize

As before, let's see what assets were prepared:

.. code-block:: bash

   $ ls authorize -1
   authorizeHot.cert
   datum.json
   redeemer.json
   value

``orchestrator-cli`` built our authorization certificate for us:

.. code-block:: bash

   cat authorize/authorizeHot.cert
   {
       "type": "CertificateConway",
       "description": "Constitution committee member hot key registration",
       "cborHex": "830e8201581ce04f00fc3676b756fcebeaabeb5b297d9cb6f0589db0893f6d5beb8b8201581cc5ce2386d5fee41a026feb39814e8a0e4185917bfbcd6f1c553d738a"
   }

We also have a datum for the new output (it should be identical to the input datum)

.. code-block:: bash

   diff <(jq '.inlineDatum' < cold-nft.utxo) <(jq '.' < authorize/datum.json)

And a redeemer:

.. code-block:: bash

   cat authorize/redeemer.json
   {
       "constructor": 0,
       "fields": [
           {
               "constructor": 1,
               "fields": [
                   {
                       "bytes": "c5ce2386d5fee41a026feb39814e8a0e4185917bfbcd6f1c553d738a"
                   }
               ]
           }
       ]
   }

Notice how the redeemer contains the hot credential script hash? This redeemer
says we are spending the cold NFT to authorize this hot committee script hash
credential.

Finally, there is a file called ``value``. This is just a convenience text file
for building the transaction with ``cardano-cli``.

.. code-block:: bash

   cat authorize/value
   addr_test1wzng9lxemp4skvd9zs3zp4eslldxsg87afcvtm22qm07saqew8uzk+5000000 lovelace + 1 c8aa0de384ad34d844dc479085c3ed00deb1306afb850a2cde6281f4

Step 4: Create the Authorization Transaction
--------------------------------------------

Now we have everything we need to build the transaction:

.. code-block:: bash

   $ cardano-cli conway transaction build \
     --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
     --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
     --tx-in $(cardano-cli query utxo --address $(cat cold-nft/script.addr) --output-json | jq -r 'keys[0]') \
     --tx-in-script-file cold-nft/script.plutus \
     --tx-in-inline-datum-present \
     --tx-in-redeemer-file authorize/redeemer.json \
     --tx-out "$(cat authorize/value)" \
     --tx-out-inline-datum-file authorize/datum.json \
     --required-signer-hash $(cat example-certificates/children/child-4/child-4.keyhash) \
     --required-signer-hash $(cat example-certificates/children/child-5/child-5.keyhash) \
     --certificate-file authorize/authorizeHot.cert \
     --certificate-script-file cold-credential/script.plutus \
     --certificate-redeemer-value {} \
     --change-address $(cat orchestrator.addr) \
     --out-file authorize/body.json
   Estimated transaction fee: Coin 521021

There is quite a lot going on here, and it warrants an explanation. First we
have our transaction inputs:

.. code-block:: bash

   --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
   --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
   --tx-in $(cardano-cli query utxo --address $(cat cold-nft/script.addr) --output-json | jq -r 'keys[0]') \

The first input is from the orchestrator's wallet, and is used to cover the fee.
The second line uses the same input as collateral, as we are running a plutus
script. The next input is the script's current UTxO. Note that these query
commands all assume there is one input at each address, and that we want to use
it.

After the last input, there are some additional options that relate to the
script input:

.. code-block:: bash

   --tx-in-script-file cold-nft/script.plutus \
   --tx-in-inline-datum-present \
   --tx-in-redeemer-file authorize/redeemer.json \

The first option attaches the script to the transaction, which is required when
spending a script output. The second option says to use the inline datum
present in the output. The third option specifies the redeemer to pass to the
script.

The options related to the tx outputs are fairly straightforward - the first
says the address and the value to send, and the second provides a datum to
embed in the output.

The next lines tell the transaction that it must be signed by ``child-4`` and
``child-5``

.. code-block:: bash

   --required-signer-hash $(cat example-certificates/children/child-4/child-4.keyhash) \
   --required-signer-hash $(cat example-certificates/children/child-5/child-5.keyhash) \

The need for these arguments reveals a bit of an incompatibility (or at least
a point of awkwardness) between the requirements of the scripts and the
requirements of Cardano transactions. Both NFT scripts require a majority of
one of the groups to sign the transaction, but it doesn't care who in that
group signs. Cardano transaction, on the other hand, must explicitly list the
public key hashes of any additional signatures required by scripts.

A consequence of this is that when you build a transaction, you must know in
advance who will be signing it. So you cannot simply build the transaction,
send it to everyone who can sign it, and wait until you receive enough
signatures to submit it. You need to first know who will sign it (e.g. who is
in the office today and able to sign), then wait for all signatures. If it
turns out one of the signers is unavailable, you will need to build a new
transaction without requiring it to be signed by that person. For demonstration
purposes, we are going to sign with child 4 and child 5 (let's assume child 6
is on vacation).

Finally, we add the certificate to the transaction:

.. code-block:: bash

      --certificate-file authorize/authorizeHot.cert \
      --certificate-script-file cold-credential/script.plutus \
      --certificate-redeemer-value {} \

The first option is straightforward: it provides the certificate file we just
created. The second and third options specify the script and a redeemer
to authorize the certificate respectively. Associating a script with a
certificate may seem somewhat ambiguous at first, but it is actually quite
intuitive on closer inspection. A certificate is always issued by a certifying
authority. That authority is identified by a credential, and must authorize the
transaction in which the certificate is issued. In the case of a hot key
authorization certificate, the certifying authority is the CC member,
identified by their cold credential. Because our cold credential is a script
hash credential, we need to provide the script in the transaction body for
execution. The redeemer value is irrelevant in our script, so we pass in the
unit value ``{}``.

Step 5. Distribute the Transaction to The Delegation Group
----------------------------------------------------------

We now have an unsigned transaction body which we need our delegators to sign.
Using the file transfer mechanism of your choice, send the transaction body
file to all the signatories specified with ``--required-signer-hash`` options.
In our example, we control all the keys anyway, so we can sign the transaction
ourselves:

.. code-block:: bash

   $ cardano-cli conway transaction witness \
      --tx-body-file authorize/body.json \
      --signing-key-file example-certificates/children/child-4/child-4.skey \
      --out-file authorize/child-4.witness
   $ cardano-cli conway transaction witness \
      --tx-body-file authorize/body.json \
      --signing-key-file example-certificates/children/child-5/child-5.skey \
      --out-file authorize/child-5.witness

Since we are spending an input from our own wallet to pay for fees, we also
need to sign the transaction with our own signing key:

.. code-block:: bash

   $ cardano-cli conway transaction witness \
      --tx-body-file authorize/body.json \
      --signing-key-file orchestrator.skey \
      --out-file authorize/orchestrator.witness

Step 6. Assemble and Submit the Transaction
-------------------------------------------

Finally, we can put everything together to submit the transaction:


.. code-block:: bash

   $ cardano-cli conway transaction assemble \
      --tx-body-file authorize/body.json \
      --witness-file authorize/child-4.witness \
      --witness-file authorize/child-5.witness \
      --witness-file authorize/orchestrator.witness \
      --out-file authorize/tx.json
   $ cardano-cli conway transaction submit --tx-file authorize/tx.json
   Transaction successfully submitted.

Step 7. Verify the Hot Credential Authorization Status On Chain
---------------------------------------------------------------

We can see the effect of our certificate by querying committee state from the
node:

.. code-block:: bash

   $ cardano-cli conway query committee-state --cold-script-hash $(cat cold-credential/script.hash)
   {
       "committee": {
           "scriptHash-e04f00fc3676b756fcebeaabeb5b297d9cb6f0589db0893f6d5beb8b": {
               "expiration": 50000,
               "hotCredsAuthStatus": {
                   "contents": {
                       "scriptHash": "c5ce2386d5fee41a026feb39814e8a0e4185917bfbcd6f1c553d738a"
                   },
                   "tag": "MemberAuthorized"
               },
               "nextEpochChange": {
                   "tag": "NoChangeExpected"
               },
               "status": "Active"
           }
       },
       "epoch": 47,
       "threshold": 0
   }

