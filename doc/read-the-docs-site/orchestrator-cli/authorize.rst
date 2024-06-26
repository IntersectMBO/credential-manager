.. _authorize:

Authorizing the Hot Credential
==============================

The next step is to authorize the hot credential so we can vote.
credential NFT, which will be very similar to initializing the cold NFT script.

Step 1: Creating the assets
---------------------------

Technically, this requires the delegation group to sign the transaction, so as the orchestrator, we can't do this by ourselves.
If you have been following along with the example certificates however, you have access to all the signing keys anyway, so we can pretend we belong to the user roles.

To publish the certificate, we need to add it to a transaction.
Our cold credential will be required to authorize the transaction, which means that we need to spend the cold NFT in the transaction.
To spend the cold NFT, our cold NFT locking script will have to authorize the transaction.
Before we can build the transaction, we need to prepare a new output for the cold NFT, as well as a redeemer to spend it.
``orchestrator-cli`` has a command that can help with this, but it in turn requires the current cold NFT output as a JSON file.
We can query this with ``cardano-cli``:

.. code-block:: bash

   $ cardano-cli conway query utxo --address $(cat init-cold/nft.addr) --output-json > cold-nft.utxo

Alternatively, the nix shell provides a command to make this easier:

.. code-block:: bash

   $ fetch-cold-nft-utxo

Now we can prepare the assets for the transaction with ``orchestrator-cli``:

.. code-block:: bash

   $ orchestrator-cli authorize \
     -u cold-nft.utxo \
     --cold-credential-script-file init-cold/credential.plutus \
     --hot-credential-script-file init-hot/credential.plutus \
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
       "cborHex": "830e8201581c533e8af4515ba4cf6035ac7087ae8978e09692fea68e9466f1683f288201581ced846aa91731bfda6ea5e3b63db8de547b201f8dee5013de05accd3a"
   }

We also have a datum for the new output (it should be identical to the input datum)

.. code-block:: bash

   diff <(jq 'to_entries | .[0].value.inlineDatum' < cold-nft.utxo) <(jq '.' < authorize/datum.json)

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
                       "bytes": "ed846aa91731bfda6ea5e3b63db8de547b201f8dee5013de05accd3a"
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
   addr_test1wpy9h326p4caud25k8qs665ts97uht7pmvlm8hd2d84vsxqjudz4q+5000000 lovelace + 1 c8aa0de384ad34d844dc479085c3ed00deb1306afb850a2cde6281f4

Step 4: Create the Authorization Transaction
--------------------------------------------

Now we have everything we need to build the transaction:

.. code-block:: bash

   $ cardano-cli conway transaction build \
     --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
     --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
     --tx-in $(cardano-cli query utxo --address $(cat init-cold/nft.addr) --output-json | jq -r 'keys[0]') \
     --tx-in-script-file init-cold/nft.plutus \
     --tx-in-inline-datum-present \
     --tx-in-redeemer-file authorize/redeemer.json \
     --tx-out "$(cat authorize/value)" \
     --tx-out-inline-datum-file authorize/datum.json \
     --required-signer-hash $(cat example-certificates/children/child-4/child-4.keyhash) \
     --required-signer-hash $(cat example-certificates/children/child-5/child-5.keyhash) \
     --certificate-file authorize/authorizeHot.cert \
     --certificate-script-file init-cold/credential.plutus \
     --certificate-redeemer-value {} \
     --change-address $(cat orchestrator.addr) \
     --out-file authorize/body.json
   Estimated transaction fee: Coin 766032

There is quite a lot going on here, and it warrants an explanation. First we
have our transaction inputs:

.. code-block:: bash

   --tx-in "$(get-orchestrator-ada-only | jq -r '.key')" \
   --tx-in-collateral "$(get-orchestrator-ada-only | jq -r '.key')" \
   --tx-in $(cardano-cli query utxo --address $(cat init-cold/nft.addr) --output-json | jq -r 'keys[0]') \

The first input is from the orchestrator's wallet, and is used to cover the fee.
The second line uses the same input as collateral, as we are running a plutus
script. The next input is the script's current UTxO. Note that these query
commands all assume there is one input at each address, and that we want to use
it.

After the last input, there are some additional options that relate to the
script input:

.. code-block:: bash

   --tx-in-script-file init-cold/nft.plutus \
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
      --certificate-script-file init-cold/credential.plutus \
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

   $ cardano-cli conway query committee-state --cold-script-hash $(cat init-cold/credential.plutus.hash)
   {
       "committee": {
           "scriptHash-533e8af4515ba4cf6035ac7087ae8978e09692fea68e9466f1683f28": {
               "expiration": 50000,
               "hotCredsAuthStatus": {
                   "contents": {
                       "scriptHash": "ed846aa91731bfda6ea5e3b63db8de547b201f8dee5013de05accd3a"
                   },
                   "tag": "MemberAuthorized"
               },
               "nextEpochChange": {
                   "tag": "NoChangeExpected"
               },
               "status": "Active"
           }
       },
       "epoch": 10,
       "threshold": 0
   }
