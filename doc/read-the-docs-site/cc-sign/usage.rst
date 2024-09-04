.. _usage:

Usage
=====

The usage of the command is as follows:

.. code-block:: bash

   cc-sign --help

   Usage: cc-sign [-v|--version] [-q|--quiet] [-y|--yes]
                  (-k|--private-key-file FILEPATH) (-t|--tx-body-file FILEPATH)
                  (-o|--out-file FILEPATH)

     CLI application for signing CC Credential Manager transactions.

   Available options:
     -h,--help                Show this help text
     -v,--version             Show version.
     -q,--quiet               Do not print summary output (implies -y)
     -y,--yes                 Automatically confirm all prompts
     -k,--private-key-file FILEPATH
                              The PEM file containing your encrypted private key
     -t,--tx-body-file FILEPATH
                              The file containing the transaction body to sign.
     -o,--out-file FILEPATH   A file path to save the transaction witness file.

There are three required options that must always be provided:

- ``--private-key-file``: This is the filepath of your private key file which you will use to sign the transaction.
- ``--tx-body-file``: This is the filepath of the transaction body file which you will sign.
- ``--out-file``: This is the filepath where the resulting witness file will be saved.

The command will first prompt you for the pass phrase you used to encrypt the private key file.
Enter it to decrypt the private key.
Then it will print out a series of summary items and ask you to confirm the details for each.
To confirm, type the character ``y`` and hit enter / return.
Typing anything else or just hitting enter / return will abort the signing.
Here is an example output:

.. code-block:: bash

   $ cc-sign \
      --tx-body-file authorize/body.json \
      --private-key-file example-certificates/children/child-4/child-4.private \
      --out-file authorize/child-4.witness

   Enter pass phrase for example-certificates/children/child-4/child-4.private:
   Checking transaction body file... OK

   Checking transaction purpose...
   Hot credential authorization transaction.
   Hot credential script hash: b8928f246d726b59c51f33fc9d643b808dd273e5d9985762e464783d.
   Is the transaction doing what you expect? (yN): y

   Check transaction certificates...
   Authorize committee hot credential certificate found
   Cold credential: ScriptHashObj (ScriptHash "533e8af4515ba4cf6035ac7087ae8978e09692fea68e9466f1683f28")
   Hot credential: ScriptHashObj (ScriptHash "b8928f246d726b59c51f33fc9d643b808dd273e5d9985762e464783d")
   Is this certificate correct? (yN): y

   Check transaction votes...
   No votes cast, as expected

   Check transaction output #0...
   Send to address addr_test1wrd2665l5depddaeg9cke7w58de9tc0q0x03recs9cm9deqfkxg0v
   5000000 Lovelace
   1 c8aa0de384ad34d844dc479085c3ed00deb1306afb850a2cde6281f4
   Cold NFT datum found
   Is this output OK? (yN): y

   Check transaction output #1...
   Send to address addr_test1vpaj9ejw5hgrl0af4frsryt9y78hj477hdm07wnmuh5paeswgza39
   599988628714 Lovelace
   Is this output OK? (yN): y

   Check extra tx body fields...

   Check transaction signatories...
   Requires signature from 7c4ce0c3eca1b077d8465cf3b44db18beea87bacf55c05c9b4d0317c (you can sign)
   Requires signature from a263b5a55cb7b8728a0a97092fad7054117f7695897990bc1ab499b4
   Do you wish to sign this transaction? (yN): y
   Saved witness to authorize/child-4.witness

Once you have the witness file, send it back to the orchestrator who gave you the transaction to sign.
