.. _cc-sign:

CC Signing Tool
===============

The CC Signing Tool is a command-line tool for members of the Membership,
Delegation, and Voting groups to sign transactions. Even though ``cardano-cli``
can be used to sign transactions, ``cc-sign`` offers several benefits compared
to using ``cardano-cli``:

- It is designed to read signing keys from encrypted PEM files generated by ``openssl``. 
- It is aware of how the credential-manager scripts work and displays output
  that is informed by this knowledge, instead of the generic output produced by
  ``cardano-cli transaction view``.
- It displays information about the transaction being signed for confirmation,
  which is a separate command in ``cardano-cli``.

.. toctree::
   :maxdepth: 1
   :titlesonly:

   installation
   usage
