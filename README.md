# Constitutional Committee Credential Management System

[![Documentation Status](https://readthedocs.org/projects/credential-manager/badge/?version=latest)](https://credential-manager.readthedocs.io/en/latest/?badge=latest)
[![Hydra Status (Intel Linux)](https://img.shields.io/endpoint?logo=nixos&logoColor=white&label=Hydra%20build%20Intel%20Linux&url=https%3A%2F%2Fci.iog.io%2Fjob%2FIntersectMBO-credential-manager%2Fmain%2Fx86_64-linux.required%2Fshield)](https://ci.iog.io/job/IntersectMBO-credential-manager/main/x86_64-linux.required)
[![Hydra Status (Intel Mac OS)](https://img.shields.io/endpoint?logo=nixos&logoColor=white&label=Hydra%20build%20Intel%20Mac%20OS&url=https%3A%2F%2Fci.iog.io%2Fjob%2FIntersectMBO-credential-manager%2Fmain%2Fx86_64-darwin.required%2Fshield)](https://ci.iog.io/job/IntersectMBO-credential-manager/main/x86_64-darwin.required)
[![Hydra Status (M1 Mac OS)](https://img.shields.io/endpoint?logo=nixos&logoColor=white&label=Hydra%20build%20M1%20Mac%20OS&url=https%3A%2F%2Fci.iog.io%2Fjob%2FIntersectMBO-credential-manager%2Fmain%2Faarch64-darwin.required%2Fshield)](https://ci.iog.io/job/IntersectMBO-credential-manager/main/aarch64-darwin.required)

The Constitutional Committee Credential Management System is a suite of Plutus
scripts and tools for managing Cardano constitutional committee credentials
with an X.509 certificate chain. It provides the following features:

* Separation of capabilities between user roles
* Multi-signature authorization of on-chain governance transactions
* Key rotation without modifying registered committee credentials
* Publicly verifiable authorization of on-chain activity via certificates

## Installing the cc-sign tool

The `cc-sign` tool is a simplified CLI tool for signing transaction with
encrypted openssl private keys. Installation instructions are listed per system
below:

### Prerequisites

You must have `openssl` installed to use this tool.

### Via Nix

```bash
nix profile install github:IntersectMBO/credential-manager#cc-sign
```

### MacOS (Non-nix)

Open a terminal window and run the following command:

```bash
sudo /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/IntersectMBO/credential-manager/main/install-cc-sign-mac-os.sh)"
```

Note that the use of `sudo` will require you to enter your password (your user account password, not your private key pass phrase).

### Windows

Open a PowerShell window and run the following commands:

```powershell
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
Invoke-RestMethod -Uri https://raw.githubusercontent.com/IntersectMBO/credential-manager/main/install-cc-sign-windows.sh | Invoke-Expression
```

### Linux (Non-nix, x64)

Download the executable from https://github.com/IntersectMBO/credential-manager/releases/download/0.1.1.0/cc-sign-linux-x64 and
put it in a directory in your PATH (e.g. /usr/local/bin or /usr/bin).

## Using the cc-sign tool

Before you use the tool, you need to have downloaded the tx body file that needs signing to a known location.

To use cc-sign, open a new terminal window (or PowerShell on Windows). The
command has the following usage pattern:

```bash
cc-sign --private-key-file PRIVATE_KEY_FILE --tx-body-file TX_BODY_FILE --out-file FILE_TO_WRITE_WITNESS_TO
```

The three files (written in all-caps) you need to provide are:

1. `PRIVATE_KEY_FILE` replace this with the filepath of your (encrypted) private key file (e.g. `~/private-keys/my-key.private`)
2. `TX_BODY_FILE` replace this with the filepath where you saved the tx body file (e.g. `~/Downloads/my-tx.body`)
    [!TIP]
    If you downloaded this file from a web browser (e.g. a Gmail attachment) or from slack, it will likely be in `~/Downloads` on MacOS or Linux and `C:\Users\Your Name\Downloads` on Windows.
2. `FILE_TO_WRITE_WITNESS_TO` this is the file path where the resulting witness will be saved (e.g. `~/my-tx.witness`)
    [!TIP]
    It is a good idea to give the witness file a name that associates it with A) who signed it and B) the transaction.
    For example, if your name is John Doe and the tx body file you signed was `my-tx.body` then name the file `my-tx-john-doe.witness`.

The tool will first prompt you to enter the pass phrase for your private key file in order to decrypt it. Type the password and hit Enter/return.
It will then print a sequence of summary items that describes what the transaction does and prompt you to confirm each one.
To confirm, type the letter `y` and hit enter/return. Finally it will prompt you if you want to sign.
Type `y` once more and hit enter and it will write your signature to the file you provided for the `--out-file` argument.
Send this witness file back to the orchestrator who sent you the transaction.

## Documentation

User manual: https://credential-manager.readthedocs.io

