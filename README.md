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

### Via Nix

```bash
nix profile install github:IntersectMBO/credential-manager#cc-sign
```

### MacOS (Non-nix)

Open a terminal window and run the following command:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/IntersectMBO/credential-manager/main/install-cc-sign-mac-os.sh)"
```

### Windows

Pending

### Linux (Non-nix, x64)

Download the executable from https://github.com/IntersectMBO/credential-manager/releases/download/0.1.1.0/cc-sign-linux-x64 and
put it in a directory in your PATH (e.g. /usr/local/bin).

## Prerequisites

You must have `openssl` installed to use this tool.

## Documentation

User manual: https://credential-manager.readthedocs.io

