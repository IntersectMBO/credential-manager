.. _installation:

Installation
============

There are many ways to install ``cc-sign``, depending on your system.

Prerequisites
-------------

You must have openssl installed to use the tool.

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
   { config, lib, pkgs, credential-manager, ... }: {
     environment.systemPackages = [
       credential-manager.packages.${pkgs.system}.cc-sign;
     ];
   }

Via Nix (non-NixOS)
-------------------

.. code-block:: bash

   nix profile install github:IntersectMBO/credential-manager#cc-sign

Windows
-------

Open a PowerShell window and run the following two commands:

.. code-block:: powershell

   Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
   Invoke-RestMethod -Uri https://raw.githubusercontent.com/IntersectMBO/credential-manager/main/install-cc-sign-windows.ps1 | Invoke-Expression

MacOS (non-Nix)
---------------

Open a terminal window and run the following command:

.. code-block:: bash

   sudo /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/IntersectMBO/credential-manager/main/install-cc-sign-mac-os.sh)"

Linux (non-Nix, X64 only)
-------------------------

Open a terminal window and run the following command:

.. code-block:: bash

   sudo curl -L --output /usr/local/bin/cc-sign https://github.com/IntersectMBO/credential-manager/releases/download/0.1.1.0/cc-sign-linux-x64
