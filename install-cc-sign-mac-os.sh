#!/usr/bin/env bash
set -euo pipefail

ARCH="$(arch)"

if [[ "$ARCH" == "x86_64" ]] || [[ "$ARCH" == "i386" ]]; then
  ZIP_URL="https://github.com/IntersectMBO/credential-manager/releases/download/0.1.5.0/cc-sign-mac-os-intel.zip"
else
  ZIP_URL="https://github.com/IntersectMBO/credential-manager/releases/download/0.1.5.0/cc-sign-mac-os-arm.zip"
fi

tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT

curl -fL --proto '=https' --tlsv1.2 --output "$tmpdir/cc-sign.zip" "$ZIP_URL"

unzip -q "$tmpdir/cc-sign.zip" -d "$tmpdir"

mkdir -p /usr/local/lib
cp -n "$tmpdir/result/bin/"*.dylib /usr/local/lib/

chmod +w "$tmpdir/result/bin/cc-sign"

mkdir -p /usr/local/bin
cp "$tmpdir/result/bin/cc-sign" /usr/local/bin/cc-sign
