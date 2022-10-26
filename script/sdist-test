#!/bin/bash

set -euxo pipefail

TAR="${TAR:-tar}"
CABAL="${CABAL:-cabal}"
TMPDIR=$(mktemp -d 2>/dev/null || mktemp -d -t 'chez-scheme-hs-test-sdist')

$CABAL v2-sdist --output-directory="$TMPDIR"
$TAR --extract --gzip --strip-components=1 -f "$TMPDIR"/chez-scheme-hs-*.tar.gz -C "$TMPDIR"
(cd $TMPDIR && $CABAL v2-build)
