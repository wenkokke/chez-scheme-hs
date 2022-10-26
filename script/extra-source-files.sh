#!/bin/sh

# Clean up submodules:
(cd vendor/ChezScheme && git clean -dfqX)
(cd vendor/ChezScheme/lz4 && git clean -dfqX)
(cd vendor/ChezScheme/nanopass && git clean -dfqX)
(cd vendor/ChezScheme/stex && git clean -dfqX)
(cd vendor/ChezScheme/zlib && git clean -dfqX)

# List all files:
find vendor/ChezScheme \
  -type f \
  -and -not -path '**/.circleci/**' \
  -and -not -path '**/.git/**' \
  -and -not -path '**/.github/**' \
  -and -not -path '**/.travis/**' \
  -and -not -path '**/lz4/doc/**' \
  -and -not -path '**/lz4/examples/**' \
  -and -not -path '**/lz4/tests/**' \
  -and -not -path '**/nanopass/doc/**' \
  -and -not -path '**/nanopass/tests/**' \
  -and -not -path '**/stex/doc/**' \
  -and -not -path '**/zlib/doc/**' \
  -and -not -name '.travis.yml' \
  -and -not -name 'appveyor.yml' \
  -and -not -name '.gitattributes' \
  -and -not -name '.gitignore' \
  -and -not -name '.gitmodules' \
  -and -not -name '*.pdf' \
  -and -not -name '*.md' \
  -and -not -iname 'BUILDING' \
  -and -not -iname 'CHANGELOG' \
  -and -not -iname 'FAQ' \
  -and -not -iname 'INDEX' \
  -and -not -iname 'INSTALL' \
  -and -not -iname 'LOG' \
  -and -not -iname 'NEWS' \
  -and -not -iname 'NOTICE' \
  -and -not -iname 'README' \
  -and -not -iname 'TODO'
