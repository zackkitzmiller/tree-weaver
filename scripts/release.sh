#!/bin/bash
set -e

# Determine version
VERSION=$(git describe --tags --abbrev=0)

# Build for all platforms
./scripts/build.sh

# Create GitHub release
gh release create $VERSION \
    dist/tree-weaver-linux-amd64 \
    dist/tree-weaver-darwin-amd64 \
    dist/tree-weaver-windows-amd64.exe \
    -F CHANGELOG.md
