#!/bin/bash
set -e

# Build the Erlang application
rebar3 compile
rebar3 escriptize

# Create distribution packages
mkdir -p dist

# Linux
GOOS=linux GOARCH=amd64 rebar3 escriptize
cp _build/default/bin/tree-weaver dist/tree-weaver-linux-amd64

# macOS
GOOS=darwin GOARCH=amd64 rebar3 escriptize
cp _build/default/bin/tree-weaver dist/tree-weaver-darwin-amd64

# Windows
GOOS=windows GOARCH=amd64 rebar3 escriptize
cp _build/default/bin/tree-weaver dist/tree-weaver-windows-amd64.exe

echo "Build complete. Artifacts in dist/"
