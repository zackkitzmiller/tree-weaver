# Tree Weaver justfile
# Install just: https://github.com/casey/just

# List available commands
default:
    @just --list

# Set up development environment
setup:
    @echo "Setting up Tree Weaver development environment..."
    @command -v rebar3 >/dev/null 2>&1 || (echo "Installing rebar3..." && curl -O https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3 && sudo mv rebar3 /usr/local/bin/)
    @command -v erlang >/dev/null 2>&1 || (echo "Please install Erlang: brew install erlang (macOS) or sudo apt-get install erlang (Ubuntu)")
    @echo "Setup complete."

# Compile the project
compile:
    rebar3 compile

# Clean build artifacts
clean:
    rebar3 clean

# Run tests
test:
    rebar3 eunit

# Run specific test
test-one TEST:
    rebar3 eunit --module={{TEST}}

# Create executable
build:
    rebar3 escriptize

# Build release packages for all platforms
release: build
    mkdir -p dist
    cp _build/default/bin/tree-weaver dist/tree-weaver-$(uname -s)-$(uname -m)
    @echo "Release built at dist/tree-weaver-$(uname -s)-$(uname -m)"

# Prepare a release (bumps version and updates changelog)
prep-release VERSION:
    #!/usr/bin/env bash
    set -e
    echo "Preparing Tree Weaver v{{VERSION}} for release..."

    # Validate version format
    if [[ ! "{{VERSION}}" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
        echo "Error: Version must be in format x.y.z"
        exit 1
    fi

    # Update version in .app.src
    sed -i.bak "s/{vsn, \"[0-9]*\.[0-9]*\.[0-9]*\"}/{vsn, \"{{VERSION}}\"}/" src/tree_weaver.app.src
    rm -f src/tree_weaver.app.src.bak

    # Update CHANGELOG.md
    DATE=$(date +%Y-%m-%d)
    sed -i.bak "s/## \[Unreleased\]/## \[Unreleased\]\n\n## \[{{VERSION}}\] - ${DATE}/" CHANGELOG.md
    sed -i.bak "s|\[Unreleased\]: https://github.com/zackkitzmiller/tree-weaver/compare/v[0-9]*\.[0-9]*\.[0-9]*...HEAD|\[Unreleased\]: https://github.com/zackkitzmiller/tree-weaver/compare/v{{VERSION}}...HEAD\n\[{{VERSION}}\]: https://github.com/zackkitzmiller/tree-weaver/compare/v$(grep -oP '{vsn, "\K[^"]*' src/tree_weaver.app.src.bak)...v{{VERSION}}|" CHANGELOG.md
    rm -f CHANGELOG.md.bak

    echo "Version bumped to {{VERSION}} and CHANGELOG.md updated"
    echo "Next steps:"
    echo "1. Review and finalize CHANGELOG.md"
    echo "2. Commit changes: git add src/tree_weaver.app.src CHANGELOG.md"
    echo "3. Push to trigger release: git commit -m \"Prepare release v{{VERSION}}\" && git push origin main"

# Publish to Hex.pm
publish-hex:
    #!/usr/bin/env bash
    set -e
    echo "Publishing to Hex.pm..."

    if ! rebar3 hex user whoami &> /dev/null; then
        echo "Not logged in to Hex.pm. Please run: rebar3 hex user auth"
        exit 1
    fi

    # Publish to Hex.pm
    rebar3 hex publish

    echo "Successfully published to Hex.pm!"

# Create a new release on GitHub (requires GitHub CLI)
publish VERSION:
    #!/usr/bin/env bash
    set -e
    echo "Releasing Tree Weaver v{{VERSION}}..."

    # Validate version format
    if [[ ! "{{VERSION}}" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
        echo "Error: Version must be in format x.y.z"
        exit 1
    fi

    # Update version in .app.src
    sed -i.bak "s/{vsn, \"[0-9]*\.[0-9]*\.[0-9]*\"}/{vsn, \"{{VERSION}}\"}/" src/tree_weaver.app.src
    rm -f src/tree_weaver.app.src.bak

    # Commit version change
    git add src/tree_weaver.app.src
    git commit -m "Bump version to {{VERSION}}"

    # Create tag
    git tag -a "v{{VERSION}}" -m "Release v{{VERSION}}"

    # Push to remote
    git push origin main
    git push origin "v{{VERSION}}"

    # Build release assets
    just release

    # Create GitHub release
    gh release create "v{{VERSION}}" \
        --title "Tree Weaver v{{VERSION}}" \
        --notes "Release v{{VERSION}}" \
        dist/*

# Start a development shell
shell:
    rebar3 shell

# Generate documentation
docs:
    mkdir -p docs
    erlc -o docs/ doc/*.erl
    erl -noshell -run edoc_run files '["doc/*.erl"]' '[{dir, "docs"}]'

# Check code style
lint:
    rebar3 dialyzer

# Run the application with a sample tree file
run-example:
    @if [ -f "feelsbank.tree" ]; then \
        ./tree-weaver feelsbank.tree --dry-run; \
    else \
        echo "Error: Example file feelsbank.tree not found"; \
    fi

# Create a sample tree file
create-sample:
    #!/usr/bin/env bash
    cat > sample.tree << EOF
    sample-project/
    |-- src/
    |   |-- main.erl
    |   |-- utils.erl
    |-- include/
    |   |-- types.hrl
    |-- test/
    |   |-- main_tests.erl
    |-- README.md
    |-- rebar.config
    EOF
    echo "Created sample.tree file"

# Run with sample
demo: create-sample build
    ./_build/default/bin/tree-weaver sample.tree --dry-run
