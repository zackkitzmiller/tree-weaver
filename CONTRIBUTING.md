# Contributing to Tree Weaver

Thank you for your interest in contributing to Tree Weaver! This document provides guidelines and instructions for contributing to the project.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Environment](#development-environment)
- [Workflow](#workflow)
- [Pull Request Process](#pull-request-process)
- [Coding Standards](#coding-standards)
- [Running Tests](#running-tests)
- [Documentation](#documentation)
- [Release Process](#release-process)

## Code of Conduct

This project and everyone participating in it is governed by our [Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code.

## Getting Started

1. Fork the repository
2. Clone your fork:
   ```bash
   git clone https://github.com/YOUR-USERNAME/tree-weaver.git
   cd tree-weaver
   ```
3. Add the original repository as an upstream remote:
   ```bash
   git remote add upstream https://github.com/zackkitzmiller/tree-weaver.git
   ```

## Development Environment

### Prerequisites

- Erlang/OTP 25+
- Rebar3
- Just (optional but recommended)

### Setup

If you have Just installed:

```bash
just setup
```

Otherwise:

```bash
# Compile the project
rebar3 compile

# Run tests
rebar3 eunit

# Start a development shell
rebar3 shell
```

## Workflow

1. Create a new branch from the latest main:
   ```bash
   git checkout main
   git pull upstream main
   git checkout -b feature/your-feature-name
   ```

2. Make your changes, adhering to coding standards

3. Add tests for your changes

4. Commit your changes with clear, descriptive messages:
   ```bash
   git commit -m "Add feature XYZ"
   ```

5. Push your branch to your fork:
   ```bash
   git push origin feature/your-feature-name
   ```

6. Create a pull request

## Pull Request Process

1. Ensure your code passes all tests
2. Update documentation if necessary
3. Fill in the pull request template
4. Request a review from maintainers
5. Address any feedback from reviewers
6. Once approved, a maintainer will merge your PR

## Coding Standards

### Erlang Style Guide

- Use 4 spaces for indentation (no tabs)
- Keep line length to a maximum of 100 characters
- Follow the [Erlang Coding Standards](https://github.com/inaka/erlang_guidelines)
- Module and function names should use snake_case
- Variables should start with uppercase letters (Erlang convention)

### Commit Messages

- Use the present tense ("Add feature" not "Added feature")
- Use the imperative mood ("Move cursor to..." not "Moves cursor to...")
- Limit the first line to 72 characters or less
- Reference issues and pull requests after the first line

## Running Tests

Tests are written using the EUnit framework.

```bash
# Run all tests
just test
# or
rebar3 eunit

# Run a specific test module
just test-one tree_weaver_tests
# or
rebar3 eunit --module=tree_weaver_tests
```

When adding new features, please include appropriate tests. Aim for at least 80% test coverage for new code.

## Documentation

- Update the README.md if you change functionality
- Document all public functions with EDoc comments
- Keep documentation up-to-date with code changes

## Release Process

Only project maintainers can create new releases. The process is:

1. Update version in `src/tree_weaver.app.src`
2. Update CHANGELOG.md
3. Create a release commit
4. Tag the release
5. Push to GitHub
6. Use GitHub Actions to build and publish the release

Contributors can use `just publish VERSION` to simulate the release process locally for testing.

## Questions?

If you have any questions about contributing, please open an issue or contact the maintainers.

---

Thank you for contributing to Tree Weaver!
