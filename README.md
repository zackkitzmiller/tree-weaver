# 🌳 Tree Weaver

A lightning-fast, cross-platform file structure generator written in Erlang. Tree Weaver enables you to easily define and create directory structures using simple tree-like text files.

![Version](https://img.shields.io/badge/version-0.1.0-blue)
![License](https://img.shields.io/badge/license-MIT-green)
![Erlang](https://img.shields.io/badge/erlang-OTP%2025+-red)

## 📋 Table of Contents

- [Overview](#-overview)
- [Features](#-features)
- [Installation](#-installation)
- [Usage](#-usage)
- [Examples](#-examples)
- [File Format](#-file-format)
- [Command Line Options](#-command-line-options)
- [Development](#-development)
- [Testing](#-testing)
- [Contributing](#-contributing)
- [License](#-license)

## 🔍 Overview

Tree Weaver transforms text-based tree representations into actual directory structures. It's perfect for:

- Quickly scaffolding new projects
- Creating consistent file structures across teams
- Documenting and implementing project architectures
- Setting up development, testing, and CI environments

## ✨ Features

- **Simple Syntax**: Uses familiar tree-like text format
- **Cross-Platform**: Works on Linux, macOS, and Windows
- **Lightning Fast**: Written in Erlang for optimal performance
- **Flexible**: Support for comments and various formatting options
- **Dry Run Mode**: Preview changes before creating files
- **Custom Destinations**: Specify output locations
- **Intelligent Scaffolding**: (Coming soon) AI-powered project templates

## 🚀 Installation

### Prerequisites

- Erlang/OTP 25 or newer
- Rebar3 (Erlang build tool)

### Installing Erlang

#### macOS (Homebrew)

```bash
brew update
brew install erlang
```

#### Ubuntu/Debian

```bash
# Add Erlang Solutions repository
wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
sudo dpkg -i erlang-solutions_2.0_all.deb
sudo apt-get update
sudo apt-get install esl-erlang
```

#### Windows

1. Download from [Official Erlang Website](https://www.erlang.org/downloads)
2. Run the installer
3. Add Erlang to system PATH

### Using Prebuilt Binaries

Download the latest release from our [GitHub Releases page](https://github.com/zackkitzmiller/tree-weaver/releases).

### Building from Source

```bash
# Clone the repository
git clone https://github.com/zackkitzmiller/tree-weaver.git
cd tree-weaver

# If you have 'just' installed (recommended)
just setup
just build

# Or using rebar3 directly
rebar3 compile
rebar3 escriptize
```

The executable will be available at `_build/default/bin/tree-weaver`.

## 🎬 Usage

```bash
# Basic usage with a tree file
tree-weaver project.tree

# Dry run (preview without creating files)
tree-weaver --dry-run project.tree

# Custom destination
tree-weaver --dest /path/to/destination project.tree

# Use current directory as root (skips creating the root directory)
tree-weaver --dest . project.tree

# Display help
tree-weaver
```

## 📝 Examples

### Simple Project Structure

Create a file `simple-project.tree`:

```
simple-project/
|-- src/
|   |-- main.erl
|-- test/
|   |-- main_tests.erl
|-- README.md
```

Run:

```bash
tree-weaver simple-project.tree
```

This creates:

```
simple-project/
├── src/
│   └── main.erl
├── test/
│   └── main_tests.erl
└── README.md
```

### Complex Web Application

See the `feelsbank.tree` example in the repository for a complex Go web application structure.

## 📄 File Format

Tree Weaver uses a simple text format to represent directory structures:

1. First line: Root directory name
2. Subsequent lines: Directory structure with indentation
3. Lines ending with `/`: Directories
4. Lines without `/`: Files
5. Comments: Use `# Comment text` anywhere in a line

Example with comments:

```
my-project/  # Root directory
|-- src/     # Source code
|   |-- main.erl  # Main entry point
|-- README.md     # Documentation
```

### Indentation Rules

- Use `|--` to indicate items
- Use `|   |--` or multiple spaces for nested items
- Consistent indentation is recommended but not strictly required

## 🔧 Command Line Options

Tree Weaver supports the following command line options:

| Option | Description |
|--------|-------------|
| `--dry-run` | Show what would be created without making changes |
| `--dest PATH` | Specify destination path (default: current directory) |
| `--dest .` | Use current directory as root (skip creating root directory) |

## 💻 Development

### Using Just

We recommend using [Just](https://github.com/casey/just) for common development tasks:

```bash
# Setup development environment
just setup

# Compile
just compile

# Run tests
just test

# Create executable
just build

# Create sample and run with it
just demo
```

### Manual Development

If you prefer not to use Just:

```bash
# Compile the project
rebar3 compile

# Run tests
rebar3 eunit

# Create executable
rebar3 escriptize

# Start dev shell
rebar3 shell
```

## 🧪 Testing

Run the test suite:

```bash
just test
# or
rebar3 eunit
```

To run a specific test:

```bash
just test-one tree_weaver_tests
# or
rebar3 eunit --module=tree_weaver_tests
```

## 🤝 Contributing

We welcome contributions! See [CONTRIBUTING.md](CONTRIBUTING.md) for details.

Quick steps:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add or update tests
5. Submit a pull request

Please follow the [Code of Conduct](CODE_OF_CONDUCT.md).

## 📜 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgements

- The Erlang community
- All contributors and supporters

---

Built with ❤️ by [Zack Kitzmiller](https://github.com/zackkitzmiller)# Tree Weaver
