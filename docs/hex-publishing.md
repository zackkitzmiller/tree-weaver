# Publishing to Hex.pm

Tree Weaver can be published to [Hex.pm](https://hex.pm), the Erlang/Elixir package manager, making it easily installable through rebar3 or mix.

## Prerequisites

1. A Hex.pm account
2. Proper configuration in project files
3. Clean, tested code

## Configuration

### rebar.config

Ensure your `rebar.config` includes all necessary Hex.pm metadata:

```erlang
{hex, [
    {doc, #{provider => ex_doc}},
    {licenses, ["MIT"]},
    {links, [
        {"GitHub", "https://github.com/zackkitzmiller/tree-weaver"},
        {"Documentation", "https://hexdocs.pm/tree_weaver"}
    ]},
    {description, "A lightning-fast, cross-platform file structure generator"}
]}.

%% Optional: Configure ex_doc for documentation
{ex_doc, [
    {source_url, "https://github.com/zackkitzmiller/tree-weaver"},
    {extras, ["README.md", "CHANGELOG.md"]},
    {main, "README.md"}
]}.
```

### Application Resource File

Ensure `src/tree_weaver.app.src` includes:

```erlang
{application, tree_weaver,
 [{description, "Dynamic File Structure Generator"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, {tree_weaver_app, []}},
  {applications, [
     kernel,
     stdlib,
     crypto,
     jsx
  ]},
  {env, []},
  {modules, [
     tree_weaver_app,
     tree_weaver_sup,
     tree_weaver_cli
  ]},
  {licenses, ["MIT"]},
  {links, [{"GitHub", "https://github.com/zackkitzmiller/tree-weaver"}]}
 ]}.
```

## Publishing Process

### 1. Authenticate with Hex.pm

```bash
rebar3 hex user auth
```

Enter your Hex.pm username and password when prompted.

### 2. Validate Your Package

Test that your package can be built correctly:

```bash
rebar3 hex build
```

This will create a tarball of your project. Review its contents to ensure everything looks correct.

### 3. Publish the Package

Using the `just` command:

```bash
just publish-hex
```

Or directly with rebar3:

```bash
rebar3 hex publish
```

Confirm the publication when prompted.

### 4. Verify Publication

Visit [https://hex.pm/packages/tree_weaver](https://hex.pm/packages/tree_weaver) to ensure your package was published correctly.

## Updating Your Package

To publish a new version:

1. Update the version in `src/tree_weaver.app.src`
2. Update `CHANGELOG.md`
3. Run `just prep-release X.Y.Z` to prepare the release
4. Run `just publish-hex` to publish to Hex.pm

## Using Your Published Package

Once published, others can use Tree Weaver by adding it to their `rebar.config`:

```erlang
{deps, [
    {tree_weaver, "0.1.0"}
]}.
```

Or in a mix project:

```elixir
defp deps do
  [
    {:tree_weaver, "~> 0.1.0"}
  ]
end
```
