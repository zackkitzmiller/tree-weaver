# Release Process

This document outlines the process for creating and publishing new releases of Tree Weaver.

## Version Numbering

Tree Weaver follows [Semantic Versioning](https://semver.org/):

- **MAJOR** version for incompatible API changes
- **MINOR** version for backward-compatible functionality
- **PATCH** version for backward-compatible bug fixes

## Creating a Release

### Manual Release Process

1. **Update Version**

   Update the version number in `src/tree_weaver.app.src`:

   ```erlang
   {application, tree_weaver,
    [{description, "Dynamic File Structure Generator"},
     {vsn, "X.Y.Z"}, % <- Update this line
     ...
   ```

2. **Update Changelog**

   Add release notes to `CHANGELOG.md` with:
   - New features
   - Bug fixes
   - Breaking changes
   - Contributors

3. **Commit Changes**

   ```bash
   git add src/tree_weaver.app.src CHANGELOG.md
   git commit -m "Bump version to X.Y.Z"
   ```

4. **Create Tag**

   ```bash
   git tag -a vX.Y.Z -m "Release vX.Y.Z"
   ```

5. **Push Changes**

   ```bash
   git push origin main
   git push origin vX.Y.Z
   ```

6. **Create GitHub Release**

   ```bash
   # Build release assets
   just release

   # Create GitHub release
   gh release create vX.Y.Z \
       --title "Tree Weaver vX.Y.Z" \
       --notes-file RELEASE_NOTES.md \
       dist/*
   ```

### Automated Release Process

Tree Weaver also supports automated releases via GitHub Actions:

1. Update version in `src/tree_weaver.app.src`
2. Commit and push to the main branch
3. The GitHub Action will:
   - Detect version change
   - Create a tag
   - Build release assets
   - Publish a new GitHub release

## Using the Just Command

For convenience, you can use the `just publish` command:

```bash
just publish X.Y.Z
```

This command:

1. Updates the version in `src/tree_weaver.app.src`
2. Commits the change
3. Creates a git tag
4. Pushes to GitHub (triggering the automated release)

## Distribution

Release artifacts are distributed via:

1. **GitHub Releases**: Pre-built binaries for Linux, macOS, and Windows
2. **Source Code**: For users who prefer to build from source

## Verification

After release:

1. Download the released binaries
2. Test basic functionality
3. Verify documentation is correct
4. Check that the version number is displayed correctly
