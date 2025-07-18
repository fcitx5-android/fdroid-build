# fdroid-build

Build and deploy apks to our fdroid repo.

## Development

All packages are defined in the `app/Packages.hs` file. You can add new packages by defining a `PackageDesc` record and adding it to the `packages` list. Take the moegirl dictionary as an example:

```haskell
moegirl :: PackageDesc
moegirl =
  PackageDesc
    { descProjectName = mkProjectName "pinyin-moegirl",
      descPackageName = mkPackageName "pinyin_moegirl",
      descVersionSource = GitHubRelease "outloudvi" "mw2fcitx",
      descCreateVersionName = pure,
      descCreateVersionCode = readInteger,
      descPreBuild = \(_, dictVer, _) projectDir ->
        downloadFile
          (githubReleaseFileUrl "outloudvi" "mw2fcitx" dictVer "moegirl.dict")
          (pinyinDictPath projectDir </> T.unpack dictVer <.> "dict"),
      descAppNameDebug = "Fcitx5 for Android (moegirl dict | Debug)",
      descAppNameRelease = "Fcitx5 for Android (moegirl dict)",
      descPluginDesc = "Fcitx 5 Pinyin Dictionary from zh.moegirl.org.cn"
    }
```

where:

- `descProjectName`: The project name of the package, starts with `fcitx5-android-plugin-` (added by `mkProjectName`).
- `descPackageName`: The package name of the plugin, starts with `org.fcitx5.android.plugin.` (added by `mkPackageName`).
- `descVersionSource`: The nvchecker source for the package version. See `app/Nvchecker.hs` for more details.
- `descCreateVersionName`: A function to create the version name from the nvchecker version string.
- `descCreateVersionCode`: A function to create the version code from the nvchecker version string.
- `descPreBuild`: A function to perform pre-build actions, such as downloading files. Version information is passed as `(Nvchecker Version, Version Name, Version Code)` and `projectDir` is the directory where the project is built.

## Usage

The following dependencies are required to run this project:
- [`plugin-scaffold`](https://github.com/fcitx5-android/plugin-scaffold)
- [`nvchecker`](https://github.com/lilydjwg/nvchecker)
- `unzip`
- `rsync`

You can use the `nix develop` to enter a Nix shell with all dependencies. Once inside the shell, you can build and run with cabal:

```bash
cabal build
cabal run
```

If you don't want to use Nix, you can set up the Haskell environment and install the dependencies manually.

### Build all packages

```bash
cabal run fdroid-build
```

### Build a specific package

```bash
cabal run fdroid-build -- <package-name>
```

For example, to build moegirl dictionary:
```bash
cabal run fdroid-build -- org.fcitx.fcitx5.android.plugin.pinyin_moegirl
```

### Build and deploy all packages

```bash
cabal run fdroid-build -- deploy
```

After running this command, the built APKs will be signed and deployed to our fdroid repository.

### Clean build artifacts

Build artifacts are stored in the `_build`, where unsigned and signed APKs are stored in `_build/unsigned` and `_build/signed` directories respectively. To clean these artifacts, run:

```bash
cabal run fdroid-build -- clean
```

### Skip fdroid version check

By default, the build process checks our fdroid repository version to skip building if the version is already up to date. If you want to skip this check, you can use the `-s` or `--skip-fdroid` flag:

```bash
cabal run fdroid-build -- -s
```
