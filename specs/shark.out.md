# Markdown Shark Support

The `shark` executable also can work with markdown documents. Two blocks can be
used to run shell-like commands within your markdown documents. The first is
`shark-build` commands. These allow you to specify a build script that is then
built and can be referenced as the context for future `shark-run` blocks.

## Shark Build

```shark-build:gdal-env:84f2ece89c2baae67f461ed371168716821c7ed1309e431f12ba7a491dbc8ef9
((from ghcr.io/osgeo/gdal:ubuntu-small-3.6.4)
 (run (shell "echo 'Something for the log!'")))
```

Once we have a GDAL environment available to us, we can write shell fragments
using that environment.

## Shark Run

```shark-run:gdal-env:8a1a508be0c5214b75f7e5a179206e5126cb9a2f772ec7ca050f58563085f364
$ gdalinfo --version > gdal.version
$ cat gdal.version
GDAL 3.6.4, released 2023/04/17

```

Of course we can make use of the pretty good networking reproducibility.


