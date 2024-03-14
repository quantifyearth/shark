# Markdown Shark Support

The `shark` executable also can work with markdown documents. Two blocks can be
used to run shell-like commands within your markdown documents. The first is
`shark-build` commands. These allow you to specify a build script that is then
built and can be referenced as the context for future `shark-run` blocks.

## Shark Build

```shark-build:gdal-env
((from ghcr.io/osgeo/gdal:ubuntu-small-3.6.4))
```

Once we have a GDAL environment available to us, we can write shell fragments
using that environment.

## Shark Run

```shark-run:gdal-env
$ gdalinfo --version > gdal.version
$ cat gdal.version
```

Of course we can make use of the pretty good networking reproducibility.


