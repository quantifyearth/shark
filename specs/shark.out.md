
# Markdown Shark Support

The `shark` executable also can work with markdown documents. Two blocks can be
used to run shell-like commands within your markdown documents. The first is
`shark-build` commands. These allow you to specify a build script that is then
built and can be referenced as the context for future `shark-run` blocks.

## Shark Build

```shark-build:gdal-env:ec610a45b8d858c2eba37fd40dd1764890828557c1c43fa84ec88c7fcdc087c1
((from osgeo/gdal:ubuntu-small-3.6.3)
 (run (shell "mkdir -p /data && echo 'Something for the log!'")))
```

Once we have a GDAL environment available to us, we can write shell fragments
using that environment.

## Shark Run

```shark-run:gdal-env:1dd3d7fdb8f1f485dd5aa0d5f383209a60aca98e67552d03a54c99be8b610eca
$ gdalinfo --version > /data/gdal.version
```

Shark keeps track of inputs and outputs. In the next code block, Shark knows to wire
up `/data/gdal.version` into the container.

```shark-run:gdal-env:e02469d800253ccf95e53b583e4a91465375a4e41479a67408331ecdeedb713e
$ cat /data/gdal.version
GDAL 3.6.3, released 2023/03/07

```

