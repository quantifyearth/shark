
# Markdown Shark Support

The `shark` executable also can work with markdown documents. Two blocks can be
used to run shell-like commands within your markdown documents. The first is
`shark-build` commands. These allow you to specify a build script that is then
built and can be referenced as the context for future `shark-run` blocks.

## Shark Build

```shark-build:gdal-env
((from osgeo/gdal:ubuntu-small-3.6.3)
 (run (shell "mkdir -p /data && echo 'Something for the log!'")))
```

Once we have a GDAL environment available to us, we can write shell fragments
using that environment.

## Shark Run

```shark-run:gdal-env
$ gdal --version > /data/gdal.version
```

Shark keeps track of inputs and outputs. In the next code block, Shark knows to wire
up `/data/gdal.version` into the container.

```shark-run:gdal-env
$ cat /data/gdal.version
```

## Shark Publish

Shark allows you to export data directly from the Shark world using a publish block. By default
this will publish to a `_shark` directory in the current working directory. Use the same file path
conventions to export data blobs.

```shark-publish
/data/gdal.version
```
