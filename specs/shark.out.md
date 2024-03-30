# Markdown Shark Support

The `shark` executable also can work with markdown documents. Two blocks can be
used to run shell-like commands within your markdown documents. The first is
`shark-build` commands. These allow you to specify a build script that is then
built and can be referenced as the context for future `shark-run` blocks.

## Shark Build

```shark-build:gdal-env:f2021ca0e994edd2acbdf227ec98714525af89f8c7ae738af686beb3f5969af9
((from osgeo/gdal:ubuntu-small-3.6.3)
 (run (shell "echo 'Something for the log!'")))
```

Once we have a GDAL environment available to us, we can write shell fragments
using that environment.

## Shark Run

```shark-run:gdal-env:00c8a07aff287577bce80fed68567b86f6cefcfd9376769c42942c16020ce9c5
$ mkdir /data
$ gdalinfo --version > /data/gdal.version
$ cat /data/gdal.version
GDAL 3.6.3, released 2023/03/07

```

Of course we can make use of the pretty good networking reproducibility.


