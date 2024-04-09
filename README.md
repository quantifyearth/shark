Shark
-----

A shell with pretty good reproducibility (only works on Linux).

## Build

Building images is just like OBuilder. You can specify a spec and run the
builder in a store of your choice. For example, here is a simple GDAL specification
that creates a new `tmf` user.

<!-- $MDX file=specs/tmf.spec -->
```
((from ghcr.io/osgeo/gdal:ubuntu-small-3.6.4)
 (run (network host)
  (shell
   "apt-get update -qqy && apt-get install -qy git wget libpq-dev python3-pip && rm -rf /var/lib/apt/lists/* && rm -rf /var/cache/apt/*"))
 (run (shell "useradd -ms /bin/bash -u 1000 tmf")) 
 (workdir /home/tmf/app)
 (run (shell "chown -R tmf:tmf /home/tmf"))
 (user (uid 1000) (gid 1000))
 (run (shell "mkdir /home/tmf/app/data")) 
 (workdir /home/tmf/app))
```

You can build this with

<!-- $MDX skip -->
```sh
$ shark build . -f specs/tmf.spec --store=zfs:obuilder-zfs
```

## Shell

Anything that has been built is far game to be used as an environment for an
interactive shell. Once you have the ID of the build you can drop in.

<!-- $MDX skip -->
```sh
shark run --store=zfs:obuilder-zfs 7ddfd6bfe7de83bd0ccdc763077b8088343182a33724d5387a906ac6a941b552
```

## Configuration

A lot of the commands to shark CLI commands can get repetitive. You might want to place a configuration
file in your home directory under `~/.shark`. A default is provided from the command line.

```sh
$ shark config
((store (Zfs obuilder-zfs)))
```

## Markdown Support 

Another mode of operation for Shark is via markdown files. Here you can use codeblocks to write
`shark-build` and `shark-run` commands. See [an example markdown file](./specs/shark.md) and
the [promoted output version](./specs/shark.out.md).

```sh
$ patdiff -ascii specs/shark.md specs/shark.out.md
------ specs/shark.md
++++++ specs/shark.out.md
@|-1,40 +1,33 ============================================================
 |
 |# Markdown Shark Support
 |
 |The `shark` executable also can work with markdown documents. Two blocks can be
 |used to run shell-like commands within your markdown documents. The first is
 |`shark-build` commands. These allow you to specify a build script that is then
 |built and can be referenced as the context for future `shark-run` blocks.
 |
 |## Shark Build
 |
-|```shark-build:gdal-env
+|```shark-build:gdal-env:ec610a45b8d858c2eba37fd40dd1764890828557c1c43fa84ec88c7fcdc087c1
 |((from osgeo/gdal:ubuntu-small-3.6.3)
 | (run (shell "mkdir -p /data && echo 'Something for the log!'")))
 |```
 |
 |Once we have a GDAL environment available to us, we can write shell fragments
 |using that environment.
 |
 |## Shark Run
 |
-|```shark-run:gdal-env
+|```shark-run:gdal-env:1dd3d7fdb8f1f485dd5aa0d5f383209a60aca98e67552d03a54c99be8b610eca
 |$ gdalinfo --version > /data/gdal.version
 |```
 |
 |Shark keeps track of inputs and outputs. In the next code block, Shark knows to wire
 |up `/data/gdal.version` into the container.
 |
-|```shark-run:gdal-env
+|```shark-run:gdal-env:e02469d800253ccf95e53b583e4a91465375a4e41479a67408331ecdeedb713e
 |$ cat /data/gdal.version
-|```
-|
-|## Shark Publish
-|
-|Shark allows you to export data directly from the Shark world using a publish block. By default
-|this will publish to a `_shark` directory in the current working directory. Use the same file path
-|conventions to export data blobs.
-|
-|```shark-publish
-|/data/gdal.version
+|GDAL 3.6.3, released 2023/03/07
+|
 |```
+|
[1]
```

The markdown files support a simple templating language with variables defined in YAML frontmatter.
With this you can run `shark template index.md --output-dir=./out` which will populate `./out` with all of the combinations of variables in separate markdown files ready to be processed 

<!-- $MDX file=specs/template.md -->
```
---
path_prefix: /var/
project_ids:
  - 1234
  - 4321
---

With templating support, Shark markdown files can support a limited amount of flexibility
in how they are interpretted at runtime.

~~~shark-build:gdal-env
((from ghcr.io/osgeo/gdal:ubuntu-small-3.6.4))
~~~

The syntax is light-weight and similar to Github Actions. Simply add some frontmatter for each
variable and quote them with `%{project_ids}`. Separate nested varibales with a dot.

~~~shark-run:gdal-env
$ echo %{project_ids} > %{path_prefix}%{project_ids}.txt
$ cat %{path_prefix}%{project_ids}.txt
~~~
```


