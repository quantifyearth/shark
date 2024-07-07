
# Markdown Shark Support

The `shark` executable also can work with markdown documents. Two blocks can be
used to run shell-like commands within your markdown documents. The first is
`shark-build` commands. These allow you to specify a build script that is then
built and can be referenced as the context for future `shark-run` blocks.

## Shark Build

```shark-build:gdal-env:4213afafe74bc720d4bb210f21c97d54a361a80a838c248f26dd7dc019a40ac2
((from ghcr.io/osgeo/gdal:ubuntu-small-3.6.3@sha256:bfa7915a3ef942b4f6f61223ee57eadbb469d6fb4a5fbf562286d1473f15eaab)
 (run (shell "mkdir -p /data && echo 'Something for the log!'")))
```

Once we have a GDAL environment available to us, we can write shell fragments
using that environment.

## Shark Run

```shark-run:gdal-env:8113359387d02c4e29df7013f0cd2d699c4f1303d99fe065204b579baf0dd509
gdalinfo --version > /data/gdal.version
curl -s https://france-geojson.gregoiredavid.fr/repo/regions/occitanie/region-occitanie.geojson > /data/region-occitanie.geojson
```

Shark keeps track of inputs and outputs. In the next code block, Shark knows to wire
up `/data/gdal.version` into the container.

```shark-run:gdal-env:101cf72cd986ca89f23d87c6af4c86908385fd977ebdd4f010ebf6ae8d0b04c6
cat /shark/8113359387d02c4e29df7013f0cd2d699c4f1303d99fe065204b579baf0dd509/gdal.version
GDAL 3.6.3, released 2023/03/07

```

## Shark Publish

Shark allows you to export data directly from the Shark world using a publish block. By default
this will publish to a `_shark` directory in the current working directory. Use the same file path
conventions to export data blobs.

```shark-publish
/obuilder-zfs/result/8113359387d02c4e29df7013f0cd2d699c4f1303d99fe065204b579baf0dd509/.zfs/snapshot/snap/rootfs/data/gdal.version
```
