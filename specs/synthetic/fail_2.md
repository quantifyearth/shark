# Simple multistage test case 2

Use alpline as a minimal image

```shark-build:image
((from ghcr.io/osgeo/gdal:ubuntu-small-3.8.5))
```

Now run multiple commands in one block:

```shark-run:image
true
false
true
```
