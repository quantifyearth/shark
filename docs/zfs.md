# ZFS Setup

The best store to use as a backend is probably ZFS. A simple setup without
changing your whole system to ZFS might look something like:


```
truncate --size XG zfs.img
sudo zpool create obuilder-zfs $PWD/zfs.img
```

And then you can run shark (from source, in a development mode).

```
sudo env "PATH=$PATH" dune exec -- shark md specs/shark.md --store=zfs:obuilder-zfs --verbose
```

If your system is booted from ZFS, then there might some `<hostname>` ZFS pool, and you will want
the datasets to be stored under a sub-dataset name for example `shark`.

```
sudo env "PATH=$PATH" dune exec -- shark md specs/shark.md --store=zfs:<hostname>:shark --zfs-path-without-pool --verbose
```

The files will be available from `/shark/results/<hash>/rootfs` after this.

