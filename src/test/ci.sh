#!/bin/bash
set -eux
export OPAMYES=true


# Borrowed from OBuilder's CI

case "$1" in
    zfs)
        dd if=/dev/zero of=/tmp/zfs.img bs=100M count=50
        ZFS_LOOP=$(sudo losetup -f)
        sudo losetup -P "$ZFS_LOOP" /tmp/zfs.img
        sudo /sbin/modprobe zfs
        sudo zpool create obuilder-zfs "$ZFS_LOOP"

        opam exec -- dune exec -- shark md specs/shark.md

        sudo zpool destroy zfs
        sudo losetup -d "$ZFS_LOOP"
        sudo rm -f /tmp/zfs.img
        ;;

    *)
        printf "Usage: main.sh [zfs]" >&2
        exit 1
esac