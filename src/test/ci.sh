#!/bin/bash
set -eux
export OPAMYES=true


# Borrowed from OBuilder's CI

case "$1" in
    zfs)
        dd if=/dev/zero of=/tmp/zfs.img bs=200M count=50
        sudo /sbin/modprobe zfs
        sudo zpool create obuilder-zfs /tmp/zfs.img

        opam exec -- dune exec -- shark md specs/shark.md

        sudo zpool destroy zfs
        sudo rm -f /tmp/zfs.img
        ;;

    *)
        printf "Usage: main.sh [zfs]" >&2
        exit 1
esac