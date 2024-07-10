#!/bin/bash
set -eux
export OPAMYES=true
export OCAMLRUNPARAM=b

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

    rsync_copy)
        sudo mkdir /rsync
        sudo chown "$(whoami)" /rsync

        sudo "$GITHUB_WORKSPACE/_build/install/default/bin/shark" md specs/shark.md --store=rsync:/rsync --rsync-mode=hardlink --verbose
        
        cat ./_shark/gdal.version

        sudo "$GITHUB_WORKSPACE/_build/install/default/bin/shark" md specs/build-test.md --store=rsync:/rsync --rsync-mode=hardlink --verbose
        
        md5sum ./_shark/bin/littlejohn

		# Expect a failure but with output.
		if sudo "$GITHUB_WORKSPACE/_build/install/default/bin/shark" md specs/shark.failure.md --store=rsync:/rsync --rsync-mode=hardlink; then
			exit 1
		else
			echo "Successfully Failed"
		fi
        
		# We run the failed build twice to check the failure logic.
		# It should find the failed build, delete it and then fail again.
		if sudo "$GITHUB_WORKSPACE/_build/install/default/bin/shark" md specs/shark.failure.md --store=rsync:/rsync --rsync-mode=hardlink --verbose; then
			exit 1
		else
			echo "Successfully Failed"
		fi

        for d in `find specs -name "succeed*.md"`; do
            sudo "$GITHUB_WORKSPACE/_build/install/default/bin/shark" md $d --store=rsync:/rsync --rsync-mode=hardlink --verbose
        done
        for d in `find specs -name "fail*.md"`; do
            if sudo "$GITHUB_WORKSPACE/_build/install/default/bin/shark" md $d --store=rsync:/rsync --rsync-mode=hardlink --verbose; then
                exit 1
            else
                echo "Successfully Failed"
            fi
        done

		sudo rm -rf /rsync
        ;;
    *)
        printf "Usage: main.sh [zfs|rsync_copy]" >&2
        exit 1
esac