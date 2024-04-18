# Shark Store

Shark manages data based on markdown files and code blocks. A majority
of the time this means running containers and wiring up previous outputs
into a container to perform some action. All of this data exists in the
Shark store: a ZFS pool.

The pool is an extension of the [Obuilder][] store. The most relevant part
of the store is:

```
/<zfs-pool>/result/<hash>/rootfs
```

This contains the container image and probably the valuable data a user created.
There are other files sitting alongside `rootfs` related to the build like the
environment variables and the user that executed the actions, as well as the log of
the build.

We also store data for a particular build in a `_shark` directory in there too. This
contains the partially or fully executed markdown file for example and other metadata
such as the inputs and outputs for this particular build.


[Obuilder]: https://github.com/quantifyearth/obuilder#shark
