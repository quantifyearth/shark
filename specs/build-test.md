# Testing an git checkout and build

This spec is a test of using a git repository import in shark to pull an data to a shark environment:

```shark-import
https://github.com/quantifyearth/littlejohn.git /data/littlejohn
```

This project happens to be in golang, so we'll need a Go build environment:

```shark-build:golang
((from "golang:latest"))
```

We can then do the build and copy the result to a shark friendly location:

```shark-run:golang
$ cd /data/littlejohn
$ mkdir /data/build/
$ GOPATH=/data/build go install
$ ls -laR /data/build
```

And then publish the results

```shark-publish
/data/build/bin/littlejohn
```
