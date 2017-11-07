# Stackage builds based on GHC HEAD

**This is incomplete and experimental!!**

This project an effort to build Stackage with arbitrary GHC development versions and, in particular, with the development HEAD. Instead of trying to build all of Stackage, it removes those packages that have version conflicts before attempting the build. For context, see the blog post [Using Stackage for GHC regression testing](http://www.tweag.io/posts/2017-10-27-stackage-head.html).

## Components

* `build.mk` is the GHC build config used for building GHC inside the Docker build image.
* `build-snapshot.sh` builds an entire Stackage snapshot (including all packages that can be built) for a given GHC Git repo tag (passed as an argument; `master` if omitted).
* `container-build-snapshot.sh` is the script executed inside the Stackage build container.
* `Dockerfile` builds the Docker build image based on `snoyberg/stackage:nightly`.
* `PruneConstraints.hs` removes all packages from a build constraints file that can not be built due to missing dependencies. It get the build constraints file as its first argument and a file with all packages that need to be removed as its second argument. The new build constraints are written to stdout.
* `SkipTestBenchs.hs` adds the packages in the file passed as the second argument to the 'skipped-tests' and 'skipped-benchmarks' of the build constraints file given in the first argument.