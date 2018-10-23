# Stackage builds based on GHC HEAD

[![CircleCI](https://circleci.com/gh/tweag/stackage-head/tree/master.svg?style=svg)](https://circleci.com/gh/tweag/stackage-head/tree/master)

This project is an effort to build Stackage with arbitrary GHC development
versions and, in particular, with the development HEAD. This allows us to
detect regressions during GHC development much faster.

## How it works

The process is currently run on Circle CI 4 times a day and can be described
as the following progression of steps:

1. Start with the docker container that is used to build Stackage Nightly,
   `snoyberg/stackage:nightly`.

2. To avoid re-compiling GHC every time we use build artifacts and some
   associated metadata provided by
   [`ghc-artifact-collector`](https://github.com/tweag/ghc-artifact-collector).

3. Download Stackage curator that is used to execute build plans.

4. Reuse a plan from the
   [`stackage-nightly`](https://github.com/fpco/stackage-nightly)
   repository. These plans are known to build fine, so they are OK for us in
   most cases, and even if a couple of packages cannot be built it's not a
   big deal and can be detected as usual (see below).

5. Update, if necessary, the downloaded plan setting source URLs in case we
   need to use not-yet-released versions of some packages.

6. Execute the chosen build plan and save the build log.

7. Parse the build log and turn it into a build report, store it for future
   runs.

8. Compare two most recent build reports and detect regressions. Fail if
   there are suspicious changes (which are necessarily due to some changes
   in GHC, because we build always with the same build plan, only changing
   GHC commits).

9. GHC team is notified if the build fails.

## Updating the snapshot

The Stackage snapshot used for the builds is updated manually.
The reason is that we don't want extra noise and volatility associated with
changing snapshots.

Updating the snapshot is usually as simple as editing `.circleci/config.yml` and
changing the line

``` yaml
  BUILD_PLAN: nightly-2018-10-23
```

to something else.

Occasinally, our docker image also needs to be rebuilt
on top of the latest `snoyberg/stackage:nightly` image—for instance,
if a newly added package needs an extra system dependency.
The corresponding `Dockerfile` is at `.circleci/images/primary/Dockerfile`.
The image then needs to be uploaded to Docker Hub and the following line updated
in  `.circleci/config.yml`:

``` yaml
  docker:
    - image: rctwg/stackage-head:0.3.2
```

You can see when `snoyberg/stackage:nightly` was last updated [here](https://hub.docker.com/r/snoyberg/stackage/tags/).

## Blog posts and talks

* [The original blog post introducing the initiative](https://www.tweag.io/posts/2017-10-27-stackage-head.html)
* [A talk from HIW'17](https://www.youtube.com/watch?v=UAI-mplXUkY)

## License

Copyright © 2018 Tweag I/O

Distributed under BSD 3 clause license.
