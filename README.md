# Stackage builds based on GHC HEAD

This project is an effort to build Stackage with arbitrary GHC development
versions and, in particular, with the development HEAD. This allows us to
detect regressions during GHC development much faster. The original blog
post introducing the initiative can be found here:
https://www.tweag.io/posts/2017-10-27-stackage-head.html. In addition to
that there is a talk from HIW'17 here:
https://www.youtube.com/watch?v=UAI-mplXUkY.

The process is currently run on Circle CI twice a day and can be described
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

5. Execute the chosen build plan and save build log.

6. Parse the build log and turn it into a build report, store it for next
   runs.

7. Compare two most recent build reports and detect regressions. Fail if
   there are suspicious changes (which are necessarily due to some changes
   in GHC, because we build always with the same build plan, only changing
   GHC commits).

8. GHC team is notified if the build fails.

## License

Copyright © 2018 Tweag I/O

Distributed under BSD 3 clause license.
