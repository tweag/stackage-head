#!/bin/sh
#
# Generate a dummy site
#

BUILD_REPORTS_DIR=test-build-reports
SITE_DIR=test-site

rm -rv $BUILD_REPORTS_DIR $SITE_DIR

stack exec stackage-head -- add --build-url https://circleci.com --ghc-metadata data/metadata-0.json --build-log data/example-log-0.txt --build-per-package-dir data/per-package-logs --target nightly-2018-06-14 --outdir $BUILD_REPORTS_DIR
stack exec stackage-head -- add --build-url https://circleci.com --ghc-metadata data/metadata-1.json --build-log data/example-log-1.txt --build-per-package-dir data/per-package-logs --target nightly-2018-06-14 --outdir $BUILD_REPORTS_DIR
stack exec stackage-head -- add --build-url https://circleci.com --ghc-metadata data/metadata-2.json --build-log data/example-log-2.txt --build-per-package-dir data/per-package-logs --target nightly-2018-06-14 --outdir $BUILD_REPORTS_DIR

stack exec stackage-head -- generate-site --site-dir $SITE_DIR --outdir $BUILD_REPORTS_DIR
