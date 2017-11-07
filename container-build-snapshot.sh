#!/usr/bin/env bash

# This script expects to either find 'nightly-ghc-reference-plan-*.yaml' or 'build-constraints.yaml' in '/opt/script'.
# If there is a build plan, that is used; otherwise, a new build plan is computed on the basis of the build constraints.

set -uo pipefail

mkdir -p /opt/stackage
cd /opt/stackage
echo "Working directory: `pwd`"
ghc --version

SCRIPTDIR=/opt/script
BINDIR=/opt/bin
export PATH=${BINDIR}:${PATH}

echo "========== GETTING STACKAGE CURATOR"
date
mkdir -p ${BINDIR}
(
cd ${BINDIR}
rm -f stackage-curator stackage-curator.bz2
wget https://s3.amazonaws.com/stackage-travis/stackage-curator/stackage-curator.bz2
bunzip2 stackage-curator.bz2
chmod +x stackage-curator
./stackage-curator --version
)

echo "========== GETTING LATEST STACK"
date
curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ${BINDIR} '*/stack'


invalid_packages() {
  local dependency_conflicts="$1"
  local invalid_packages_prefix="$2"
  local description="$3"
  
  # Extract all conflicting packages from stackage-curators stderr output
  echo -n "" >${invalid_packages_prefix}-unsorted.txt
  for pkg in `grep -e '^- \[ \]' ${dependency_conflicts} | cut -d ' ' -f4 | sed -e 's/-[0-9][0-9.]*//g'`; do
    stackage-curator list-revdeps --plan-file ${PLAN_FILE} --deep ${pkg}   >>${invalid_packages_prefix}-unsorted.txt
  done
  sort -u ${invalid_packages_prefix}-unsorted.txt >${invalid_packages_prefix}.txt
  echo "*** Packages that will be ${description}: `wc -l ${invalid_packages_prefix}.txt | cut -d ' ' -f1` packages"
  cat ${invalid_packages_prefix}.txt
  echo "*** End packages that will be ${description}"
}

echo "========== DETERMINING BUILD PLAN"
date
BUILD_CONSTRAINTS=build-constraints.yaml
ORIGINAL_BUILD_CONSTRAINTS=build-constraints.original.yaml
SKIPPING_BUILD_CONSTRAINTS=build-constraints.skipping.yaml
PLAN_PREFIX="nightly-ghc-reference-plan"
DAY=`date "+%Y-%m-%d"`
LABEL=${DAY}-`ghc --numeric-version`
PLAN_PATH="${SCRIPTDIR}/${PLAN_PREFIX}-*.yaml"
if [ -e "${PLAN_PATH}" ];
then

  cp ${PLAN_PATH} .
  PLAN_FILE=`basename ${PLAN_PATH}`
  echo "*** Using existing plan"

else

  sed -e 's/ghc-major-version: "8.2"/ghc-major-version: "8.3"/' "${SCRIPTDIR}/${BUILD_CONSTRAINTS}" >"${BUILD_CONSTRAINTS}"
  PLAN=${PLAN_PREFIX}-${LABEL}
  PLAN_FILE=${PLAN}.yaml
  echo "*** Computing new plan from build constraints"
  stack update
  stackage-curator create-plan --target nightly-${DAY} --plan-file ${PLAN_FILE} 

  stackage-curator check --plan-file ${PLAN_FILE} 2>dependency-conflicts.txt
  if [ $? -ne 0 ]; then

    invalid_packages "dependency-conflicts.txt" "original-invalid-packages" "skipped in tests and benchmarks"

    # Skip tests and benchmarks for all invalid packages ('stackage-curator list-revdeps' doesn't distinguish dependency
    # origins and we don't want to prune only because of tests or benchmarks)
    mv ${BUILD_CONSTRAINTS} ${ORIGINAL_BUILD_CONSTRAINTS}
    /opt/script/SkipTestsBenchs.hs ${ORIGINAL_BUILD_CONSTRAINTS} original-invalid-packages.txt >${BUILD_CONSTRAINTS}

    stackage-curator create-plan --target nightly-${DAY} --plan-file ${PLAN_FILE} 
  
    stackage-curator check --plan-file ${PLAN_FILE} 2>build-dependency-conflicts.txt
    invalid_packages "build-dependency-conflicts.txt" "invalid-packages" "removed from the build"

    # Prune the build constraints (removing all conflicting packages and their dependencies)
    mv ${BUILD_CONSTRAINTS} ${SKIPPING_BUILD_CONSTRAINTS}
    /opt/script/PruneConstraints.hs ${SKIPPING_BUILD_CONSTRAINTS} invalid-packages.txt >${BUILD_CONSTRAINTS}

    # Create a new plan without the conflicts
    stackage-curator create-plan --target nightly-${DAY} --plan-file ${PLAN_FILE} 
  fi

fi
echo "PLAN_FILE = ${PLAN_FILE}"
stackage-curator check --plan-file ${PLAN_FILE}
echo "*** Plan statistics"
stackage-curator stats --plan-file ${PLAN_FILE}

echo "========== BUILDING PACKAGES"
date
# FIXME: WE WANT TO EVENTUALLY ENABLE TESTS & BENCHES
DOCMAP_FILE=${PLAN_PREFIX}-${LABEL}-docmap.yaml
BUNDLE_FILE=${PLAN_PREFIX}-${LABEL}.bundle
LOG_FILE=${PLAN_PREFIX}-${LABEL}.log
stackage-curator make-bundle --jobs 4 --plan-file ${PLAN_FILE} --docmap-file ${DOCMAP_FILE} --bundle-file ${BUNDLE_FILE} \
  --target nightly-${DAY} --skip-tests --skip-benches --skip-haddock --skip-hoogle --no-rebuild-cabal -v \
  >${LOG_FILE} 2>&1

echo "========== FINISHED"
date
