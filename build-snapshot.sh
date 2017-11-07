#!/usr/bin/env bash

set -eu +x

# Parse command line arguments
debug=0
repo_tag="master"
while [ $# -gt 0 ]; do
  case "$1" in
    (--debug) debug=1;;
    (--tag=*) repo_tag=${1:6};;
    (*)
      echo "usage: build-snapshot.sh [--debug] [--tag=GHC_REPO_TAG]"
      echo "  --debug preserves the build container"
      exit 1
      ;;
  esac
  shift
done

echo "========== BUILDING GHC:${repo_tag}"
docker build --tag ghc-head --build-arg REPO_TAG=${repo_tag} .

echo "========== BUILDING STACKAGE"
if [ ! -r build-constraints.yaml ]; then
  
  wget https://raw.githubusercontent.com/fpco/stackage/master/build-constraints.yaml
  
fi
CONTAINER_NAME=stackage
if [ ${debug} -eq 0 ]; then
  
  docker run --rm -t -v `pwd`:/opt/script:ro ghc-head /opt/script/container-build-snapshot.sh
  # FIXME: this is still useless as we don't extract any information yet
  
else
  
  # For debugging, we grab the stackage build out of the container, dump the Docker logs & preserve the container.
  docker rm ${CONTAINER_NAME} || true
  docker run --name ${CONTAINER_NAME} -v `pwd`:/opt/script:ro ghc-head /opt/script/container-build-snapshot.sh
  docker cp ${CONTAINER_NAME}:/opt/stackage stackage
  docker logs ${CONTAINER_NAME} >stackage/docker.log
  echo "Container name = ${CONTAINER_NAME}"

fi
