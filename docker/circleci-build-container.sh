#!/bin/bash

if [ "$DOCKER_EMAIL" == "" ] || [ "$DOCKER_USER" == "" ] || [ "$DOCKER_PASS" == "" ]; then
  echo "DOCKER_EMAIL, DOCKER_USER and DOCKER_PASS variables are not set."
  echo "Will not build and push docker container."
  exit 0
fi

CURRENT_TAG=$(git describe --exact-match --tags $(git log -n1 --pretty='%h') 2> /dev/null || echo "")
CURRENT_VER=$(cat *.cabal | grep -e "^name" | tr -s " " | cut -d' ' -f2)
CURRENT_HASH=$(git rev-parse --short HEAD)

DOCKER_REPO=$(echo ${DOCKER_REPO:-"$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME"} | tr '[:upper:]' '[:lower:]')
echo "Docker repo: $DOCKER_REPO"

HASH_TAG="$DOCKER_REPO:$CURRENT_HASH"

EXTRA_TAG=$(echo ${2:-''} | tr '[:upper:]' '[:lower:]')

case $1 in
  build)
    set -e
    mkdir -p ./docker/bin/.local
    cp -r ~/.local/bin ./docker/bin/.local/
    cp -r ~/.eta ./docker/bin/
    cp -r ~/.etlas ./docker/bin/
    rm -rf ./docker/bin/.etlas/logs/*
    rm -rf ./docker/bin/.etlas/packages/*

    echo "Building an image: $HASH_TAG"
    docker build -t "$HASH_TAG" ./docker
    set +e
  ;;

  push)
    set -e
    echo "Logging into docker"
    docker login -e "$DOCKER_EMAIL" -u "$DOCKER_USER" -p '$(shell echo $$DOCKER_PASS)'

    echo "Push: $HASH_TAG"
    docker push "$HASH_TAG"

    if [ "$CURRENT_TAG" ]; then
      echo "Push: $DOCKER_REPO:$CURRENT_TAG"
      docker tag "$HASH_TAG" "$DOCKER_REPO:$CURRENT_TAG"
      docker push "$DOCKER_REPO:$CURRENT_TAG"
    fi
    if [ "$EXTRA_TAG" ]; then
        echo "Push: $DOCKER_REPO:$EXTRA_TAG"
        docker tag "$HASH_TAG" "$DOCKER_REPO:$EXTRA_TAG"
        docker push "$DOCKER_REPO:$EXTRA_TAG"
    fi
    set +e
  ;;
esac
