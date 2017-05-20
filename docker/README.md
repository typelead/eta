# Building ETA docker container in Circle CI
This folder contains everything necessary to automatically build and publish Eta/Etlas docker container in CI.

## Requirements
The following CircleCI env variables are expected to be set:

`DOCKER_EMAIL`, `DOCKER_USER`, `DOCKER_PASS` - these are used for `docker login` that is needed for `docker push`.

## Publishing rules
By default the target repository will be inferred from the GIT repository name, i.e. containers that are built from `https://github.com/typelead/eta` will be pushed as `typelead/eta:<tag>`.
This behavior can be changed by setting `DOCKER_REPO` env variable to override the default setting. For example, setting `DOCKER_REPO` to `quay.io/jdoe/myrepo` will result pushing to that docker repository.

## Results and tags
The contaner is pushed as:

- `<docker-repo>:<git-hash>` - is pushed always
- `<docker-repo>:<git-tag>` - is pushed if the commit that is being built is tagged
- `<docker-repo>:<git-branch>` - is pushed for branches other than `master` (see `circle.yaml`)
- `<docker-repo>:latest` - is pushed when `master` branch is built.

Note that the same container in the same build can have multiple docker tags. For example, when `master` branch is built, the container is pushed with two tags: `<docker-repo>:<git-hash>` and `<docker-repo>:latest`. And if the commit was tagged as well then it will also have `<docker-repo>:<git-tag>` tag.
See `circle.yaml` and `circleci-build-container.sh` for more details.
