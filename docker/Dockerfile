FROM docker.io/library/openjdk:17-slim
MAINTAINER Casey Link <casey@outskirtslabs.com>

RUN set -ex; \
    export DEBIAN_FRONTEND=noninteractive; \
    apt-get update; \
    apt-get update; \
    apt-get install -y --no-install-recommends gosu ca-certificates; \
    /usr/sbin/update-ca-certificates \
    apt-get autoremove -y; \
    rm -rf /var/lib/apt/lists/*


RUN groupadd --gid 1000 app \
  && useradd --uid 1000 --gid app --shell /bin/bash --create-home --home-dir /app app

WORKDIR /app
COPY ./docker/scripts/entrypoint.sh /entrypoint.sh
RUN set -ex; chmod +rx /*.sh; chown -R app:app /app
STOPSIGNAL SIGTERM
ENTRYPOINT ["/entrypoint.sh"]

ARG GIT_HASH
COPY --chown=app:app ./target/sno/rewardmgr-${GIT_HASH}.jar /app/app.jar
# these are down at the bottom so changes to them don't cause a full rebuild
ARG DOCKER_IMAGE_TAG
ARG GIT_BRANCH
ARG BUILD_DATE
ENV DOCKER_IMAGE_TAG=${DOCKER_IMAGE_TAG}
ENV GIT_BRANCH ${GIT_BRANCH}
ENV GIT_HASH ${GIT_HASH}
ENV NREPL_PORT 7000
ENV BUILD_DATE ${BUILD_DATE}
