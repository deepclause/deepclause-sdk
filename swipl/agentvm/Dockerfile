# Example AgentVM / Alpine image for the plain SWI-Prolog runtime.
# Build from the repository root:
#   docker build -f swipl/agentvm/Dockerfile -t deepclause-swi .
FROM alpine:edge

RUN echo 'https://dl-cdn.alpinelinux.org/alpine/edge/testing' >> /etc/apk/repositories \
 && apk add --no-cache swi-prolog ca-certificates bash
WORKDIR /opt/deepclause
COPY swipl/prolog ./prolog
COPY swipl/bin ./bin
COPY swipl/scripts ./scripts
# dc_agent/dc_frame load the shared MI with ../../src/prolog-src.
COPY src/prolog-src /opt/src/prolog-src
RUN find prolog -name '*.qlf' -delete \
 && chmod +x bin/dc scripts/build-qlf.sh \
 && scripts/build-qlf.sh

ENV DC_SANDBOX=agentvm
WORKDIR /workspace
ENTRYPOINT ["/opt/deepclause/bin/dc"]
