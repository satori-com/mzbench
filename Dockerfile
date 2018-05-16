FROM erlang:20.3.2-alpine

# install packages
RUN apk update && apk add --no-cache \
    bash \
    bc \
    g++ \
    git \
    make \
    musl-dev \
    net-tools \
    openssh openssh-server \
    openssl \
    py2-pip \
    rsync \
    wget \
    zlib-dev \
    ;

# Download kubectl
ARG KUBECTL_VERSION=1.9.6
ARG KUBECTL_CHECKSUM=e6aa618d682d0bcc90602cbf2e22a134d36af7199d7081015042eca02ba36368
RUN wget -O /usr/bin/kubectl https://storage.googleapis.com/kubernetes-release/release/v${KUBECTL_VERSION}/bin/linux/amd64/kubectl && \
    echo "${KUBECTL_CHECKSUM}  /usr/bin/kubectl" | sha256sum -c - && \
    chmod +x /usr/bin/kubectl

WORKDIR /opt/mzbench
COPY . .
# Clean sources just in case you build smth in the same folder
RUN  make -C /opt/mzbench/server clean \
 && make -C /opt/mzbench/node clean \
 && find . -name "*.so" -or -name "*.o" | xargs -I{} rm {}

# Compile and configure server
RUN pip install -r requirements.txt \
 && make -C /opt/mzbench/server generate \
 && mkdir -p /etc/mzbench /root/.ssh \
 && echo "[{mzbench_api, [{network_interface, \"0.0.0.0\"},{listen_port, 80}]}]." > /etc/mzbench/server.config \
 && ssh-keygen -A \
 && cp /etc/ssh/ssh_host_rsa_key /root/.ssh/id_rsa \
 && cat /etc/ssh/ssh_host_rsa_key.pub >> /root/.ssh/authorized_keys \
 && chmod 0600 /root/.ssh/authorized_keys


# Compile and install worker into the /root/.local/share
RUN make -C /opt/mzbench/node install


# env variables can be changed at runtime using `docker run --env ...`
ENV ORIG_PATH=$PATH
ENV CODE_LOADING_MODE=interactive
# config can be added via runtime env
# docker run --env MZBENCH_CONFIG_FILE=<path>
CMD /opt/mzbench/server/_build/default/rel/mzbench_api/bin/mzbench_api foreground
