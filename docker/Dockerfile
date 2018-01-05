FROM centos:centos7
ADD https://github.com/satori-com/mzbench/releases/download/0.5.1/mzbench-0.5.1-centos7.x86_64.rpm mzbench.rpm
# For some reason yum install for this docker image does not work
RUN rpm2cpio mzbench.rpm | cpio -idmv
RUN yum -y install epel-release && yum clean all
RUN yum -y install python-pip rsync && yum clean all
RUN pip install mzbench_api_client
ADD server.config /etc/mzbench/server.config
ADD mzbench_init /mzbench_init
EXPOSE 80
CMD ["/mzbench_init"]
