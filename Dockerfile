

FROM debian:wheezy-backports


# Setup package manager
ARG DEBIAN_FRONTEND=noninteractive
ADD ./sources.list /etc/apt/sources.list
RUN apt-get update


# Install some tooling
RUN apt-get install -qqy \
	vim tree git dropbear wget

# Install fenfire prereq
RUN apt-get install -qqy \
	ghc6 libghc6-gtk-dev libraptor1-dev c2hs libghc6-harp-dev \
	libghc6-haxml-dev happy alex libghc6-network-dev \
	libghc-syb-dev

# libghc-hlist-dev
RUN apt-get install -qqy cabal-install
RUN cabal update && cabal install HList random


# Get fenfire
RUN mkdir -vp /opt
RUN git clone https://github.com/timthelion/fenfire.git /opt/fenfire


# Config, build and install
RUN cd /opt/fenfire && runhaskell Setup.hs configure --user --prefix ~
RUN cd /opt/fenfire && runhaskell Setup.hs build
RUN cd /opt/fenfire && runhaskell Setup.hs install

EXPOSE 22

RUN mkdir /root/.ssh

RUN cd /root && wget http://www.snee.com/rdf/elvisimp.rdf

COPY container-init.sh /init
CMD /init

