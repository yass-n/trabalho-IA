#!/usr/bin/env bash

# testa se o script já foi executado e termina a execução caso se verdadeiro
test -f /etc/bootstraped && exit

# Proxy da UFU :/
# PROXY_UFU="http://proxy.ufu.br:3128/"
# export http_proxy=$PROXY_UFU

apt-get update

#
# GHC
#

apt-get install -y ghc

# Sinaliza que este script já foi executado
date > /etc/bootstraped