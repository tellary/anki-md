#!/bin/bash

stack build
sudo cabal install --prefix /usr/local
