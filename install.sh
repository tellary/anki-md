#!/bin/bash

stack build
sudo cp $(stack path --local-install-root)/bin/ankiMd /usr/local/bin
