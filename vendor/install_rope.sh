#!/bin/sh
# http://www.enigmacurry.com/2009/01/21/autocompleteel-python-code-completion-in-emacs/

mkdir /tmp/rope && cd /tmp/rope
hg clone http://bitbucket.org/agr/rope
hg clone http://bitbucket.org/agr/ropemacs
hg clone http://bitbucket.org/agr/ropemode
sudo easy_install rope
ln -s ../ropemode/ropemode ropemacs/
sudo easy_install ropemacs
