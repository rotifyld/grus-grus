#!/bin/bash
cd ~/Code/Haskell/zpp/grus-grus/src/bnfc/ || exit
bnfc -haskell GrusGrus.cf
rm *.bak Makefile SkelGrusGrus.hs TestGrusGrus.hs