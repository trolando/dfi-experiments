#!/bin/sh

# Model checking and Equivalence checking generated with mCRL2

if [ ! -f sources/modelchecking.zip ]; then
    wget http://www.open.ou.nl/jke/games/modelchecking.zip -P sources/
fi
mkdir -p inputs/modelchecking && unzip -nj sources/modelchecking.zip "modelchecking/games/*" -d inputs/modelchecking

if [ ! -f sources/equivchecking.zip ]; then
    wget http://www.open.ou.nl/jke/games/equivchecking.zip -P sources/
fi
mkdir -p inputs/equivchecking && unzip -nj sources/equivchecking.zip "equivchecking/games/*" -d inputs/equivchecking

# Some games generated with PGSolver (parity game solver)

if [ ! -f sources/pgsolver.zip ]; then
    wget http://www.open.ou.nl/jke/games/pgsolver.zip -P sources/
fi
mkdir -p inputs/pgsolver && unzip -nj sources/pgsolver.zip "pgsolver/games/*" -d inputs/pgsolver

# Some games generated with MLSolver (modal logic solver)

if [ ! -f sources/mlsolver.zip ]; then
    wget http://www.open.ou.nl/jke/games/mlsolver.zip -P sources/
fi
mkdir -p inputs/mlsolver && unzip -nj sources/mlsolver.zip "mlsolver/games/*" -d inputs/mlsolver

# Other language inclusion benchmarks

if [ ! -f sources/bench-li.zip ]; then 
    wget https://www7.in.tum.de/tools/gpupg/bench-li.zip -P sources/
fi
mkdir -p inputs/langincl && unzip -nj sources/bench-li.zip -d inputs/langincl

# And Strix games

mkdir -p inputs/synt && tar jxfv sources/strix_syntcomp_pg_collection.tar.bz2 -C inputs/synt --strip=1
