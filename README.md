Distraction Fixpoint Iteration experiments
==========================================

This repository hosts the experimental scripts for the new fixpoint algorithm to solve parity games that is based on distractions.

You can contact the main author of this work at <t.vandijk@utwente.nl>

Information on the experiments is found in the submitted paper.

Compiling the sources
-----
- Compile Oink from https://www.github.com/trolando/oink using CMake
- Use "Release" setting and enable compiling extra tools.
- Copy "rngame" and "oink" from Oink's build directory to the tools/ directory

Generating the input files
-----
Use `download.sh` to download and extract the benchmarks by Keiren and others.
Use `generate.py` to generate random benchmarks if you want that, but we did not actually use the random benchmarks in the paper as they are not representative of random benchmarks.

Running the experiments
-----
Ensure you have run `download.sh`.

The experiments were run on a cluster using slurm.
You could use a command line like `sbatch --exclusive -N... -p... exp-cluster-slurm.sh`.
The main script is `exp-cluster.py` which uses `expfw.py` and `exp.py` for the framework and for the experiment definitions. 

Use `exp-cluster.py run` to simply run all experiments, one by one.
Use `exp-cluster.py list` or `exp-cluster.py todo` in combination with `exp-cluster.py run <>groupname` to run an experiment group.
To get the current results, use `exp-cluster.py report` or `exp-cluster.py report <groupname>`.
The framework uses a JSON cache to store results which significantly speeds up the process.
Use `exp-cluster.py cache` to update the cache and then `exp-cluster.py csv` to write all results to stdout in CSV format.

Experimental results
-----

All log files are stored in `logs-cluster.tar.bz2`.
From these files, the `cache-cluster.json` is the cache of the script.
The results of experiments are then stored in `results.csv`.

Analysing the results
-----

Use `analyse.r`; the file `results.csv` must be present.
This file generates some information that is used in the paper.
Furthermore, a number of png and tex files are created that could have been used for graphs in the paper, but there was no space left.
./exp.py run


