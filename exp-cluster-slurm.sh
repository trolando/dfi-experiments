#!/bin/bash

for g in `./exp-cluster.py todo`
do
    # echo $g
    srun -N1 -n1 -c16 -o job%J.out --exclusive ./exp-cluster.py run "$g" &
done

wait
