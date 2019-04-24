#!/usr/bin/env python3
import gzip
import os
from subprocess import call, Popen, PIPE
from threading import Thread


def piper(inp, outp):
    for line in iter(inp.readline, b''):
        outp.write(line)
    outp.close()


def rngame(*args, _out):
    p = Popen(["tools/rngame"]+[str(x) for x in args], stdout=PIPE)
    Thread(target=piper, args=(p.stdout, gzip.open(_out, "wb"))).start()
    p.wait()


def generate_rngames(outdir, count, size, maxPrio, minOutDeg, maxOutDeg):
    for k in range(0, count):
        fn = "{}/rn-{}-{}-{}-{}-{}.pg.gz".format(outdir, size, maxPrio, minOutDeg, maxOutDeg, k)
        if not os.path.isfile(fn):
            print("Generating random game {} with parameters {}, {}, {}, {}...".format(k, size, maxPrio, minOutDeg, maxOutDeg))
            rngame(size, maxPrio, minOutDeg, maxOutDeg, "x", _out=fn)


def ensure_dir(file_path):
    directory = os.path.dirname(file_path)
    if not os.path.exists(directory):
        os.makedirs(directory)


if __name__ == "__main__":
    ensure_dir("inputs/random1/")
    generate_rngames("inputs/random1", 20, 1000, 1000, 1, 1000)
    generate_rngames("inputs/random1", 20, 2000, 2000, 1, 2000)
    #generate_rngames("input/random1", 20, 4000, 4000, 1, 4000)
    #generate_rngames("input/random1", 20, 7000, 7000, 1, 7000)
    ensure_dir("inputs/random2/")
    generate_rngames("inputs/random2", 20, 10000, 10000, 1, 2)
    generate_rngames("inputs/random2", 20, 20000, 20000, 1, 2)
    generate_rngames("inputs/random2", 20, 40000, 40000, 1, 2)
    generate_rngames("inputs/random2", 20, 70000, 70000, 1, 2)
    generate_rngames("inputs/random2", 20, 100000, 100000, 1, 2)
    generate_rngames("inputs/random2", 20, 200000, 200000, 1, 2)
    #generate_rngames("input/random2", 20, 400000, 400000, 1, 2)
    #generate_rngames("input/random2", 20, 700000, 700000, 1, 2)
    #generate_rngames("input/random2", 20, 1000000, 1000000, 1, 2)
    #generate_rngames("input/random2", 20, 2000000, 2000000, 1, 2)
    #generate_rngames("input/random2", 20, 4000000, 4000000, 1, 2)
    ensure_dir("inputs/random3/")
    for n in [2000,4000,6000,8000,10000,12000,14000,16000,18000,20000]:
        for d in [2,3,4,5]:
            generate_rngames("inputs/random3", 20, n, d, 1, n)
