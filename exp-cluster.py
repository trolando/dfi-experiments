#!/usr/bin/env python3
from expfw import ExperimentEngine, Experiment
from exp import OinkExperiments
import sys

ITERATIONS = 5
TIMEOUT = 1800

dirs = []
dirs += ["synt"]
# dirs += ["random2"]
dirs += ["modelchecking"]
dirs += ["equivchecking"]
# dirs += ["random1"]
# dirs += ["pgsolver"]
# dirs += ["mlsolver"]
# dirs += ["langincl"]


engine = ExperimentEngine(logdir="logs-cluster", cachefile="cache-cluster.json", timeout=TIMEOUT)
for d in dirs:
    exps = OinkExperiments("inputs/"+d, ["fpi-n","fpi","zlk","zlk-n","pp","pp-n","tl","tl-n","psi","psi-n","fpi-1","fpi-2","fpi-4","fpi-8","fpi-16"])
    for group in exps:
        for exp in group:
            exp.dataset = d
    engine += exps

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)


def usage():
    eprint("Valid calls:")
    eprint("exp-cluster.py todo           List all groups to do")
    eprint("exp-cluster.py report         Report all experiments")
    eprint("exp-cluster.py report <GROUP> Report all experiments in a group")
    eprint("exp-cluster.py run <GROUP>    Run a group")
    eprint("exp-cluster.py cache          Update the cache")
    eprint("exp-cluster.py csv            Write the CSV of the results to stdout")
    eprint("exp-cluster.py clean          Delete cache and delete error experiments")


def main():
    if len(sys.argv) > 1:
        if sys.argv[1] == 'todo':
            engine.initialize(ITERATIONS, False)
            for x in engine.todo(iterations=ITERATIONS):
                print(x)
        elif sys.argv[1] == 'report':
            engine.initialize(ITERATIONS, False)
            if len(sys.argv) > 2:
                engine.report(group=sys.argv[2], iterations=ITERATIONS)
            else:
                engine.report(iterations=ITERATIONS)
        elif sys.argv[1] == 'run' and len(sys.argv) == 2:
            # run the given group with given number of iterations
            engine.initialize(ITERATIONS, False)
            engine.run(iterations=ITERATIONS)
        elif sys.argv[1] == 'run' and len(sys.argv) > 2:
            # run the given group with given number of iterations
            engine.initialize(ITERATIONS, False)
            engine.run(group=sys.argv[2], iterations=ITERATIONS)
        elif sys.argv[1] == 'cache':
            engine.initialize(ITERATIONS, True)
            engine.save_cache(True)
            count_done = sum([len(x) for i, x in enumerate(engine.results) if i < ITERATIONS])
            count_to = sum([1 for i, x in enumerate(engine.results) for a,b in x.items() if b[0] == Experiment.TIMEOUT and b[1] < TIMEOUT])
            print("Remaining: {} experiments not done + {} experiments rerun for higher timeout.".format(ITERATIONS*len(engine)-count_done, count_to))
        elif sys.argv[1] == 'csv':
            engine.initialize(ITERATIONS, False)
            expmap = {e.name: e for e in engine}
            for i, it in enumerate(engine.results):
                if i > ITERATIONS:
                    break
                for ename, res in it.items():
                    e = expmap[ename]
                    status, value = res
                    if status == Experiment.DONE:
                        nodes = value.get("nodes", 0)
                        edges = value.get("edges", 0)
                        priorities = value.get("priorities", 0)
                        time = value['time']
                        solving = value.get("solving",value["time"])
                        metric = value.get("iterations", value.get("promotions", value.get("tangles", -1)))
                        print("{}; {}; {}; {:.6f}; 1; {}; {}; {}; {:.6f}; {}".format(e.group, e.dataset, e.solver, value['time'], nodes, edges, priorities, solving, metric))
                    elif status == Experiment.TIMEOUT:
                        nodes = edges = priorities = 0
                        print("{}; {}; {}; {:.6f}; 0; {}; {}; {}; 0; 0".format(e.group, e.dataset, e.solver, TIMEOUT, nodes, edges, priorities))
        elif sys.argv[1] == 'clean':
            engine.initialize(ITERATIONS, False)
            engine.clean(iterations=ITERATIONS)
        else:
            usage()
    else:
        usage()


if __name__ == "__main__":
    main()
