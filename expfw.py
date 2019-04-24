#!/usr/bin/env python3
import json
import os
import sys
from subprocess import Popen, TimeoutExpired
import time
import random
import itertools


def call(*popenargs, timeout=None, **kwargs):
    # print("calling {}".format(str(popenargs)))
    with Popen(*popenargs, **kwargs) as p:
        try:
            return p.wait(timeout=timeout)
        except TimeoutExpired:
            p.terminate()
            p.wait()
            raise


class Experiment(object):
    NOTDONE = 0
    DONE = 1
    TIMEOUT = 2
    ERROR = 3

    def __init__(self, name, call, group=None):
        self.name = name
        self.call = call
        self.group = group

    def __repr__(self):
        return self.name

    def parse_log(self, contents):
        """Parse the log file.
        Return None if not good, or a dict with the results otherwise.
        """
        raise NotImplementedError

    def get_text(self, res):
        """Given the result of parse_log, return a str representing the main result.
        """
        raise NotImplementedError

    def get_status(self, filename):
        """Obtain the status of the experiment.
        Return a pair:
        Experiment.DONE, dict
        Experiment.ERROR, dict
        Experiment.TIMEOUT, time
        Experiment.NOTDONE, None
        """
        if os.path.isfile(filename):
            try:
                with open(filename, 'r') as handle:
                    res = self.parse_log(handle.read())
                    if res is not None:
                        if 'error' in res:
                            return Experiment.ERROR, res
                        else:
                            return Experiment.DONE, res
            except UnicodeDecodeError:
                print("Unicode error in file "+filename+"!")
                raise

        timeout_filename = "{}.timeout".format(filename)
        if os.path.isfile(timeout_filename):
            try:
                with open(timeout_filename, 'r') as handle:
                    return Experiment.TIMEOUT, int(handle.read())
            except Exception:
                return Experiment.NOTDONE, None
        else:
            if os.path.isfile(filename):
                return Experiment.ERROR, {'error': 'unknown error'}
            else:
                return Experiment.NOTDONE, None

    def run_experiment(self, timeout, filename):
        # remove output and timeout files
        if os.path.isfile(filename):
            os.unlink(filename)
        timeout_filename = "{}.timeout".format(filename)
        if os.path.isfile(timeout_filename):
            os.unlink(timeout_filename)

        # report that we are running the experiment
        print("Performing {}... ".format(self.name), end='')
        sys.stdout.flush()

        try:
            with open(filename, 'w+') as out:
                call(self.call, stdout=out, stderr=out, timeout=timeout)
        except KeyboardInterrupt:
            # if CTRL-C was hit, move the file
            os.rename(filename, "{}.interrupted".format(filename))
            print("Experiment interrupted.")
            sys.exit()
        except OSError:
            print("OS Error, typically caused by a missing executable.")
            sys.exit()
        except TimeoutExpired:
            # timeout hit, write current timeout value to timeout file
            with open(timeout_filename, 'w') as handle:
                handle.write(str(timeout))
            print("timeout.")
            return Experiment.TIMEOUT, timeout
        else:
            # experiment finished, either report done or not done...
            status, value = self.get_status(filename)
            if status == Experiment.DONE:
                print("done; {}.".format(self.get_text(value)))
            elif status == Experiment.ERROR:
                print("\033[1;31merror: {}\033[m.".format(value['error']))
            else:
                print("not done.")
            return status, value

    def todo(self):
        return True


def flatten_iter(x):
    if not hasattr(x, '__iter__'):
        yield x
    else:
        exhausted = object()
        iterators = [iter(x)]
        while iterators:
            it = next(iterators[-1], exhausted)
            if it is exhausted:
                iterators.pop()
            elif hasattr(it, '__iter__'):
                iterators.append(iter(it))
            else:
                yield it


###
# We use a lazy ExperimentCollection because evaluating a
# collection to the individual experiments results in work
# like listing files in a directory that we sometimes don't
# want to do yet.
###


class ExperimentCollection(object):
    def __init__(self):
        self.lazy = []
        self.flat = []
        self.filter = None

    def __iadd__(self, other):
        self.lazy.append(other)
        return self

    def __iter__(self):
        if len(self.lazy) > 0:
            self.flat += flatten_iter(self.lazy)
            self.lazy = []
        return filter(self.filter, iter(self.flat))

    def __len__(self):
        if len(self.lazy) > 0:
            self.flat += flatten_iter(self.lazy)
            self.lazy = []
        if self.filter is None:
            return len(self.flat)
        else:
            return sum(1 for x in self.flat if self.filter(x))

    def setfilter(self, filter_function):
        self.filter = filter_function


class ExperimentEngine(object):
    def __init__(self, **kwargs):
        """Initialize a set of experiments.
        - logdir (default "logs")
        - cachefile (default "cache.json")
        - timeout (default 1200 seconds)
        """
        self.experiments = ExperimentCollection()
        self.logdir = kwargs.get('logdir', 'logs')
        self.timeout = int(kwargs.get('timeout', 1200))
        self.cachefile = kwargs.get('cachefile', 'cache.json')
        self.results = []

    def __iadd__(self, other):
        self.experiments += other
        return self

    def __iter__(self):
        return iter(self.experiments)

    def __len__(self):
        return len(self.experiments)

    def setfilter(self, filter_function):
        self.experiments.setfilter(filter_function)

    def initialize(self, iterations=None, verbose=True):
        # create directory for logs if not exists
        if not os.path.exists(self.logdir):
            os.makedirs(self.logdir)
        # check if every experiment has a unique name
        self.sanity_check()
        # get results from cache and logs
        self.fill_results(iterations=iterations, verbose=verbose)

    def sanity_check(self):
        names_set = set()
        count = 0
        for e in self:
            names_set.add(e.name)
            count += 1
        if len(names_set) != count:
            print("Sanity check failed!")
            names_list = [e.name for e in self]
            for name in names_set:
                if sum([1 for x in names_list if x == name]) != 1:
                    print("{} occurs multiple times!".format(name))
            exit(0)

    def extend_for_iteration(self, iteration):
        """Ensure the <results> array is large enough.
        """
        while len(self.results) <= iteration:
            self.results.append({})

    def get_logfile(self, experiment, iteration):
        return "{}/{}-{}".format(self.logdir, experiment.name, iteration)

    def get_status(self, experiment, iteration):
        """Get the status of the experiment.
        Returns from the cache unless the experiment timed out with a lower
        timeout than configured, because maybe there is an updated result.
        """
        # check first in the cache
        if experiment.name in self.results[iteration]:
            status, value = self.results[iteration][experiment.name]
            # return cache result IF the timeout is not lower than configured
            if status != Experiment.TIMEOUT or value >= self.timeout:
                return status, value
        # check the log file
        logfile = "{}/{}-{}".format(self.logdir, experiment.name, iteration)
        status, value = experiment.get_status(logfile)
        # update cache
        if status != Experiment.NOTDONE:
            self.results[iteration][experiment.name] = status, value
        # return result
        return status, value

    def print_status(self, experiment, iteration):
        """Get experiment status and print to stdout.
        Returns True if the status was DONE / TIMEOUT / ERROR, otherwise False.
        """
        status, value = self.get_status(experiment, iteration)
        if status == Experiment.DONE:
            print("{}: {}.".format(experiment.name, experiment.get_text(value)))
            return True
        elif status == Experiment.TIMEOUT:
            print("{}: timeout ({}).".format(experiment.name, value))
            return True
        elif status == Experiment.ERROR:
            print("{}: \033[1;31merror: {}\033[m.".format(experiment.name, value['error']))
            return True
        else:
            print("{}: not done.".format(experiment.name))
            return False

    def load_cache(self, verbose=True, clean=True):
        """Load results from the cache file.
        """
        # get from file
        if os.path.isfile(self.cachefile):
            with open(self.cachefile) as f:
                self.results = json.load(f)
            if clean:
                exp_names = {e.name for e in self}
                self.results = [{k:v for k,v in X.items() if k in exp_names} for X in self.results]
            if verbose:
                self.report_cache("Loaded")

    def save_cache(self, verbose=True):
        # first prune empty iterations
        while len(self.results) > 0 and len(self.results[-1]) == 0:
            self.results.pop()
        # just json dump it
        with open(self.cachefile, 'w') as f:
            json.dump(self.results, f)
            if verbose:
                self.report_cache("Stored")

    def report_cache(self, prefix):
        # count stuff
        count_done = 0
        count_to = 0
        count_err = 0
        for it in self.results:
            for status, value in it.values():
                if status == Experiment.DONE:
                    count_done += 1
                elif status == Experiment.TIMEOUT:
                    count_to += 1
                elif status == Experiment.ERROR:
                    count_err += 1
        print("{} {} results, {} timeouts, {} errors, {} iterations."
              .format(prefix, count_done, count_to, count_err, len(self.results)))

    def fill_results(self, iterations=None, verbose=True):
        """
        Get all results for <iterations> iterations (or ALL iterations if iterations == None)
        """
        self.sanity_check()
        try:
            self.load_cache(verbose=verbose)
        except Exception:
            print("Exception while loading cache, ignoring cache.")
            self.results = []

        for i in itertools.count():
            if iterations is not None and i >= iterations:
                return
            self.extend_for_iteration(i)
            for e in self:
                self.get_status(e, i)
            if len(self.results[i]) == 0:
                return

    def get_groups(self):
        return {x.group for x in self}

    def todo(self, by_group=True, iterations=1):
        """List all experiments/groups that we still need to run.
        """
        res = set()
        for e in self:
            ident = e.group if by_group else e.name
            if ident in res:
                continue
            if not e.todo():
                continue
            for i in itertools.count():
                if i >= iterations:
                    break
                status, value = self.get_status(e, i)
                if (status == Experiment.NOTDONE or
                        (status == Experiment.TIMEOUT and value < self.timeout)):
                    res.add(ident)
                    break
        return res

    def report(self, group=None, by_group=True, iterations=None):
        """Report the current status of the experiments.
        """
        # if group is set, limit to experiments in the group
        if group is not None:
            experiments = [e for e in self if e.group == group]
        else:
            experiments = list(self)
        # if by_group is set, order by group
        if by_group:
            experiments.sort(key=lambda e: e.group)
        # report until empty iteration
        for i in itertools.count():
            if iterations is not None and i >= iterations:
                return
            if len(self.results) <= i or len(self.results[i]) == 0:
                return
            for e in experiments:
                self.print_status(e, i)

    def clean(self, iterations=None):
        """Erase all logfiles of errors and clear the cache.
        """
        # if group is set, limit to experiments in the group
        experiments = list(self)
        # report until empty iteration
        for i in itertools.count():
            if iterations is not None and i >= iterations:
                break
            if len(self.results) <= i or len(self.results[i]) == 0:
                break
            for e in experiments:
                status, value = self.get_status(e, i)
                if status == Experiment.ERROR:
                    fname = self.get_logfile(e, i)
                    if os.path.isfile(fname):
                        print("removed: " + fname)
                        os.unlink(fname)
        if os.path.isfile(self.cachefile):
            print("removed: " + self.cachefile)
            os.unlink(self.cachefile)
            self.results = []

    def run(self, group=None, iterations=None):
        """Run experiments (possibly forever).
        """
        for iteration in itertools.count():
            if iterations is not None and iteration >= iterations:
                return
            self.extend_for_iteration(iteration)
            todo = self.get_groups() if group is None else [group]
            for g in random.sample(todo, k=len(todo)):
                # report that we are going to run a group
                print("Running experiments in group {}.".format(g))
                # run experiments in group <group> for iteration <iteration>
                exps = [e for e in self if e.group == g]
                for experiment in random.sample(exps, k=len(exps)):
                    # do not use the cache in this particular case
                    logfile = "{}/{}-{}".format(self.logdir, experiment.name, iteration)
                    status, value = experiment.get_status(logfile)
                    if (status == Experiment.DONE or status == Experiment.ERROR or
                            (status == Experiment.TIMEOUT and value >= self.timeout)):
                        continue
                    if not experiment.todo():
                        continue
                    # ok, really run the experiment and then sleep for 1 second
                    status, value = experiment.run_experiment(self.timeout, logfile)
                    self.results[iteration][experiment.name] = status, value
                    time.sleep(1)
            # report that we finished this iteration
            print("Iteration {} done.".format(iteration))
