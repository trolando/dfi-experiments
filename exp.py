#!/usr/bin/env python3
import os
import sys
import re
from itertools import chain

# import framework
from expfw import Experiment, ExperimentEngine


OINK = "tools/oink"


###
# We have some classes implementing Experiment:
# - <parse_log> to parse a log file into a result dictionary (or None)
# - <get_text> to obtain a textual description from a result dictionary
###


class ExpOink(Experiment):
    def parse_log(self, contents):
        s = re.search(r'solution verified', contents)
        if not s:
            return None
        res = {}
        s = re.search(r'solving took ([\d\.,]+)', contents)
        if s:
            res['solving'] = float(s.group(1))
        else:
            res['solving'] = float(0)
        s = re.search(r'preprocessing took ([\d\.,]+)', contents)
        if s:
            res['preprocessing'] = float(s.group(1))
        else:
            res['preprocessing'] = float(0)
        s = re.search(r'total solving time: ([\d\.,]+)', contents)
        if s:
            res['time'] = float(s.group(1))
        s = re.search(r'with ([\d]+) nodes and ([\d]+) edges', contents)
        if s:
            res['nodes'] = int(s.group(1))
            res['edges'] = int(s.group(2))
        else:
            res['nodes'] = res['edges'] = 0
        s = re.search(r'([\d]+) priorities', contents)
        if s:
            res['priorities'] = int(s.group(1))
        else:
            res['priorities'] = 0
        s = re.search(r'solved with ([\d\.,]+) major iterations, ([\d\.,]+) minor iterations', contents)
        if s:
            res['iterations'] = int(s.group(1)+s.group(2)) # major + minor
        s = re.search(r'solved with ([\d\.,]+) iterations', contents)
        if s:
            res['iterations'] = int(s.group(1))
        s = re.search(r'solved with ([\d\.,]+) promotions', contents)
        if s:
            res['promotions'] = int(s.group(1))
        s = re.search(r'solved with ([\d\.,]+) tangles and ([\d\.,]+) iterations', contents)
        if s:
            res['tangles'] = int(s.group(2))
            res['iterations'] = int(s.group(2))
        return res

    def get_text(self, res):
        if 'error' in res:
            return res['error']
        return "{:0.6f} sec".format(res['time'])

    def compress(self):
        self.name = "{}-c".format(self.name)
        self.solver = "{}-c".format(self.solver)
        self.call += ["--compress"]
        return self

    def inflate(self):
        self.name = "{}-i".format(self.name)
        self.solver = "{}-i".format(self.solver)
        self.call += ["--inflate"]
        return self

    def scc(self):
        self.name = "{}-s".format(self.name)
        self.solver = "{}-s".format(self.solver)
        self.call += ["--scc"]
        return self

    def nosp(self):
        self.name = "{}-n".format(self.name)
        self.solver = "{}-n".format(self.solver)
        self.call += ["--no-loops", "--no-single", "--no-wcwc"]
        return self


class ExpOinkFPI(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "fpi"
        self.name = "{}-fpi".format(name)
        self.call = ["tools/oink", "--fpi", "-w", "-1", "-v", model]
        self.model = model


class ExpOinkFPI1(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "fpi-1"
        self.name = "{}-fpi-1".format(name)
        self.call = ["tools/oink", "--fpi", "-w", "1", "-v", model]
        self.model = model


class ExpOinkFPI2(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "fpi-2"
        self.name = "{}-fpi-2".format(name)
        self.call = ["tools/oink", "--fpi", "-w", "2", "-v", model]
        self.model = model


class ExpOinkFPI4(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "fpi-4"
        self.name = "{}-fpi-4".format(name)
        self.call = ["tools/oink", "--fpi", "-w", "4", "-v", model]
        self.model = model


class ExpOinkFPI8(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "fpi-8"
        self.name = "{}-fpi-8".format(name)
        self.call = ["tools/oink", "--fpi", "-w", "8", "-v", model]
        self.model = model


class ExpOinkFPI16(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "fpi-16"
        self.name = "{}-fpi-16".format(name)
        self.call = ["tools/oink", "--fpi", "-w", "16", "-v", model]
        self.model = model


class ExpOinkTL(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "tl"
        self.name = "{}-tl".format(name)
        self.call = ["tools/oink", "--tl", "-v", model]
        self.model = model

    def parse_log(self, contents):
        res = super(ExpOinkTL, self).parse_log(contents)
        if res is not None:
            s = re.search(r'solved with ([\d]+) tangles', contents)
            if s:
                res['tangles'] = int(s.group(1))
        return res


class ExpOinkATL(ExpOinkTL):
    def __init__(self, name, model):
        self.group = name
        self.solver = "atl"
        self.name = "{}-atl".format(name)
        self.call = ["tools/oink", "--atl", "-v", model]
        self.model = model


class ExpOinkOTFTL(ExpOinkTL):
    def __init__(self, name, model):
        self.group = name
        self.solver = "otftl"
        self.name = "{}-otftl".format(name)
        self.call = ["tools/oink", "--otftl", "-v", model]
        self.model = model


class ExpOinkOTFATL(ExpOinkTL):
    def __init__(self, name, model):
        self.group = name
        self.solver = "otfatl"
        self.name = "{}-otfatl".format(name)
        self.call = ["tools/oink", "--otfatl", "-v", model]
        self.model = model


class ExpOinkRRTL(ExpOinkTL):
    def __init__(self, name, model):
        self.group = name
        self.solver = "rrtl"
        self.name = "{}-rrtl".format(name)
        self.call = ["tools/oink", "--rrtl", "-v", model]
        self.model = model


class ExpOinkORRTL(ExpOinkTL):
    def __init__(self, name, model):
        self.group = name
        self.solver = "orrtl"
        self.name = "{}-orrtl".format(name)
        self.call = ["tools/oink", "--orrtl", "-v", model]
        self.model = model


class ExpOinkSSPM(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "sspm"
        self.name = "{}-sspm".format(name)
        self.call = ["tools/oink", "--sspm", "-v", model]
        self.model = model

    def parse_log(self, contents):
        res = super(ExpOinkSSPM, self).parse_log(contents)
        if res is not None:
            s = re.search(r'solved with ([\d]+) lifts, ([\d]+) lift attempts, max l ([\d]+).', contents)
            if s:
                res['lifts'] = int(s.group(1))
                res['attempts'] = int(s.group(2))
                res['bound'] = int(s.group(3))
        return res


class ExpOinkBSSPM(ExpOinkSSPM):
    def __init__(self, name, model):
        self.group = name
        self.solver = "bsspm"
        self.name = "{}-bsspm".format(name)
        self.call = ["tools/oink", "--bsspm", "-v", model]
        self.model = model


class ExpOinkNPP(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "npp"
        self.name = "{}-npp".format(name)
        self.call = ["tools/oink", "--npp", "-v", model]
        self.model = model


class ExpOinkPP(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "pp"
        self.name = "{}-pp".format(name)
        self.call = ["tools/oink", "--pp", "-v", model]
        self.model = model


class ExpOinkPPP(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "ppp"
        self.name = "{}-ppp".format(name)
        self.call = ["tools/oink", "--ppp", "-v", model]
        self.model = model


class ExpOinkRR(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "rr"
        self.name = "{}-rr".format(name)
        self.call = ["tools/oink", "--rr", "-v", model]
        self.model = model


class ExpOinkDP(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "dp"
        self.name = "{}-dp".format(name)
        self.call = ["tools/oink", "--dp", "-v", model]
        self.model = model


class ExpOinkRRDP(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "rrdp"
        self.name = "{}-rrdp".format(name)
        self.call = ["tools/oink", "--rrdp", "-v", model]
        self.model = model


class ExpOinkZLK(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "zlk"
        self.name = "{}-zlk".format(name)
        self.call = ["tools/oink", "--zlk", "-w", "-1", "-v", model]
        self.model = model


class ExpOinkZLK1(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "zlk-1"
        self.name = "{}-zlk-1".format(name)
        self.call = ["tools/oink", "--zlk", "-w", "1", "-v", model]
        self.model = model


class ExpOinkZLK2(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "zlk-2"
        self.name = "{}-zlk-2".format(name)
        self.call = ["tools/oink", "--zlk", "-w", "2", "-v", model]
        self.model = model


class ExpOinkZLK8(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "zlk-8"
        self.name = "{}-zlk-8".format(name)
        self.call = ["tools/oink", "--zlk", "-w", "8", "-v", model]
        self.model = model


class ExpOinkPSI(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "psi"
        self.name = "{}-psi".format(name)
        self.call = ["tools/oink", "--psi", "-w", "-1", "-v", model]
        self.model = model


class ExpOinkPSI1(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "psi-1"
        self.name = "{}-psi-1".format(name)
        self.call = ["tools/oink", "--psi", "-w", "1", "-v", model]
        self.model = model


class ExpOinkPSI8(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "psi-8"
        self.name = "{}-psi-8".format(name)
        self.call = ["tools/oink", "--psi", "-w", "8", "-v", model]
        self.model = model


class ExpOinkSPM(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "spm"
        self.name = "{}-spm".format(name)
        self.call = ["tools/oink", "--spm", "-v", model]
        self.model = model


class ExpOinkTSPM(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "tspm"
        self.name = "{}-tspm".format(name)
        self.call = ["tools/oink", "--tspm", "-v", model]
        self.model = model


class ExpOinkQPT(ExpOink):
    def __init__(self, name, model):
        self.group = name
        self.solver = "qpt"
        self.name = "{}-qpt".format(name)
        self.call = ["tools/oink", "--qpt", "-v", model]
        self.model = model

    def parse_log(self, contents):
        res = super(ExpOinkQPT, self).parse_log(contents)
        if res is not None:
            s = re.search(r'solved with ([\d]+) lifts, ([\d]+) lift attempts, max k ([\d]+).', contents)
            if s:
                res['lifts'] = int(s.group(1))
                res['attempts'] = int(s.group(2))
                res['bound'] = int(s.group(3))
        return res


###
# Now that we have defined our experiments, we define the collections
###


class FileFinder(object):
    def __init__(self, directory, extensions):
        self.directory = directory
        self.extensions = extensions

    def __iter__(self):
        if not hasattr(self, 'files'):
            self.files = []
            for ext in self.extensions:
                dotext = "." + ext
                # get all files in directory ending with the extension
                files = [f[:-len(dotext)] for f in filter(lambda f: f.endswith(dotext) and os.path.isfile(self.directory+"/"+f), os.listdir(self.directory))]
                self.files.extend([(x, "{}/{}{}".format(self.directory, x, dotext)) for x in files])
        return self.files.__iter__()


class OinkExperiments(object):
    def __init__(self, directory, solvers=[]):
        self.files = FileFinder(directory, ["pg","pg.bz2","pg.gz","gm","gm.bz2","gm.gz"])
        self.solvers = solvers

    def get_solvers(self):
        return {
            'fpi': lambda name, filename: ExpOinkFPI(name, filename),
            'fpi-1': lambda name, filename: ExpOinkFPI1(name, filename),
            'fpi-2': lambda name, filename: ExpOinkFPI2(name, filename),
            'fpi-4': lambda name, filename: ExpOinkFPI4(name, filename),
            'fpi-8': lambda name, filename: ExpOinkFPI8(name, filename),
            'fpi-16': lambda name, filename: ExpOinkFPI16(name, filename),
            'tl': lambda name, filename: ExpOinkTL(name, filename),
            'atl': lambda name, filename: ExpOinkATL(name, filename),
            'otftl': lambda name, filename: ExpOinkOTFTL(name, filename),
            'otfatl': lambda name, filename: ExpOinkOTFATL(name, filename),
            'pp': lambda name, filename: ExpOinkPP(name, filename),
            'rr': lambda name, filename: ExpOinkRR(name, filename),
            'dp': lambda name, filename: ExpOinkDP(name, filename),
            'zlk': lambda name, filename: ExpOinkZLK(name, filename),
            'psi': lambda name, filename: ExpOinkPSI(name, filename),
            'fpi-n': lambda name, filename: ExpOinkFPI(name, filename).nosp(),
            'tl-n': lambda name, filename: ExpOinkTL(name, filename).nosp(),
            'pp-n': lambda name, filename: ExpOinkPP(name, filename).nosp(),
            'dp-n': lambda name, filename: ExpOinkDP(name, filename).nosp(),
            'zlk-n': lambda name, filename: ExpOinkZLK(name, filename).nosp(),
            'psi-n': lambda name, filename: ExpOinkPSI(name, filename).nosp(),
            }

    def __iter__(self):
        if not hasattr(self, 'grouped'):
            # define
            slvrs = self.get_solvers()
            if len(self.solvers) != 0:
                slvrs = {k:v for k,v in slvrs.items() if k in self.solvers}
            for slvr in slvrs:
                setattr(self, slvr, {})
            self.grouped = {}
            for name, filename in self.files:
                self.grouped[name] = []
                for slvr, fn in slvrs.items():
                    exp = fn(name, filename)
                    getattr(self, slvr)[name] = exp
                    self.grouped[name].append(exp)
        return self.grouped.values().__iter__()
