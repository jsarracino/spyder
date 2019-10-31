#!/usr/bin/env python3

import sys
import os, os.path
import platform
import shutil
import time
import re
import difflib
import pickle
from subprocess import call, check_output, STDOUT
from colorama import init, Fore, Back, Style

import json

# Globals
if platform.system() in ['Linux', 'Darwin']:
    TIMEOUT_CMD = 'gtimeout'                                     # Timeout command
    TIMEOUT = '10m'                                             # Timeout value (seconds)   
    SPYDER_CMD = ['/usr/bin/time', '-p', TIMEOUT_CMD, TIMEOUT, './Script.hs']                             # Command to call Spyder
    SKETCH_CMD = ['/usr/bin/time', '-p', TIMEOUT_CMD, TIMEOUT, './sketch']
    SPYDER_BENCH = ['./Script.hs', '-b', 'Spy']

JSON_FILE = 'results.json'
 
WC_CMD = "wc -l"
LOGFILE = 'results.log'                                         # Log file
SPY_LOGFILE = 'spy-results.log'                                         # Log file
SK_LOGFILE = 'sk-results.log'                                         # Log file
DUMPFILE = 'results'                                            # Result serialization file
CSV_FILE = 'result.csv'                                         # CSV-output file
LATEX_FILE = '../papers/spyder-oopsla19/results.tex'                      # Latex-output file
ORACLE_FILE = 'solutions'                                       # Solutions file
SPYDER_OPTS = ['-i']                              # Options to use for Spyder benchmarks
SKETCH_OPTS = ['']
SKETCH_SIZES = [3,10,50]
# SKETCH_SIZES = []
FNULL = open(os.devnull, 'w')                                   # Null file

class Benchmark:
    def __init__(self, name, description, edited=False, sktimeout=False):
        self.name = name                # Id
        self.description = description  # Description (in the table)
        self.edited = edited
        self.sktimeout = sktimeout

    def str(self):
        return self.name + ': ' + self.description

class BenchmarkGroup:
    def __init__(self, name, default_options, benchmarks):
        self.name = name
        self.default_options = default_options  # Command-line options to use for all benchmarks in this group when running in common context
        self.benchmarks = benchmarks            # List of benchmarks in this group

ALL_BENCHMARKS = [
    BenchmarkGroup("Numerical Programs", [], [
        # these all work, kinda. the times are wonky? and so are sizes?
        # Benchmark('Midpoint_2', '2 variable midpoint'),
        Benchmark('Midpoint_3', '3 variable midpoint'),
        Benchmark('Midpoint_3_dist', '3 variable distinct midpoint'),
        Benchmark('Midpoint_3_dist_1D', '3 variable distinct midpoint 1D array', sktimeout=True),
        # Benchmark('Bound_2', '2 bounded variables'),
        # Benchmark('Bound_3', '3 bounded variables'),
        # Benchmark('Bound_3_1D', '3 bounded variables'),

        # this doesn't work (type error in test program??)
        # Benchmark('Midpoint_3_dist_2D', '3 variable distinct midpoint 2D array'),
        
        ]),
    # BenchmarkGroup("Web Applications",  [], [
        # Benchmark('GoL1D', '1D GoL', edited=True),
        # Benchmark('GoL1D_Buttons', '1D GoL with buttons', edited=True),
        # Benchmark('Expenses','split costs', edited=True),
        # Benchmark('Expenses_1D','split costs 1D', edited=True, sktimeout=True),
        # Benchmark('Overview','overview example', edited=True, sktimeout=True),
        # type error in test program? 2d problem.
        # Benchmark('GoL2D', '2D GoL with buttons', edited=True),
        # these don't work yet
        # Benchmark('Expenses_1D_Sum','split costs 1D', edited=True, sktimeout=True),
        # Benchmark('Expenses_1D_Sum_Color','split costs 1D with color', edited=True, sktimeout=True),
        # ])
]

class SynthesisResult:
    def __init__(self, name, spyder_holes, spyder_patches, time, sktime, edited):
        self.name = name                        # Benchmark name
        self.spyder_holes = spyder_holes        # Number of holes
        self.spyder_patch_size = spyder_patches
        self.spyder_time = time                 # Synthesis time (seconds)
        self.sketch_time = sktime            # Sketch synthesis times (seconds)
        self.edited = edited

    def str(self):
        return self.name + ', ' + '{0:0.2f}'.format(self.spyder_time) # + ', '  + self.code_size + ', ' + self.spec_size + ', ' + self.measure_count

spy_cursor = 0
def run_benchmark(name, opts, default_opts, edited, runSK):
    '''Run benchmark name with command-line options opts (use default_opts with running the common context variant); record results in the results dictionary'''

    global spy_cursor
    with open(LOGFILE, 'a+') as logfile, open(SPY_LOGFILE, 'a+') as spy_log, open(SK_LOGFILE, 'a+') as sk_log:
      
      logfile.write(name + '\n')
      logfile.seek(0, os.SEEK_END)
      
      print("BENCH: running spyder on input file %s.spy" % name, file=spy_log, flush=True)
    #   print("current directory: %s" % )
      start = time.time()
      call(SPYDER_CMD + SPYDER_OPTS + ['/Users/john/spyder/test/bench/spy/benchs/' + name + '.spy'], stdout=spy_log, stderr=spy_log)
      if runSK:
        end = time.time()
        os.chdir('/Users/john/sketch-1.7.5/sketch-frontend')
      
        for size in SKETCH_SIZES:
            opts = list(SKETCH_OPTS)
            opts.append("--fe-no-output-print")
            opts.append("--bnd-unroll-amnt=" + str(size))
            print('/Users/john/spyder/test/bench/spy/benchs/' + name + str(size) + '.sk')
            call(SKETCH_CMD + ['--bnd-unroll-amnt=' + str(size)] + ['/Users/john/spyder/test/bench/spy/benchs/' + name + str(size) + '.sk'], stdout=sk_log, stderr=sk_log)
        os.chdir('/Users/john/spyder')
      
      

        print(["{0:0.2f}".format(end - start)])
      else:
        print("skipping sketch")
      if False: # Synthesis failed
          print(*[Back.BLACK + Fore.RED + Style.BRIGHT + 'FAIL' + Style.RESET_ALL])
          results[name] = SynthesisResult(name, (end - start), '-', '-', '-', '-', 0, False)
      else: # Synthesis succeeded: code metrics from the output and record synthesis time
        #   sp_lastLines = os.popen("tail -n 8 %s" % SPY_LOGFILE).read().split('\n')
        #   sk_lastLines = os.popen("tail -n 8 %s" % SK_LOGFILE).read().split('\n')
          
          spy_log.seek(spy_cursor)
          spy_contents = spy_log.read()
          spy_cursor = spy_log.tell()

          sk_log.seek(0)
          sk_contents = sk_log.read()
          if not runSK:
            sk_times = []
          else:
            sk_times = list(map(lambda x: x.split(' ')[-1][:-1], re.findall(r"real\s*\d+\.\d+\s*", sk_contents)))

          if len(sk_times) < 2:
              sketch_time = [-1,-1,-1]
          else:
              sketch_time = list(map(float, sk_times[-3:-1] + [sk_times[-1]]))

        #   print(sketch_time)

        #   spy_log.seek(0)
        #   spy_contents = spy_log.read()
          
          spyder_holes = len(re.findall(r"holes:",spy_contents))
          spyder_time = list(map(lambda x: x.split(' ')[-1][:-1], re.findall(r"real\s*\d+\.\d+\s*", spy_contents)))[-1]
          spyder_time = float(spyder_time)
          spyder_patches = list(map(lambda x: int(re.search(r"\d+", x)[0]), re.findall(r"size of fix: \s*\d+\s*", spy_contents)))
        #   print(spyder_patches)
          spyder_patch_total = sum(map(int, spyder_patches))
          results[name] = SynthesisResult(name, spyder_holes, spyder_patch_total, spyder_time, sketch_time, edited)

          print(*[Back.BLACK + Fore.GREEN + Style.BRIGHT + ' OK ' + Style.RESET_ALL])

    #   print()
      
def format_time(t):
    return '{0:0.2f}'.format(t)

def write_json():
    '''Generate JSON file from the results dictionary'''
    outputs = []
    for group in groups:
        for b in group.benchmarks:
            bench_result = results[b.name]
            output = {
                "name": b.name,
                "patch-size": bench_result.spyder_patch_size,
                "patch-distr": bench_result.spyder_holes,
                "spy-time": format_time(bench_result.spyder_time),
                "sk-times": [format_time(x) for x in bench_result.sketch_time],
                "edited": bench_result.edited
            }
            outputs.append(output)
    # print(outputs)
    with open(JSON_FILE, 'w') as outfile:
        json.dump(outputs, outfile)


def diffLines(l, r):
    wcL = int(os.popen("./clean.sh %s | %s" % (l, WC_CMD)).read().strip().split(' ')[0])
    wcR = int(os.popen("./clean.sh %s | %s" % (l, WC_CMD)).read().strip().split(' ')[0])

    return (wcL, wcR)


if __name__ == '__main__':
    init()
    
    # cl_opts = cmdline()
    
    # Check if there are serialized results
    if os.path.isfile(DUMPFILE):
        # results = pickle.load(open(DUMPFILE, 'rb'))
        results=dict()
    else:
        results = dict()

    # Delete old log file
    if os.path.isfile(LOGFILE):
      os.remove(LOGFILE)
    if os.path.isfile(SK_LOGFILE):
      os.remove(SK_LOGFILE)
    if os.path.isfile(SPY_LOGFILE):
      os.remove(SPY_LOGFILE)

    # Run experiments
    # groups = ALL_BENCHMARKS[:1] if cl_opts.small else ALL_BENCHMARKS
    groups = ALL_BENCHMARKS
        
    # fill_with_blanks()
    for group in groups:
        for b in group.benchmarks: 
            if b.name in results:
                print(*[b.str() + Back.BLACK + Fore.YELLOW + Style.BRIGHT + 'SKIPPED' + Style.RESET_ALL])
            else:
                print(*[b.str()])
                run_benchmark(b.name, [], group.default_options, b.edited, not b.sktimeout)
                os.remove(LOGFILE)
                with open(DUMPFILE, 'wb') as data_dump:
                    pickle.dump(results, data_dump)    
            
    # Generate CSV
    write_json()            
    # Generate Latex table
    # write_latex()

    # Compare with previous solutions and print the diff
    # if os.path.isfile(ORACLE_FILE) and (not cl_opts.small):
        # fromlines = open(ORACLE_FILE).readlines()
        # tolines = open(LOGFILE, 'U').readlines()
        # diff = difflib.unified_diff(fromlines, tolines, n=0)
        # print
        # sys.stdout.writelines(diff)
