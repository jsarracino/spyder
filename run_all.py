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

# Globals
if platform.system() in ['Linux', 'Darwin']:
	SPYDER_CMD = ['/usr/bin/time', '-p', './Script.hs']                             # Command to call Spyder
	SKETCH_CMD = ['/usr/bin/time', '-p', './sketch']
	TIMEOUT_CMD = 'timeout'                                     # Timeout command
	TIMEOUT = '120'                                             # Timeout value (seconds)

LOGFILE = 'results.log'                                         # Log file
SPY_LOGFILE = 'spy-results.log'                                         # Log file
SK_LOGFILE = 'sk-results.log'                                         # Log file
DUMPFILE = 'results'                                            # Result serialization file
CSV_FILE = 'result.csv'                                         # CSV-output file
LATEX_FILE = '../papers/spyder-popl19/results.tex'                      # Latex-output file
ORACLE_FILE = 'solutions'                                       # Solutions file
SPYDER_OPTS = ['-i']                              # Options to use for Spyder benchmarks
SKETCH_OPTS = ['']
SKETCH_SIZES = [3,10,50]
FNULL = open(os.devnull, 'w')                                   # Null file

class Benchmark:
	def __init__(self, name, description, components=''):
		self.name = name         		# Id
		self.description = description  # Description (in the table)
		self.components = components    # Description of components used (in the table)

	def str(self):
		return self.name + ': ' + self.description + ' ' + str(self.components)

class BenchmarkGroup:
	def __init__(self, name, default_options, benchmarks):
		self.name = name
		self.default_options = default_options  # Command-line options to use for all benchmarks in this group when running in common context
		self.benchmarks = benchmarks            # List of benchmarks in this group

ALL_BENCHMARKS = [
	BenchmarkGroup("Numerical Programs", [], [
		Benchmark('Midpoint_midPoint', 'midpoint','Mid'),
		Benchmark('Midpoint_notEq', 'distinct mid','Mid-NE'),
		Benchmark('Arrs1D_Eq', 'equal arrays','Eq-1D'),
		Benchmark('Arrs2D_Eq', 'equal arrays','Eq-2D')
		]),
	BenchmarkGroup("Web Applications",  [], [
		Benchmark('GoL1D_cells2colors', '1D GoL', 'cells2colors'),
		Benchmark('GoL1D_goodConfig', '1D GoL', 'goodConfig'),
		Benchmark('Expenses','split costs', 'splitting'),
		Benchmark('spreadsheet_date', 'overview', 'conversions, colors'),
		Benchmark('Spreadsheet-Full', 'overview', 'conversions, colors')
		])
]

class SynthesisResult:
	def __init__(self, name, source_size, inv_size, spyder_holes, time, sktime):
		self.name = name                        # Benchmark name
		self.spyder_source_size = source_size   # Size of Spyder source code
		self.spyder_invariant_size = inv_size   # Cumulative invariant size (in AST nodes)
		# self.spyder_lines = spyder_lines        # Number of lines inserted
		self.spyder_holes = spyder_holes        # Number of holes
		self.spyder_time = time                 # Synthesis time (seconds)
		self.sketch_time = sktime            # Sketch synthesis times (seconds)

	def str(self):
		return self.name + ', ' + '{0:0.2f}'.format(self.spyder_time) # + ', '  + self.code_size + ', ' + self.spec_size + ', ' + self.measure_count

spy_cursor = 0
def run_benchmark(name, opts, default_opts):
	'''Run benchmark name with command-line options opts (use default_opts with running the common context variant); record results in the results dictionary'''

	global spy_cursor
	with open(LOGFILE, 'a+') as logfile, open(SPY_LOGFILE, 'a+') as spy_log, open(SK_LOGFILE, 'a+') as sk_log:
	  logfile.write(name + '\n')
	  logfile.seek(0, os.SEEK_END)
	  # Run Spyder on the benchmark:
	  start = time.time()
	  print("BENCH: running spyder on input file %s.spy" % name)
	  call(SPYDER_CMD + SPYDER_OPTS + [benchs_fname + name + '.spy'], stdout=spy_log, stderr=spy_log)
	  end = time.time()
	  os.chdir(sketch_fname)
	  for size in SKETCH_SIZES:
		opts = list(SKETCH_OPTS)
		opts.append("--fe-no-output-print")
		opts.append("--bnd-unroll-amnt=" + str(size))
		print(benchs_fname + name + str(size) + '.sk')
		call(SKETCH_CMD + ['--bnd-unroll-amnt=' + str(size)] + [benchs_fname + name + '.sk'], stdout=sk_log, stderr=sk_log)
	  os.chdir(spyder_fname)

	  print(["{0:0.2f}".format(end - start)])
	  if False: # Synthesis failed
		  print([Back.RED + Fore.RED + Style.BRIGHT + 'FAIL' + Style.RESET_ALL])
		  results[name] = SynthesisResult(name, (end - start), '-', '-', '-', '-')
	  else: # Synthesis succeeded: code metrics from the output and record synthesis time
		  sp_lastLines = os.popen("tail -n 8 %s" % SPY_LOGFILE).read().split('\n')
		#   sk_lastLines = os.popen("tail -n 8 %s" % SK_LOGFILE).read().split('\n')

		  spy_log.seek(spy_cursor)
		  spy_contents = spy_log.read()
		  spy_lines = spy_contents.split('\n')
		  spy_cursor = spy_log.tell()

		  sk_log.seek(0)
		  sk_contents = sk_log.read().split('\n')

		  spyder_time = float(sp_lastLines[-3].split(' ')[-1])

		  spyder_source_size = spy_lines[0].split(' ')[-1]
		  spyder_invariant_size = spy_lines[1].split(' ')[-1]
		  spyder_holes = len(re.findall("holes: \d+.*$",spy_contents))
		  spy_log.seek(0)
		  sk_log.seek(0)
		  #sketch_time = [str(t) for (t,) in re.findall("(real).*",sk_log.read())]
		  sketch_time = []
		  for line in sk_log.read().split('\n'):
				line = line.split(' ')
				if (line[0] == "real"):
					sketch_time.append(float(line[-1]))
		  print(sketch_time)
		  results[name] = SynthesisResult(name, spyder_source_size, spyder_invariant_size, spyder_holes, spyder_time, sketch_time )

		  print([Back.GREEN + Fore.GREEN + Style.BRIGHT + 'OK' + Style.RESET_ALL])

	  print()
def format_time(t):
	if t < 0:
		return '-'
	else:
		return '{0:0.2f}'.format(t)

# def write_csv():
	# '''Generate CSV file from the results dictionary'''
	# with open(CSV_FILE, 'w') as outfile:
		# for group in groups:
			# for b in group.benchmarks:
				# outfile.write (b.name + ',')
				# result = results [b.name]
				# outfile.write (result.spec_size + ',')
				# outfile.write (result.code_size + ',')
				# outfile.write (format_time(result.time) + ',')
				# outfile.write ('\n')

def fill_with_blanks():
  for group in groups:
	  for b in group.benchmarks:
		  results [b.name] = SynthesisResult(b.name, '-', '-', '-', '-', 0.0)

def write_latex():
	'''Generate Latex table from the results dictionary'''
	total_count = 0

	with open(LATEX_FILE, 'w') as outfile:
		for group in groups:
			if group.benchmarks.__len__() > 1:
				outfile.write ('\multirow{')
				outfile.write (str(group.benchmarks.__len__()))
				outfile.write ('}{*}{\\parbox{2cm}{\center{')
				outfile.write (group.name)
				outfile.write ('}}}')
			else:
				outfile.write (' ' + group.name + ' ')

			for b in group.benchmarks:
				result = results [b.name]
				# print(result.sketch_time)
				# print(result.spyder_source_size)
				# print(result.spyder_invariant_size)
				# print(result.spyder_holes)
				# print(result.spyder_time)
				row = \
					' & ' + b.description +\
					' & ' + str(result.spyder_source_size) +\
					' & ' + str(result.spyder_invariant_size) + \
					' & ' + str(result.spyder_holes) + \
					' & ' + format_time(result.spyder_time) + ' \\\\'
					# ' & ' + format_time(result.sketch_time[0]) + \
					# ' & ' + format_time(result.sketch_time[1]) + \
					# ' & ' + format_time(result.sketch_time[2]) + ' \\\\'
				outfile.write (row)
				outfile.write ('\n')
				total_count = total_count + 1

			outfile.write ('\\hline')

	print(['Total:', total_count])

# def cmdline():
	# import argparse
	# a = argparse.ArgumentParser()
	# a.add_argument('--medium', action='store_true')
	# a.add_argument('--small', action='store_true')
	# return a.parse_args()

if __name__ == '__main__':
	spyder_fname = sys.argv[1]
	sketch_fname = sys.argv[3]
	benchs_fname = sys.argv[2]
	init()

	# cl_opts = cmdline()

	# Check if there are serialized results
	# if os.path.isfile(DUMPFILE):
	if False:
		results = pickle.load(open(DUMPFILE, 'r'))
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
				print([b.str() + Back.YELLOW + Fore.YELLOW + Style.BRIGHT + 'SKIPPED' + Style.RESET_ALL])
			else:
				print([b.str()])
				run_benchmark(b.name, [], group.default_options)
				os.remove(LOGFILE)
				# with open(DUMPFILE, 'w') as data_dump:
				#     pickle.dump(results, data_dump)
	# Generate CSV
	# write_csv()
	# Generate Latex table
	#write_latex()

	# Compare with previous solutions and print the diff
	# if os.path.isfile(ORACLE_FILE) and (not cl_opts.small):
		# fromlines = open(ORACLE_FILE).readlines()
		# tolines = open(LOGFILE, 'U').readlines()
		# diff = difflib.unified_diff(fromlines, tolines, n=0)
		# print
		# sys.stdout.writelines(diff)
