#!/usr/bin/env python3

import json
import copy

# import 


class ProgSize:
  def __init__(self, spec, impl):
    self.spec = spec
    self.impl = impl

  def toLatex(self):
    return '%i & %i' % (self.impl, self.spec)

  @staticmethod
  def blank():
    return ProgSize(0,0)

class SynthInfo:
  def __init__(self, holes, size):
    self.holes = holes
    self.size = size

  def toLatex(self):
    return '%i & %i' % (self.holes, self.size)
  @staticmethod
  def blank():
    return SynthInfo(0,0)

class SKTime:
  def __init__(self, three, ten, fifty):
    self.three = three
    self.ten = ten
    self.fifty = fifty

  def toLatex(self):
    def toS(v):
      if v < 0:
        return '-'
      else:
        return '%.2f' % v
    return '%s & %s & %s' % (toS(self.three), toS(self.ten), toS(self.fifty))
  @staticmethod
  def blank():
    return SKTime(0.0,0.0,0.0)

class BenchGroup:
  def __init__(self, title, rows):
    self.title = title
    self.rows = rows

  def toLatex(self):
    return '\\multirow{%i}{*}{\\parbox{2cm}{\\center{%s}}}' % (len(self.rows), self.title) + ' \\\\\n'.join([x.toLatex() for x in self.rows]) + ' \\\\\n'

  @staticmethod
  def blank(title, names):
    return BenchGroup(title, [BenchRow.blank(x) for x in names])


class BenchRow:
  def __init__(self, name, imp, frp, spy, synthInfo, spyTime, skTime):
    self.name = name
    self.imp = imp
    self.frp = frp
    self.spy = spy
    self.synthInfo = synthInfo
    self.spyTime = spyTime
    self.skTime = skTime 

  @staticmethod
  def blank(name):
    return BenchRow(name, ProgSize.blank(), ProgSize.blank(), ProgSize.blank(), SynthInfo.blank(), 0.0, SKTime.blank())

  def toLatex(self):
    prefix = ' & %s & ' % self.name.replace('_','\\_')
    return prefix + ' & '.join([x.toLatex() for x in [self.imp, self.frp, self.spy, self.skTime]]) + ' & %.2f & ' % self.spyTime + self.synthInfo.toLatex()

  

benches = [ 
  BenchGroup.blank('Numerical Programs', [
    "Midpoint_2", "Midpoint_3", "Midpoint_3_dist", "Midpoint_3_dist_1D",# "Midpoint_3_dist_2D",
    "Bound_2", "Bound_3", "Bound_3_1D"
  ]),
  BenchGroup.blank('Web Applications', [
    "GoL1D", "GoL1D_Buttons", #"GoL2D",
    "Expenses", "Expenses_1D", # "Expenses_1D_Sum", "Expenses_1D_Sum_Color",
    "Overview"
  ])
  
]

def outputLatex():
  print('\\hline'.join([x.toLatex() for x in benches]))

# map benchmark name to {imp, frp, spyder} dict
def loadSizes(szPath):
  output = {}
  with open(szPath) as sizes:
    for size in json.load(sizes):
      if size["name"] not in output:
        output[size["name"]] = {}

      output[size["name"]][size["type"]] = ProgSize(size["spec"], size["impl"])

  return output

# map benchmark name to {sk, spy, synthInfo} dict
def loadResults(timingPath):
  output = {}
  with open(timingPath) as results:
    for result in json.load(results):
      if result["name"] not in output:
        output[result["name"]] = {}

      [three, ten, fifty] = [float(x) for x in result["sk-times"]]

      output[result["name"]]["sk"] = SKTime(three, ten, fifty)
      output[result["name"]]["spy"] = float(result["spy-time"])
      output[result["name"]]["synthInfo"] = SynthInfo(int(result["patch-distr"]), int(result["patch-size"]))

  return output


def addSizes(benches, sizes):
  for bench in benches:
    for row in bench.rows:
      # print(row.name)
      # print(sizes)
      row.imp = sizes[row.name]["imp"]
      row.spy = sizes[row.name]["spy"]
      row.frp = sizes[row.name]["frp"]

def addResults(benches, results):
  for bench in benches:
    for row in bench.rows:
      if (row.name not in results):
        continue
      row.synthInfo = results[row.name]["synthInfo"]
      row.spyTime = results[row.name]["spy"]
      row.skTime = results[row.name]["sk"]

# def outputLatex(benches):
#   for 

if __name__ == "__main__":
  sizes = loadSizes('../data/bench-sizes.json')
  addSizes(benches, sizes)
  results = loadResults('../results.json')
  addResults(benches, results)
  outputLatex()