#!/usr/bin/env python3

import json
import copy

def separateDiffs(dpath, outpath):
  with open(dpath) as dfile:
    djson = json.load(dfile)
  output = []

  baseline = makeBaseline(djson)
  for entry in djson:
    maint = copy.deepcopy(entry)
    eff = copy.deepcopy(maint)


    maint["diff-type"] = "maintenance"
    eff["diff-type"] = "feature"

    maint["effort"] = entry["diff"]["total"] - baseline[entry["post"]]
    eff["effort"] = baseline[entry["post"]]
    
    output.append(maint)
    output.append(eff)

  # normalize(output)
  with open(outpath, 'w') as ofile:
    json.dump(output, ofile)
    

def normalize(diffs):
  totals = {}
  for datum in diffs:
    if datum["post"] in totals and datum["diff-type"] == "maintenance":
      totals[datum["post"]] += datum["effort"]
    else:
      totals[datum["post"]] = datum["effort"]

  for datum in diffs:
    if totals[datum["post"]] > 0:
      datum["effort"] = datum["effort"]/(totals[datum["post"]])
    else:
      datum["effort"] = 0.0

  


def makeBaseline(diffs):
  output = {}

  for datum in diffs:
    if datum["type"] != 'imp':
      output[datum["post"]] = datum["diff"]["impl"]
  
  return output


if __name__ == "__main__":
  separateDiffs('../data/bench-diffs.json', 'diffs.json')