#!/usr/bin/env python3

import json
import os
import copy
import subprocess

def compareSizes(infile, outfile):
  with open(infile) as inf:
    benches = json.load(inf)
  exts = ["imp", "frp", "spy"]

  outbenches = []



  for bench in benches:

    print("measuring ast diffs for " + bench["pre"])
    
    for ext in exts:
      newbench = copy.deepcopy(bench)
      newbench["type"] = ext
      output = subprocess.run(['./calc_diff.sh', bench["pre"]+'-'+ext+'.tree', bench["post"]+"-"+ext+'.tree'],capture_output=True)
      # print(output.stdout)
      # print(bench["pre"]+'-'+ext+'.tree')
      [td, id, sd] = output.stdout.split(b'\n')[0:3]

      newbench["diff"]={
        "total": eval(td),
        "impl": eval(id),
        "spec": eval(sd)
      }
      outbenches.append(newbench)

  with open(outfile,'w') as outf:
    json.dump(outbenches,outf)
  


if __name__ == "__main__":
  compareSizes("benches.json","bench-diffs.json")