#!/usr/bin/env python3

import settings
import json
import copy
import subprocess

def getNum(byts):
  return int(byts.split(b' ')[-1])

def calcSizes(benchf, outfile):
  with open(benchf) as inf:
    benches = json.load(inf)
  exts = ["imp", "frp", "spy"]

  outbenches = []

  

  for bench in benches:
  # for bench in ["Bound_2", "Bound_3", "Bound_3_1D"]:

    print("Calculating size for " + bench)
    
    for ext in exts:
      newbench = {"name":bench,"type":ext}
      output = subprocess.run(['./calc_size.sh', bench+'.'+ext], capture_output=True)
      # print(output.stdout)
      [ib, sb] = output.stdout.split(b'\n')[0:2]
      # print([getNum(ib), getNum(sb)])
      newbench["impl"]=getNum(ib)
      newbench["spec"]=getNum(sb)
      newbench["total"]=newbench["impl"] + newbench["spec"]
      outbenches.append(newbench)

  with open(outfile,'w') as outf:
    json.dump(outbenches,outf)

if __name__ == "__main__":
  calcSizes(settings.names,settings.sizes)