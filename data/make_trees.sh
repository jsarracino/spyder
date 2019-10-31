#!/usr/bin/env bash

# benches=(Midpoint_2 Midpoint_3 Midpoint_3_dist Midpoint_3_dist_1D Bound_2 Bound_3 Bound_3_1D GoL1D GoL1D_Buttons Expenses Expenses_1D Overview)
# benches=(Expenses Expenses_1D Expenses_1D_Sum Expenses_1D_Sum_Color Overview)
# benches=(Bound_2 Bound_3 Bound_3_1D)
benches=(Midpoint_3_dist_1D)

exts=(frp imp spy)
prefix=test/bench/spy/benchs
root=~/spyder

pushd $root 1> /dev/null

for bench in "${benches[@]}"
do
  echo "treeifying $bench"
  for ext in "${exts[@]}"
  do
    ./Script.hs -d -i $prefix/$bench.$ext -o data/$bench-$ext.tree
  done 
done

popd 1> /dev/null