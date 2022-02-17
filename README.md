# Spyder -- synthesis for maintenance of data invariants

## Building
Spyder requires a fork of Boogaloo and Boogie as submodules, as well as a local installation of Z3.

For Boogaloo and Boogie, you can get these with git clone when you checkout spyder, for example:

`git clone --recurse-submodules https://github.com/jsarracino/spyder.git`.

You also have to build Boogie -- follow the directions in boogie/README.md. The script `boog.sh` calls Boogie using Mono -- to use in other platforms, tweak `boog.sh` to invoke Boogie properly.

Spyder uses (a somewhat dated version of) Stack, which is tested for Stack Version 1.7.1, and Z3 (tested on Z3 version 4.5.0).
You will need to install Z3's header files to make things work, and the easiest way to do this is to build Z3 from source.

## Running
To run Spyder, run
`./Script.hs -i <path-to-input-file> -o <path-to-output-file>`.

Some examples are in `test/bench/spy/examples/`, e.g. `test/bench/spy/examples/num-basic.spy`.
Note that Spyder produces output in *Boogie*, not Spyder, so the result is a bit mangled compared to the input code and specifications.

The compilation can take some time to run and produces some warning messages, as well as some intermediate errors from Boogie. This is normal (indeed, the errors are used by Spyder to determine whether or not a program needs to be repaired).
Here is some example output on WSL: 
```
john@DESKTOP-45ON76F:~/spyder$ time ./Script.hs -i test/bench/spy/examples/num-mid-2d.spy -o num-basic.bpl

Language/Spyder/Translate/Specs.hs:46:5: Warning:
    Pattern match(es) are overlapped
    In an equation for ‘recur’: recur _ = ...

Language/Spyder/Translate/Rebuild.hs:53:10: Warning:
    Pattern match(es) are overlapped
    In a case alternative: otherwise -> ...
source: size 24
inv: size 10
 CHECKING BOOGIE AT tmp.bpl:
tmp.bpl(29,1): Error BP5003: A postcondition might not hold on this return path.
COMPLETING: incM
 CHECKING BOOGIE AT check.bpl:
check.bpl(25,5): Error BP5001: This assertion might not hold.
 CHECKING BOOGIE AT check.bpl:
 CHECKING BOOGIE AT check.bpl:
Done!

real    0m36.214s
user    0m34.998s
sys     0m1.142s
```

## Debugging
Intermediate programs are placed in `tmp.bpl`, `check.bpl`, `compile-debug.bpl`, `cegis-test-debug.bpl`, and `cegis-search-debug.bpl`. Feel free to delete these.