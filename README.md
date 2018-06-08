# Spyder -- synthesis of web model-view programs

## Building
Spyder requires Boogaloo and Boogie as submodules, as well as a local installation of z3. Boogaloo is hosted on mercurial, so you have to first clone
Spyder, and then clone Boogaloo. To get everything, run
`git clone --recurse-submodules https://github.com/jsarracino/spyder.git && cd spyder && hg clone https://bitbucket.org/nadiapolikarpova/boogaloo`.

You also have to build Boogie -- follow the directions in boogie/README.md. The script `boog.sh` calls Boogie using Mono -- to use in other platforms, tweak `boog.sh` to invoke Boogie properly.

Spyder uses Stack -- to build, run
`stack build`.
## Running
To run Spyder, run
`stack exec -- spyder -i <path-to-input-file> -o <path-to-output-file>`.

Some examples are in `test/bench/spy/examples/`, e.g. `test/bench/spy/examples/num-basic.spy`.

## Debugging
Intermediate programs are placed in `compile-debug.bpl` and `cegis-debug.bpl` :).