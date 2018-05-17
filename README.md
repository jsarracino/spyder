# Spyder -- synthesis of web model-view programs

## Building
Spyder requires Boogaloo and Boogie as submodules, as well as a local installation of z3. Boogaloo is hosted on mercurial, so you have to first clone
Spyder, and then clone Boogaloo. To get everything, run
`git clone --recurse-submodules https://github.com/jsarracino/spyder.git && hg clone https://bitbucket.org/nadiapolikarpova/boogaloo`.

Spyder uses Stack -- to build, run
`stack build`.
## Running
To run Spyder, run
`stack exec -- spyder -i <path-to-input-file> -o <path-to-output-file>`.

## Debugging
Intermediate programs are placed at `debug.bpl` :).