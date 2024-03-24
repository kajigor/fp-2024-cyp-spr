#!/bin/bash
set -e
currentDir=$(cd -P -- "$(dirname -- "$0")" && pwd -P)

(cd $currentDir && ghc -o ./Main ./Main.hs)
(cd $currentDir && ./Main)
