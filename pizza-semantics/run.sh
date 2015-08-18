#!/bin/bash
echo "starting fancy semantics"
gf -make FoodsEng.gf 2> /dev/null
echo "Write sentences, e.g. \"this very Italian pizza is very Italian\""
runghc foodstate.hs
