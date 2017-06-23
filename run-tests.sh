#!/usr/bin/env bash
runghc -i./test test/TokeniserIndividualPartsTest.hs
runghc -i./test test/ReductionTest.hs
runghc -i./test test/EvalTest.hs
cd test
runghc -i./.. EndToEndTest.hs
cd ..
