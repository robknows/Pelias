#!/usr/bin/env bash
runghc -i./test test/TokeniserIndividualPartsTest.hs
runghc -i./test test/ReductionTest.hs
runghc -i./test test/EvalTest.hs
cd test
runghc -i./.. FixCatcherTest.hs
cd ..
runghc -i./test test/APITest.hs
