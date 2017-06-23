#!/usr/bin/env bash
runghc -i./test test/TokeniserIndividualPartsTest.hs
runghc -i./test test/ReductionTest.hs
runghc -i./test test/EvalTest.hs
runghc -i./test test/EndToEndTest.hs
