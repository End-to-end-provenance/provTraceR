library(provTraceR)
library(testthat)

# get provenance directory
prov.dir <- system.file("testdata", package="provTraceR", mustWork=TRUE)

# test prov.trace with vector input
test.expected <- system.file("testexpected", "prov.trace.expected", package="provTraceR", mustWork=TRUE)
expect_known_output(prov.trace(c("script-1.R", "script-2.R"), prov.dir=prov.dir, check=FALSE), test.expected, update=FALSE)

# test prov.trace with text file input
test.expected <- system.file("testexpected", "prov.trace.expected", package="provTraceR", mustWork=TRUE)
expect_known_output(prov.trace("scripts-1-2.txt", prov.dir=prov.dir, check=FALSE), test.expected, update=FALSE)

# test prov.trace with file details = TRUE
test.expected <- system.file("testexpected", "prov.trace.details.expected", package="provTraceR", mustWork=TRUE)
expect_known_output(prov.trace("scripts-1-2.txt", prov.dir=prov.dir, file.details=TRUE, check=FALSE), test.expected, update=FALSE)

# test prov.trace.run
test.expected <- system.file("testexpected", "prov.trace.expected", package="provTraceR", mustWork=TRUE)
expect_known_output(prov.trace.run("scripts-1-2.txt", prov.dir=prov.dir, check=FALSE), test.expected, update=FALSE)

# test prov.trace with same file
test.expected <- system.file("testexpected", "prov.trace.same.expected", package="provTraceR", mustWork=TRUE)
expect_known_output(prov.trace("script-3.R", prov.dir=prov.dir, check=FALSE), test.expected, update=FALSE)

# test prov.trace with scripts in wrong order
expect_error(prov.trace(c("script-2.R", "script-1.R"), prov.dir=prov.dir))
