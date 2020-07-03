## provTraceR

The provTraceR package displays information about files used or created 
by an R script or a series of R scripts.

## Installation
To install from GitHub:

```{r}
devtools::install_github("End-to-end-provenance/provTraceR")
```
Once installed, load the package:

```{r}
library("provTraceR")
```

## Usage
This package includes two functions:

1. To use existing provenance to trace file lineage:

```{r}
prov.trace(scripts, prov.dir=NULL, file.details=FALSE, save=FALSE, save.dir=NULL, check=TRUE)
```

2. To run one or more scripts, collect provenance, and trace file lineage:

```{r}
prov.trace.run(scripts, prov.dir=NULL, file.details=FALSE, save=FALSE, save.dir=NULL, check=TRUE, prov.tool="rdtLite", details=FALSE, ...)
```

The <i>scripts</i> parameter may contain a single script name, a vector
of script names, or a text file (with extension .txt) of script names.

For <i>prov.trace</i> only: If more than one script is specified, the order
of the scripts must match the order of execution as recorded in the 
provenance; otherwise an error message is displayed. For console sessions,
set <i>scripts</i> = "console".

For <i>prov.trace.run</i> only: The provenance collection tool specified by
prov.tool must be "rdtLite" or "rdt". If details = TRUE, fine-grained provenance
is collected. Other optional parameters (...) are passed to rdtLite or rdt.

It is assumed that provenance for each script is stored under a single
provenance directory set by the <i>prov.dir</i> option.  If not, the provenance
directory may be specified with the <i>prov.dir</i> parameter. Timestamped 
provenance and provenance in scattered locations are not currently supported.

Files are matched by hash value. INPUTS lists files that are required 
to run the script or scripts. These include files read by a script and not
written by an earlier script or previously written by the same script.
OUTPUTS lists files written by the script or scripts. EXCHANGES lists 
files with the same hash value that were written by one script and read 
by a later script; if the location changed, both locations are listed.

If <i>file.details</i> = TRUE, additional details are displayed, including script
execution timestamps, saved file names, and file hash values.

If <i>save</i> = TRUE, results are displayed in the console and saved to the
file prov-trace.txt.

The <i>save.dir</i> parameter determines where the results file is saved. 
If NULL (the default), the R session temporary directory is used. If a period (.),
the current working directory is used. Otherwise the directory specified by
save.dir is used.

If <i>check</i> = TRUE, each file recorded in the provenance is checked against the
user's file system.  A dash (-) in the output indicates that the file no longer
exists, a plus (+) indicates that the file exists but the hash value has changed,
and a colon (:) indicates that the file exists and the hash value is unchanged.
If <i>check</i> = FALSE, no comparison is made.

## Examples

```{r}
prov.trace("script.R")
prov.trace(c("script1.R", "script2.R", "script3.R"))
prov.trace("analyze-data.txt")
prov.trace("console")

prov.trace.run("script.R")
prov.trace.run(c("script1.R", "script2.R", "script3.R"))
prov.trace.run("analyze-data.txt")
```

where analyze-data.txt contains:

```{r}
script1.R
script2.R
script3.R
```

