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
prov.trace(scripts, prov.dir=NULL, file.details=FALSE)
```

2. To run one or more scripts, collect provenance, and trace file lineage:

```{r}
prov.trace.run(scripts, prov.dir=NULL, file.details=FALSE, details=FALSE)
```

The <i>scripts</i> parameter may contain a single script name, a vector
of script names, or a text file (with extension .txt) of script names.

For <i>prov.trace</i> only: If more than one script is specified, the order
of the scripts must match the order of execution as recorded in the 
provenance; otherwise an error message is displayed. For console sessions,
set <i>scripts</i> = "console".

It is assumed that provenance for each script is stored under a single
provenance directory set by the <i>prov.dir</i> option.  If not, the provenance
directory may be specified with the <i>prov.dir</i> parameter. Timestamped 
provenance and provenance in scattered locations are not currently supported.

Files are matched by hash value. INPUTS lists files that are read by a script
but not written by the same script or an earlier script. OUTPUTS lists files 
written by a script. EXCHANGES lists files with the same hash value that were
written by one script and read by a later script; if the location changed, 
both locations are listed.

In the output, a dash (-) indicates that the file no longer exists at the
original location, a plus (+) indicates that the file exists but the hash
value has changed, and a colon (:) indicates that the file exists and the
hash value is unchanged.

If <i>file.details</i> = TRUE, additional details are displayed, including script
execution timestamps, saved file names, and file hash values.

If <i>details</i> = TRUE, fine-grained provenance is collected by <i>rdtLite</i>;
otherwise only coarse-grained provenance is collected.

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

