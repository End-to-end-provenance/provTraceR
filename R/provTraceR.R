# Copyright (C) President and Fellows of Harvard College and 
# Trustees of Mount Holyoke College, 2020.

# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public
#   License along with this program.  If not, see
#   <http://www.gnu.org/licenses/>.

###############################################################################

# The provTraceR package displays information about files used or created 
# by an R script or a series of R scripts.

# The prov.trace function uses existing provenance to trace file
# lineage. The prov.trace.run function runs the specified script(s),
# collects provenance, and uses the provenance to trace file lineage.

# The scripts parameter may contain a single script name, a vector
# of script names, or a text file (with extension .txt) of script names.

# For prov.trace only: If more than one script is specified, the order
# of the scripts must match the order of execution as recorded in the 
# provenance; otherwise an error message is displayed. For console sessions,
# set scripts = "console".

# For prov.trace.run only: The provenance collection tool is specified by the
# parameter prov.tool and must be "rdtLite" or "rdt". The default is "rdtLite".

# It is assumed that provenance for each script is stored under a single
# provenance directory set by the prov.dir option.  If not, the provenance
# directory may be specified with the prov.dir parameter. Timestamped 
# provenance and provenance in scattered locations are not currently supported.

# Files are matched by hash value. INPUTS lists files that are read by a script
# but not written by the same script or an earlier script. OUTPUTS lists files 
# written by a script. EXCHANGES lists files with the same hash value that were
# written by one script and read by a later script; if the location changed, 
# both locations are listed.

# In the output, a dash (-) indicates that the file no longer exists at the
# original location, a plus (+) indicates that the file exists but the hash
# value has changed, and a colon (:) indicates that the file exists and the
# hash value is unchanged.

# If file.details = TRUE, additional details are displayed, including script
# execution timestamps, saved file names, and file hash values.

# If save = TRUE, results are displayed in the console and saved to the file
# prov-trace.txt.

# The parameter save.dir determines where the results file will be saved.
# If save.dir is NULL (the default), the R session temporary directory is used.
# If save.dir = ".", the current working directory is used. Otherwise the 
# directory specified by save.dir is used.

###############################################################################

#' @title
#' File lineage functions
#' @description
#' prov.trace traces file lineage from existing provenance. 
#' @param scripts a script name, a vector of script names, a text file of script 
#' names (file extension = .txt), or "console"
#' @param prov.dir provenance directory
#' @param file.details whether to display file details
#' @param save whether to save results to the file prov-trace.txt
#' @param save.dir where to save the results file. If NULL, use the R session 
#' temporary directory. If a period (.), use the current working directory.
#' Otherwise use save.dir.
#' @return no return value
#' @export
#' @rdname lineage

prov.trace <- function(scripts, prov.dir=NULL, file.details=FALSE, save=FALSE,
	save.dir=NULL) {

	if (tools::file_ext(scripts) == "txt") {
		scripts <- get.scripts.from.file(scripts)
	}
	prov <- get.provenance(scripts, prov.dir)
	check.order.of.execution(prov, scripts)
	trace.files(prov, scripts, file.details, save, save.dir)
}

#' @description
#' prov.trace.run runs the specified script(s), collects provenance, and uses
#' the provenance to trace file lineage.
#' @param scripts a script name, a vector of script names, or a text file of 
#' script names (file extension = .txt)
#' @param prov.dir provenance directory
#' @param file.details whether to display file details
#' @param save whether to save results to the file prov-trace.txt
#' @param save.dir where to save the results file. If NULL, use the R session 
#' temporary directory. If a period (.), use the current working directory.
#' Otherwise use save.dir.
#' @param prov.tool provenance collection tool (rdtLite or rdt)
#' @param details whether to collect fine-grained provenance
#' @param ... other parameters passed to the provenance collector
#' @return no return value
#' @export
#' @rdname lineage

prov.trace.run <- function(scripts, prov.dir=NULL, file.details=FALSE, save=FALSE,
	save.dir=NULL, prov.tool="rdtLite", details=FALSE, ...) {

	if (tools::file_ext(scripts) == "txt") {
		scripts <- get.scripts.from.file(scripts)
	}
	run.scripts(scripts, prov.tool, details, ...)
	prov <- get.provenance(scripts, prov.dir)
	trace.files(prov, scripts, file.details, save, save.dir)
}

#' get.scripts.from.file reads one or more script names from a text file
#' (file extension must be .txt). Blank lines are ignored.
#' @param scripts a text file containing script names
#' @return a vector of script names
#' @noRd

get.scripts.from.file <- function(scripts) {
	if (!file.exists(scripts)) {
		cat(scripts, "not found")
		stop()
	}
	script.names <- readLines(scripts, warn=FALSE)
	ss <- vector()
	# skip blank lines
	for (i in 1:length(script.names)) {
		if (script.names[i] != "") {
			ss <- append(ss, script.names[i])
		}
	}
	return(ss)
}

#' run.scripts run scripts in the order specified and collects provenance.
#' @param scripts a vector of script names
#' @param prov.tool provenance collection tool (rdtLite or rdt)
#' @param details whether to collect fine-grained provenance
#' @param ... other parameters passed to the provenance collector
#' @return no return value
#' @noRd

run.scripts <- function(scripts, prov.tool, details, ...) {
	# get provenance collection tool
	if (prov.tool == "rdtLite") {
		prov.run <- rdtLite::prov.run
	} else if (prov.tool == "rdt") {
		prov.run <- rdt::prov.run
	} else {
		cat("Provenance collector must be rdtLite or rdt")
		stop()
	}
	# run each script in turn
	for (i in 1:length(scripts)) {
		if (scripts[i] == "console") {
			cat("Use prov.trace for console sessions")
			stop()
		}
		if (!file.exists(scripts[i])) {
			cat(scripts[i], "not found")
			stop()
		}
		tryCatch (rdtLite::prov.run(scripts[i], details=details, ...), error = function(x) {print (x)})
	}
}

#' get.provenance returns a list containing provenance for each script.
#' @param scripts a vector of script names
#' @param prov.dir provenance directory
#' @return a list of provenance for each script
#' @noRd

get.provenance <- function(scripts, prov.dir) {
	snum <- length(scripts)
	prov <- list()
	# get provenance directory
	if (is.null(prov.dir)) {
		prov.dir <- getOption("prov.dir")
	} else {
		prov.dir <- normalizePath(prov.dir, winslash="/", mustWork=FALSE)
	}
	if (!dir.exists(prov.dir)) {
		cat(prov.dir, "not found")
		stop()
	}
	# get provenance for each script
	for (i in 1:snum) {
		if (scripts[i] == "console") {
			file.name <- "console"
		} else {
			file.name <- substr(scripts[i], 1, nchar(scripts[i])-2)
 		}
 		prov.file <- paste(prov.dir, "/prov_", file.name, "/prov.json", sep="")
 		if (!file.exists(prov.file)) {
 			cat(prov.file, "not found")
 			stop()
 		}
		prov[[i]] <- provParseR::prov.parse(prov.file)
	}
	return(prov)
}

#' check.order.of.execution checks the order of execution against the 
#' specified order.
#' @param prov a list of provenance for each script
#' @param scripts a vector of script names
#' @return no return value
#' @noRd

check.order.of.execution <- function(prov, scripts) {
	snum <- length(scripts)
	# not relevant for a single script
	if (snum > 1) {
		ts <- vector(length=snum)
		for (i in 1:snum) {
			ee <- provParseR::get.environment(prov[[i]])
			ts[i] <- ee[ee$label=="provTimestamp", "value"]
		}
		for (i in 1:(snum-1)) {
			if (ts[i] > ts[i+1]) {
				cat("Order of execution does not match\n")
				stop()
			}
		}
	}
}

#' trace.files collects information on input and output files and 
#' generates output.
#' @param prov a list of provenance for each script
#' @param scripts a vector of script names
#' @param file.details whether to display file details
#' @param save whether to save results to the file prov-trace.txt
#' @param save.dir where to save the results file
#' @return no return value
#' @noRd

trace.files <- function(prov, scripts, file.details, save, save.dir) {
	infiles <- get.infiles(prov, scripts)
	outfiles <- get.outfiles(prov, scripts)
 	# output to console and file
 	if (save) {
		save.to.text.file(prov, scripts, infiles, outfiles, file.details, save.dir)
	}
	# output to console only
	else {
		display.output(prov, scripts, infiles, outfiles, file.details)
	}
}

#' save.to.text.file sends output to the console and to the file prov-trace.txt
#' on the current working directory.
#' @param prov a list of provenance for each script
#' @param scripts a vector of script names
#' @param infiles a data frame of input files
#' @param outfiles a data frame of output files
#' @param file.details whether to display file details
#' @param save.dir where to save the results file
#' @return no return value
#' @noRd

save.to.text.file <- function(prov, scripts, infiles, outfiles, file.details, save.dir) {
	sdir <- get.save.dir(save.dir)
	trace.file <- paste(sdir, "/prov-trace.txt", sep="")
	sink(trace.file, split=TRUE)
	display.output(prov, scripts, infiles, outfiles, file.details)
	sink()
	cat(paste("\nSaving results in", trace.file))
}

#' get.save.dir returns the directory where the results file (prov-trace.txt) 
#' will be saved. If NULL, the R session temporary directory is used. If a 
#' period (.), the current working directory is used. Otherwise save.dir is used.
#' @param save.dir where to save the results file
#' @return the result file directory
#' @noRd

get.save.dir <- function(save.dir) {
	if (is.null(save.dir)) {
		sdir <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
	} else if (save.dir == ".") {
		sdir <- getwd()
	} else {
		sdir <- normalizePath(save.dir, winslash="/", mustWork=FALSE)
		if (!dir.exists(sdir)) {
			cat(sdir, "not found")
			stop()
		}
	}
	return(sdir)
}

#' display.output generates output and sends it to the console.
#' @param prov a list of provenance for each script
#' @param scripts a vector of script names
#' @param infiles a data frame of input files
#' @param outfiles a data frame of output files
#' @param file.details whether to display file details
#' @return no return value
#' @noRd

display.output <- function(prov, scripts, infiles, outfiles, file.details) {
	display.scripts(prov, scripts, file.details)
	display.input.files(infiles, outfiles, file.details)
	display.output.files(outfiles, file.details)
	display.exchanged.files(scripts, infiles, outfiles, file.details)
}

#' check.file.system checks if the specified file exists in its original
#' location and if the hash value has changed,
#' @param file original path and file name
#' @return a coded string value
#' @noRd

check.file.system <- function(file) {
	location <- file$location
	hash.algorithm <- file$hash.algorithm
	if (!file.exists(location)) {
		tag <- "-"
	} else if (file$hash != digest::digest(file=location, algo=hash.algorithm)) {
		tag <- "+"
	} else {
		tag <- ":"
	}
	return(tag)
}

#' get.infiles returns a data frame of all input files.
#' @param prov a list of provenance for each script
#' @param scripts a vector of script names
#' @return a data frame of input files
#' @noRd

get.infiles <- function(prov, scripts) {
	snum <- length(scripts)
	infiles.list <- list()
	for (i in 1:snum) {
 		infiles.list[[i]]  <- provParseR::get.input.files(prov[[i]])
 		if (nrow(infiles.list[[i]]) > 0) {
 			# add script number
 			infiles.list[[i]]$script <- i
 			# add hash algorithm
			ee <- provParseR::get.environment(prov[[i]])
 			infiles.list[[i]]$hash.algorithm <- ee[ee$label=="hashAlgorithm", "value"]
 		}
	}
	infiles <- infiles.list[[1]]
	if (snum > 1) {
		for (i in 2:snum) {
			infiles <- rbind(infiles, infiles.list[[i]])
		}
	}
	return(infiles)
}

#' get.outfiles returns a data frame of all output files.
#' @param prov a list of provenance for each script
#' @param scripts a vector of script names
#' @return a data frame of output files
#' @noRd

get.outfiles <- function(prov, scripts) {
	snum <- length(scripts)
	outfiles.list <- list()
	for (i in 1:snum) {
 		outfiles.list[[i]]  <- provParseR::get.output.files(prov[[i]])
 		if (nrow(outfiles.list[[i]]) > 0) {
 			# add script number
			outfiles.list[[i]]$script <- i
 			# add hash algorithm
			ee <- provParseR::get.environment(prov[[i]])
 			outfiles.list[[i]]$hash.algorithm <- ee[ee$label=="hashAlgorithm", "value"]
 		}
	}
	outfiles <- outfiles.list[[1]]
	if (snum > 1) {
		for (i in 2:snum) {
			outfiles <- rbind(outfiles, outfiles.list[[i]])
		}
	}
	return(outfiles)
}

#' display.scripts displays information for each script in the console.
#' @param prov a list of provenance for each script
#' @param scripts a vector of script names
#' @param file.details whether to display file details
#' @return no return value
#' @noRd

display.scripts <- function(prov, scripts, file.details) {
	cat("SCRIPTS:\n\n")
	snum <- length(scripts)
	for (i in 1:snum) {
		ee <- ee <- provParseR::get.environment(prov[[i]])
		script.name <- ee[ee$label=="script", "value"]
		prov.timestamp <- ee[ee$label=="provTimestamp", "value"]
		if (script.name == "") {
			script.name <- "Console session"
		}
		cat(i, ":", script.name, "\n")
		if (file.details == TRUE) {
			cat("        Executed:", prov.timestamp, "\n\n")
		}
	}
	cat("\n")
}

#' display.input.files displays information for input files in the console.
#' @param infiles a data frame of input files
#' @param outfiles a data frame of output files
#' @param file.details whether to display file details
#' @return no return value
#' @noRd

display.input.files <- function(infiles, outfiles, file.details) {
	cat("INPUTS:\n\n")
	count <- 0	
	if (nrow(infiles) > 0) {
		# check for preceding or current output file with same hash value
		infiles$match <- FALSE
		for (i in 1:nrow(infiles)) {
			if (nrow(outfiles) > 0) {
				for (j in 1:nrow(outfiles)) {
					if (outfiles$script[j] <= infiles$script[i] && infiles$hash[i] == outfiles$hash[j]) {
						infiles$match[i] <- TRUE
					}
				}
			}
		}
		index <- which(infiles$match == FALSE)
		count <- length(index)
		if (count > 0) {
			ii <- infiles[index, ]
			# order by script number and location
			ii <- ii[order(ii$script, ii$location), ]
			for (i in 1:nrow(ii)) {
				if (ii$location[i] != "") {
					# display location for files
					tag <- check.file.system(ii[i, ])
					cat(ii$script[i], tag, ii$location[i], "\n")
					if (file.details == TRUE) {
						cat("        Saved:", ii$value[i], "\n")
						cat("        Hash: ", ii$hash[i], "\n\n")
					}
				} else {
					# display name for urls
					cat(ii$script[i], ":", ii$name[i], "\n")
					if (file.details == TRUE) {
						cat("\n")
					}

				}
			}
		}
	}
	if (count == 0) {
		cat("None\n")
	}
	cat("\n")
}

#' display.output.files displays information for output files in the console.
#' @param outfiles a data frame of output files
#' @param file.details whether to display file details
#' @return no return value
#' @noRd

display.output.files <- function(outfiles, file.details) {
	cat("OUTPUTS:\n\n")
	if (nrow(outfiles) > 0) {
		oo <- outfiles
		# order by script number and location
		oo <- oo[order(oo$script, oo$location), ]
		for (i in 1:nrow(oo)) {
			tag <- check.file.system(oo[i, ])
			cat(oo$script[i], tag, oo$location[i], "\n")
			if (file.details == TRUE) {
				cat("        Saved:", oo$value[i], "\n")
				cat("        Hash: ", oo$hash[i], "\n\n")
			}
		}
	} else {
		cat("None\n")
	}
	cat("\n")
}

#' display.exchanged.files displays information for files written by one script and read 
#' by a subsequent script in the console.
#' @param scripts a vector of script names
#' @param infiles a data frame of input files
#' @param outfiles a data frame of output files
#' @param file.details whether to display file details
#' @return no return value
#' @noRd

display.exchanged.files <- function(scripts, infiles, outfiles, file.details) {
	snum <- length(scripts)
	# not relevant for a single script
	if (snum > 1) {
		cat("EXCHANGES:\n\n")
		count <- 0
		for (i in 2:snum) {
			for (j in 1:nrow(infiles)) {
				for (k in 1:nrow(outfiles)) {
					if (infiles$script[j] == i && outfiles$script[k] < i && infiles$hash[j] == outfiles$hash[k]) {
						# display infile location if hash values match
						tag <- check.file.system(infiles[j, ])
						cat(outfiles$script[k], ">", infiles$script[j], tag, infiles$location[j], "\n")
						if (file.details == TRUE) {
							cat("        Saved:", infiles$value[i], "\n")
							cat("        Hash: ", infiles$hash[i], "\n\n")
						}
						if (infiles$location[j] != outfiles$location[k]) {
							# display outfile location if different from infile location
							tag <- check.file.system(outfiles[k, ])
							cat("      ", tag, " ", outfiles$location[k], "\n")
							if (file.details == TRUE) {
								cat("        Saved:", outfiles$value[i], "\n")
								cat("        Hash: ", outfiles$hash[i], "\n\n")
							}
						}
						count <- count + 1
					}
				}
			}
		}
		if (count == 0) {
			cat("None\n\n")
		}
	}
}

