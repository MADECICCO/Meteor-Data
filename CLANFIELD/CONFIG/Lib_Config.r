# =================================================================
#
#-- Report suite master
#
#-- Author: Steve Bosley, HAG, UKMON, NEMETODE
#-- Version 1.0, 31/10/2016
#
#-- Shared under the Creative Common  Non Commercial Sharealike 
#   License V4.0 (www.creativecommons.org/licebses/by-nc-sa/4.0/)
#
#-- Version History
#  ----------------
#   Vers  Date          Note
#   1.0   31/10/2016    First release
#
# =================================================================


## Set libraries
library(readODS)
library(dplyr)
library(lubridate)
library(ggplot2)
library("RColorBrewer")

#-- Source data file name (Value of NA triggers file picker dialogue)


SourceUnified   = NA

#-- Page sizing

Portrait = data.frame(
    otype=c("JPG","PDF"),
    width=c(877,6.5),
    height=c(1745,9.5),
    papr = "a4",
    row.names=1)

Landscape = data.frame(
    otype=c("JPG","PDF"),
    width=c(1745,9.5),
    height=c(877,6.5),
    papr = "a4r",
    row.names=1)

#-- Timeline Plot intervals  (change to suit stream reporting)

SelectInterval    = 10 * 60 # in seconds
SelectIntervalSol = 0.01    # in degrees solar longitude

#-- Options for output
OutType = "JPG"
#OutType = "PDF"
#OutType = "CONSOLE"

#-- Filesystem parameters

root = "D:/R Libraries/MeteorData/R MONTHLY REPORTING/CLANFIELD"		# Filesystem root 

#-- Analysis folders

WorkingDir     = paste(root,"/RWORKSPACE",sep="")        # R workspace for saved R environments
DataDir        = paste(root,"/DATA",sep="")              # Source meteor data
FuncDir        = paste(root,"/LIBRARY/FUNCTIONS",sep="") # Path to R report code modules
PlotDir        = paste(root,"/LIBRARY/PLOTS",sep="")     # Path to R report code modules
TabsDir        = paste(root,"/LIBRARY/TABLES",sep="")    # Path to R report code modules
RefDir         = paste(root,"/CONFIG",sep="")            # Path to Reference data (meteor lookup)
ReportDir      = paste(root,"/REPORTS",sep="")		       # Path to output report directory