#*******************************************************************************
#
#-- Author: Steve Bosley HAG, UKMON, NEMETODE
#
#-- Description:
#
#   Common functions:
#
#   - Runscript         (run an external script with output to jpeg or PDF)
#   - get_stream        (prompt user for a list of streams)
#   - get_year          (Prompt user for year)
#   - read_ufo          (Read UFO Orbit data file)
#   - filter_stream     (Filter input data frame by type, stream, and year)
#   - get.bin.counts    (Frequency distribution with variable bin size)
#   - filter_apply_qa   (Apply QA filter)
#
#-- Shared under the Creative Common  Non Commercial Sharealike 
#   License V4.0 (www.creativecommons.org/licebses/by-nc-sa/4.0/)
#
#-- Version history:
#
#   Vers  Date    Notes
#   ----  ----    -----
#   1.0   31/10/2016  First release
#
#
#*******************************************************************************




Runscript <- function(Scriptfile,Otype=NA, orient="port",
                      Oyear=NA, Omonth=NA, Ocamera=NA, Ocategory=NA,Oshower=NA,
                      Olimit=NA) {
#===============================================================================
#
#-- Runs Scriptfile using source command, directing output as specified
#-- by otype.  Orientation set by orient parameter.  Page sizes are defined
#-- in the configuration file.
#
#===============================================================================
# Define Output Macro which runs script setting output, page orientation 
# and plot size
  
    output_type       = Otype
    paper_width       = orient[Otype,]$width
    paper_height      = orient[Otype,]$height
    paper_orientation = orient[Otype,]$papr
    
    #print(paste(Oyear,Omonth,Ocamera,sep="  "))
    if (is.na(Ocamera)) {
          SelectCam <<- ""
    }
    else {
          SelectCam <<- Ocamera
    }
    if (is.na(Ocategory)) {
          SelectCat <<- ""
    }
    else {
          SelectCat <<- Ocategory
    }      
    #stop()

    Outfile = sub("\\.R","\\.r",Scriptfile)
	  Outfile = paste(ReportDir,"/",sub("\\.r",paste("_",SelectYr,"_",SelectMon,"_",SelectCam,"_",SelectCat,sep=""),Outfile),sep="")
	  
	  #######################################################################################################
	  ######## Inserted by SRB to clear existing file which appears to be preventing write of new one #######
	  #######################################################################################################
	  if (file.exists(paste(Outfile,".",Otype,sep=""))) {
	     cat(paste("Remove existing:", paste(Outfile,".",Otype,sep="")),"\n")  
	     file.remove(paste(Outfile,".",Otype,sep=""))
	  } 
	  else {
	     cat(paste("No file found:", paste(Outfile,".",Otype,sep="")),"\n")  
	    
	  } 
	  #######################################################################################################
	  ####### end of SRB insert                                                                       #######
	  #######################################################################################################
	  
	  cat(paste("Running:", Scriptfile),"\n")  
	  #print(paste(Oyear,Omonth,sep=" "))
	  
	  source(paste(PlotDir,Scriptfile,sep="/"),local=TRUE)
	  
	  

	  #dev.off()

}

select_dev <- function(Ofile, Otype = "JPG", wd=1000, ht=1000, pp="a4") { 

  repeat{
    if(dev.cur() == 1) {
      break
    }
    dev.off()
  }
  
  plot.new()

  if (Otype=="JPG") jpeg(filename=paste(Ofile,".jpg",sep=""), 
                         width = wd, height = ht, 
                         units = "px", res=150, pointsize = 12, quality = 100, bg = "white")
  
  if (Otype=="PDF") pdf(paste(Ofile,".pdf",sep=""),
                        onefile=TRUE, paper=as.character(pp), 
                        width = wd, height = ht)
}




get_year <- function() {
#===============================================================================
#
#-- Presents a menu of years based on permitted values
#
#===============================================================================
#-- Get year
    
    Years <- c("ALL","2012","2013","2014","2015","2016","2017")
# Prompt user for year to be processed

    i <- menu(Years, graphics=TRUE, title="Choose Reporting Year")
    
    if (i==0) stop("No year selected")
    
    return(as.character(Years[i]))
}


get_month <- function() {
  #===============================================================================
  #
  #-- Presents a menu of years based on permitted values
  #
  #===============================================================================
  #-- Get month
  
  Months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  # Prompt user for month to be processed
  
  i <- menu(Months, graphics=TRUE, title="Choose Reporting Month")
  
  if (i==0) stop("No month selected")
  
  return(as.character(Months[i]))
}

convert_month <- function() {
  #===============================================================================
  #
  #-- returns the numeric value of SelectMon
  #
  #===============================================================================
  #-- Get month
  
  Months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","ALL","YTD")
  
  for (i in 1:14) {
    if(SelectMon == Months[i]) {
       return(i)
    }
  }
  
}



get_camera <- function() {
  #===============================================================================
  #
  #-- Presents a menu of cameras based on permitted values
  #
  #===============================================================================
  #-- Get camera
  
  Cameras <- c("ALL","north east","north west","south east","north","south")
  # Prompt user for camera to be processed
  
  i <- menu(Cameras, graphics=TRUE, title="Choose Camera")
  
  if (i==0) stop("No camera selected")
  
  return(as.character(Cameras[i]))
}

get_category <- function() {
  #===============================================================================
  #
  #-- Presents a menu of categories based on permitted values
  #
  #===============================================================================
  #-- Get category
  
  Categories <- c("ALL","Aircraft","Bat","Bird","Flash","Insect","Meteor",
               "Moon","Noise","Spider","Twilight","Weather","White")
  # Prompt user for category to be processed
  
  i <- menu(Categories, graphics=TRUE, title="Choose Category")
  
  if (i==0) stop("No category selected")
  
  return(as.character(Categories[i]))
}



read_ufo <- function(mx) {
#===============================================================================
#
#-- Presents file picker filtered for CSV files (if no preconfigured input file)
#-- Ingests file
#-- Standardises data
#
#===============================================================================

# - Read UFO Orbit data file
    
# setup filter
    filt <- matrix(c("Comma separated / Excel files","*.csv"), 
            nrow = 1, ncol = 2, byrow = TRUE,
            dimnames = list(c("csv"),c("V1","V2")))
    
if (is.na(SourceUnified)) {
        infile <- choose.files(caption = "Select UFO Orbit Unified file",multi = FALSE,filters=filt)
	} else {
        infile <- paste(DataDir,SourceUnified,sep="/")
	}

cat(paste("Ingesting UFO data file:",infile,"\n"))

if (infile == "") stop

# --- Read the UFO data file

# Read raw data
    mt <- read.csv(infile, header=TRUE)

# Standardise ID1
    mt$X_ID1<- substring(mt$X_ID1,2)

# standardise localtime
    mt$X_localtime <- as.POSIXct(strptime(mt$X_localtime, "_%Y%m%d_%H%M%S"))

# remove NA localtimes if any
    mt <- subset(mt, ! is.na(X_localtime))

# Standardise streamname
    mt$X_stream <- toupper(ifelse(substring(mt$X_stream,1,2)=="_J",substring(mt$X_stream,5),substring(mt$X_stream,2)))

    cat(paste("Rows ingested:",nrow(mt),"\n"))

return (mt)

}

filter_stream <- function(mx, mstream="ALL", myr="ALL", mtype="UNIFIED") {
#===============================================================================
#
#-- Filters input data frame mx for all meteors meeting selection criteria
#-- (stream and year)
#
#===============================================================================
#-- Filter input data frame by type (e.g. unified), stream, and year
    cat(paste("Filtering input for stream:",mstream,", year:", myr,", type:",mtype,"\n"))
    if (mtype == "UNIFIED") {
	my <- subset(mx, substring(X_ID1,2,8) == "UNIFIED")
	my$X_ID1[nchar(my$X_ID1) == 11] <- sub("D_","D_0",my$X_ID1[nchar(my$X_ID1) == 11])
	}

    if (mtype != "UNIFIED") my <- subset(mx, substring(X_ID1,2,8) != "UNIFIED")

    if (mstream != "ALL")  my <- subset(my, X_stream == mstream)
    if (myr     != "ALL")  my <- subset(my, substring(my$X_localtime,1,4) == myr)
    cat(paste(mtype,"rows:",nrow(my),"\n"))    
    return (my)

}


