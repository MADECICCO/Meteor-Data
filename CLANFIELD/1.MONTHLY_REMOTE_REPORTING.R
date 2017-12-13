#=============================================================================
#
#-- Author: Steve Bosley - HAG, UKMON, NEMETODE
#
#
#-- Description:
#
#   This script runs a set of R scripts which generate tables and reports
#   from a file containing station observations created using UFO Analyser.  
#
#   This script prompts the user for the output type (PDF, JPEG or CONSOLE), 
#   a stream name and year.  It then filters and standardises UFO orbit data
#   before calling the plot / table routines.
#
#   Each script can use the following data prepared by this master script:
#
#   CategoryData:  The selected remote monitoring data
#   Dataset:       A descriptive title printed on the plot footer
#   SelectYear:    The 4 digit number of the year for which reporting is required
#   SelectMonth:   The 3 character abbreviation of the reporting month required
#   SelectCamera:  The 2 character identifier of the camera for which reporting is required
#   SelectCategory:The name of the event category for which reporting is required
#
#   Environment varables are set by script Lib_Config.r which sets pointers
#   to source data, directory holding the scripts, directory recieving reports
#   etc. This file must be first configured to match the installation.
#
#   Note, the distribution (ANALYSIS folder) must be held in My Documents
#
#-- Shared under the Creative Common  Non Commercial Sharealike 
#   License V4.0 (www.creativecommons.org/licenses/by-nc-sa/4.0/)
#
#-- Prerequisites
#   Ensure that the file "Clanfield Meteor Camera Remote Monitoring for 2017 "
#   has been updated for the reporting month AND copied into Data folder
#
#-- Version history
#
#   Vers  Date          Notes
#   ----  ----          -----
#   1.0   30/11/2016    First release
#
#=============================================================================

cat("Reporting started ",format(Sys.time(), "%a %b %d %Y %H:%M:%S"))

# Initialise environment variables and common functions

  source("D:/R Libraries/MeteorData/R MONTHLY REPORTING/CLANFIELD/CONFIG/Lib_Config.r")
  source(paste(FuncDir,"/common_functions.r",sep=""))

# Close any open graphical output devices (other than NULL)
 repeat{
   if(dev.cur() == 1) {
     break
   }
   dev.off()
 }
# Select Output Type
 if (is.na(OutType)) {
   Olist = c("PDF","JPG")
   i <- menu(Olist, graphics=TRUE, title="Choose output type")
   OutType = Olist[i]
 }
  
# Set the R working directory (this is where R saves its environment)

setwd(WorkingDir)
  
# Read the Remote Analysis spreadsheet(s) produced when the video clips were reviewed using TeamViewer 
# prior to downloading for home PC Analysis using UFO Analyser.
# NB the spreadsheet maintenance is a manual activity used to record counts of event categories deleted
# or kept during this initial review. It is Clanfield specific - intended to understand the causes of
# non-meteoric events - but it could be copied by any other station that wanted to.
    

# Select which  year / month to process
    
OutYear <<- get_year()
OutMonth <<- get_month()
OutCamera <<- get_camera()
OutCat <<- get_category()
        
SelectYr <<- OutYear
SelectMon <<- OutMonth
SelectCam <<- OutCamera
SelectCat <<- OutCat

## get month number ##########################################################################
## setup arrays of valid Months
valid_months <<- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
numeric_months <<- c(1,2,3,4,5,6,7,8,9,10,11,12)
Months_len <<- length(valid_months)
m <<- 0
m_valid <<- FALSE
while (!m_valid) {
  m <<- m + 1
  if (as.character(OutMonth) != valid_months[m]) { 
    ## no match found
  }
  else {
    ## match found
    m_valid <<-- TRUE
    month_index <<- m
  }        
}
################################################################################################################
        
## Read in all required Remote Monitoring Data
Runscript("Write_Pareto.r",Oyear="ALL",Omonth="ALL",Ocamera="ALL",Otype=OutType,orient=Landscape)

## Produce Monthly Charts ######################################################################################
## NORTH AND SOUTH CAMERAS
if (OutYear < "2016" | (OutYear == "2016" & OutMonth == "Jan")) {
        cat("Printing NORTH and SOUTH Monthly Charts ")
        Runscript("Chart_Pareto.r",Oyear=OutYear,Omonth=OutMonth,Ocamera="north",Otype=OutType,orient=Landscape)
        Runscript("Chart_Pareto.r",Oyear=OutYear,Omonth=OutMonth,Ocamera="south",Otype=OutType,orient=Landscape)
        Runscript("Chart_Daily_Bar.r",Oyear=OutYear,Omonth=OutMonth,Ocamera="north",Otype=OutType,orient=Landscape)
        Runscript("Chart_Daily_Bar.r",Oyear=OutYear,Omonth=OutMonth,Ocamera="south",Otype=OutType,orient=Landscape)
}
## NORTH EAST CAMERA
if (OutYear > "2015" | (OutYear == "2015" & month_index > 3)) {
        cat("Printing NORTH EAST Monthly Charts ")
        Runscript("Chart_Pareto.r",Oyear=OutYear,Omonth=OutMonth,Ocamera="north east",Otype=OutType,orient=Landscape)
        Runscript("Chart_Daily_Bar.r",Oyear=OutYear,Omonth=OutMonth,Ocamera="north east",Otype=OutType,orient=Landscape)
}
## NORTH WEST AND SOUTH EAST CAMERAS
if (OutYear > "2016" | (OutYear == "2016" & month_index > 1)) {
  cat("Printing NORTH WEST and SOUTH EAST Monthly Charts ")
  Runscript("Chart_Pareto.r",Oyear=OutYear,Omonth=OutMonth,Ocamera="north west",Otype=OutType,orient=Landscape)
  Runscript("Chart_Pareto.r",Oyear=OutYear,Omonth=OutMonth,Ocamera="south east",Otype=OutType,orient=Landscape)
  Runscript("Chart_Daily_Bar.r",Oyear=OutYear,Omonth=OutMonth,Ocamera="north west",Otype=OutType,orient=Landscape)
  Runscript("Chart_Daily_Bar.r",Oyear=OutYear,Omonth=OutMonth,Ocamera="south east",Otype=OutType,orient=Landscape)
}
## Produce Monthly ALL CAMERA SUMMARIES
SelectCam <<- "ALL"
cat("Printing ALL CAMERA Monthly Charts ")
Runscript("Chart_Pareto.r",Oyear=OutYear,Omonth=OutMonth,Ocamera="ALL",Otype=OutType,orient=Landscape)
Runscript("Chart_Daily_Bar.r",Oyear=OutYear,Omonth=OutMonth,Ocamera="ALL",Otype=OutType,orient=Landscape)

## Produce YTD Charts ##########################################################################################
SelectMon <<- "YTD"
SelectCam <<- OutCamera
## NORTH AND SOUTH CAMERAS
if (OutYear <= "2016") {
  cat("Printing NORTH and SOUTH YTD Charts ")
  Runscript("Chart_Pareto.r",Oyear=OutYear,Omonth="YTD",Ocamera="north",Otype=OutType,orient=Landscape)
  Runscript("Chart_Pareto.r",Oyear=OutYear,Omonth="YTD",Ocamera="south",Otype=OutType,orient=Landscape)
}
## NORTH EAST CAMERA
if (OutYear >= "2015") {
  cat("Printing NORTH EAST YTD Charts ")
  Runscript("Chart_Pareto.r",Oyear=OutYear,Omonth="YTD",Ocamera="north east",Otype=OutType,orient=Landscape)
}
## NORTH WEST AND SOUTH EAST CAMERAS
if (OutYear >= "2016" ) {
  cat("Printing NORTH WEST and SOUTH EAST YTD Charts ")
  Runscript("Chart_Pareto.r",Oyear=OutYear,Omonth="YTD",Ocamera="north west",Otype=OutType,orient=Landscape)
  Runscript("Chart_Pareto.r",Oyear=OutYear,Omonth="YTD",Ocamera="south east",Otype=OutType,orient=Landscape)
}
## Produce YTD ALL CAMERA SUMMARIES ############################################################################
cat("Printing ALL CAMERA YTD Charts ")
Runscript("Chart_Pareto.r",Oyear=OutYear,Omonth="YTD",Ocamera="ALL",Otype=OutType,orient=Landscape)

## 

## Produce Lifetime BY CAMERA SUMMARIES ########################################################################
cat("Printing LIFETIME Charts ")
Runscript("Chart_Category_Bar.r",Oyear="ROLL",Omonth=OutMonth,Ocategory="White",Otype=OutType,orient=Landscape)
Runscript("Chart_Category_Bar.r",Oyear="ROLL",Omonth=OutMonth,Ocategory="Noise",Otype=OutType,orient=Landscape)
Runscript("Chart_Category_Bar.r",Oyear="ROLL",Omonth=OutMonth,Ocategory="Aircraft",Otype=OutType,orient=Landscape)
Runscript("Chart_Category_Bar.r",Oyear="ROLL",Omonth=OutMonth,Ocategory="Twilight",Otype=OutType,orient=Landscape)
Runscript("Chart_Category_Bar.r",Oyear="ROLL",Omonth=OutMonth,Ocategory="Weather",Otype=OutType,orient=Landscape)
Runscript("Chart_Category_Bar.r",Oyear="ROLL",Omonth=OutMonth,Ocategory="Moon",Otype=OutType,orient=Landscape)
Runscript("Chart_Category_Bar.r",Oyear="ROLL",Omonth=OutMonth,Ocategory="Spider",Otype=OutType,orient=Landscape)
Runscript("Chart_Category_Bar.r",Oyear="ROLL",Omonth=OutMonth,Ocategory="Bird",Otype=OutType,orient=Landscape)
Runscript("Chart_Category_Bar.r",Oyear="ROLL",Omonth=OutMonth,Ocategory="Bat",Otype=OutType,orient=Landscape)
Runscript("Chart_Category_Bar.r",Oyear="ROLL",Omonth=OutMonth,Ocategory="Insect",Otype=OutType,orient=Landscape)
Runscript("Chart_Category_Bar.r",Oyear="ROLL",Omonth=OutMonth,Ocategory="Meteor",Otype=OutType,orient=Landscape)
Runscript("Chart_Category_Bar.r",Oyear="ROLL",Omonth=OutMonth,Ocategory="Flash",Otype=OutType,orient=Landscape)
        
        
cat("Run complete ",format(Sys.time(), "%a %b %d %Y %H:%M:%S"))
      

    
    
    
