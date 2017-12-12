#=============================================================================
#
#-- Author: Steve Bosley - HAG, UKMON, NEMETODE
#
#
#-- Description:
#
#   This script runs a set of R scripts which generate tables and reports
#   from the monthly analysis sheets used to optimise our UFO Analyser data.  
#
#   This script prompts the user for the output type (PDF, JPEG or CONSOLE), 
#   a stream name and year.  It then filters and standardises UFO orbit data
#   before calling the plot / table routines.
#
#   Each script can use the following data prepared by this master script:
#
#   CategoryData:  The selected remote monitoring data
#   Dataset:      A desciptive title printed on the plot footer
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
#   #####################################################################################
#   NB. BEFORE RUNNING THIS SCRIPT THE SCRIPT "WRITE_CONSOLIDATA.R" NEEDS TO BE UPDATED #
#   TO ADD THE DATA FILE FOR THE NEW MONTH !!!                                          #
#   #####################################################################################
#
#-- Version history
#
#   Vers  Date          Notes
#   ----  ----          -----
#   1.0   30/11/2016    First release
#
#=============================================================================

cat("Reporting started",format(Sys.time(), "%a %b %d %Y %H:%M:%S"))

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
        
SelectMonN <<- convert_month()
#print(paste(SelectMon,SelectMonN,sep=""))
        
## Read in all required Analysis Spreadsheets - This script creates Cleandata which is used by all the following scripts
Runscript("Write_Consolidata.r",NA,NA,NA,Otype=OutType,orient=Landscape)

## Produce Monthly Analysis Tables
## NORTH AND SOUTH CAMERAS
if (OutYear < "2016" | (OutYear == "2016" & OutMonth == "Jan")) {
  cat("Printing NORTH and SOUTH Monthly Charts")
  Runscript("Write_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NO",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Write_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="SO",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NO",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="SO",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_Scatter_SD_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NO",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_Scatter_SD_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="SO",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_Mag_Spread.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NO",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_Mag_Spread.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="SO",Ocategory=" ",Otype=OutType,orient=Landscape)
}
## NORTH EAST CAMERA
if (OutYear > "2015" | (OutYear == "2015" & SelectMonN > 3)) {
  cat("Printing NORTH EAST Monthly Charts")
  Runscript("Write_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NE",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NE",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_Scatter_SD_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NE",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_Mag_Spread.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NE",Ocategory=" ",Otype=OutType,orient=Landscape)
}
## NORTH WEST AND SOUTH EAST CAMERAS
if (OutYear > "2016" | (OutYear == "2016" & SelectMonN > 1)) {
  cat("Printing NORTH WEST and SOUTH EAST Monthly Charts")
  Runscript("Write_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NW",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Write_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="SE",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NW",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="SE",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_Scatter_SD_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NW",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_Scatter_SD_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="SE",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_Mag_Spread.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NW",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_Mag_Spread.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="SE",Ocategory=" ",Otype=OutType,orient=Landscape)
}
## Produce Monthly ALL CAMERA SUMMARIES
SelectCam <<- "ALL"
cat("Printing ALL CAMERA Monthly Charts")
Runscript("Write_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="ALL",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="ALL",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_Scatter_SD_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="ALL",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_Mag_Spread.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="ALL",Ocategory=" ",Otype=OutType,orient=Landscape)

## Produce YTD Charts ##########################################################################################
SelectMon <<- "YTD"
SelectMonN <<- 14
SelectCam <<- OutCamera
# NORTH AND SOUTH CAMERAS
if (OutYear <= "2016") {
  cat("Printing NORTH and SOUTH YTD Charts")
  Runscript("Write_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NO",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Write_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="SO",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NO",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="SO",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_Scatter_SD_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NO",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_Scatter_SD_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="SO",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_Mag_Spread.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NO",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_Mag_Spread.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="SO",Ocategory=" ",Otype=OutType,orient=Landscape)
}
## NORTH EAST CAMERA
if (OutYear >= "2015") {
  cat("Printing NORTH EAST YTD Charts")
  Runscript("Write_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NE",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NE",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_Scatter_SD_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NE",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_Mag_Spread.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NE",Ocategory=" ",Otype=OutType,orient=Landscape)
}
## NORTH WEST AND SOUTH EAST CAMERAS
if (OutYear >= "2016") {
  cat("Printing NORTH WEST and SOUTH EAST YTD Charts")
  Runscript("Write_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NW",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Write_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="SE",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NW",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="SE",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_Scatter_SD_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NW",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_Scatter_SD_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="SE",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_Mag_Spread.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="NW",Ocategory=" ",Otype=OutType,orient=Landscape)
  Runscript("Chart_Mag_Spread.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="SE",Ocategory=" ",Otype=OutType,orient=Landscape)
}
## Produce YTD ALL CAMERA SUMMARIES
SelectCam <<- "ALL"
cat("Printing ALL CAMERA YTD Charts")
Runscript("Write_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="ALL",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_SD_V_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="ALL",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_Scatter_SD_CDEG.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="ALL",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_Mag_Spread.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="ALL",Ocategory=" ",Otype=OutType,orient=Landscape)

## Produce Lifetime SUMMARIES ##################################################################################
SelectYr <<- "ALL"
SelectMon <<- "ALL"
SelectMonN <<- 13
SelectCam <<- OutCamera
cat("Printing LIFETIME Charts")
Runscript("Write_SD_V_CDEG.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="ALL",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Write_SD_V_CDEG.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="NO",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Write_SD_V_CDEG.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="NW",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Write_SD_V_CDEG.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="NE",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Write_SD_V_CDEG.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="SO",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Write_SD_V_CDEG.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="SE",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_SD_V_CDEG.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="ALL",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_SD_V_CDEG.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="NO",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_SD_V_CDEG.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="NW",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_SD_V_CDEG.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="NE",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_SD_V_CDEG.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="SO",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_SD_V_CDEG.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="SE",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_Scatter_SD_CDEG.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="ALL",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_Scatter_SD_CDEG.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="NO",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_Scatter_SD_CDEG.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="NW",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_Scatter_SD_CDEG.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="NE",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_Scatter_SD_CDEG.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="SO",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_Scatter_SD_CDEG.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="SE",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_Mag_Spread.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="ALL",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_Mag_Spread.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="NO",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_Mag_Spread.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="NW",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_Mag_Spread.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="NE",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_Mag_Spread.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="SO",Ocategory=" ",Otype=OutType,orient=Landscape)
Runscript("Chart_Mag_Spread.r",Oyear="ALL",Omonth=SelectMonN,Ocamera="SE",Ocategory=" ",Otype=OutType,orient=Landscape)

cat("Reporting complete",format(Sys.time(), "%a %b %d %Y %H:%M:%S"))
      

    
    
    