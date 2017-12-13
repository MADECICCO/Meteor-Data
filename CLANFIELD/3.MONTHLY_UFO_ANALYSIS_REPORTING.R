#=============================================================================
#
#-- Author: Steve Bosley - HAG, UKMON, NEMETODE
#
#
#-- Description:
#
#   This script runs a set of R scripts which generate tables and reports
#   from the monthly Output files created at the end of a UFO Analysis cycle  
#
#   This script prompts the user for the output type (PDF, JPEG or CONSOLE), 
#   a reporting year and month.  It then ...
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
  
# Read the UFO Analysis Monthly .csv files from the original tree structure 
# The logic is Clanfield specific but could be easily modified - it is the closest I have to the UKMON
# R Reporting Suite
    

    # Select which  year / month to process
    
        OutYear <<- get_year()
        OutMonth <<- get_month()
        OutCamera <<- get_camera()
        OutLimit <<- 5
        
        SelectYr <<- OutYear
        SelectMon <<- OutMonth
        SelectCam <<- OutCamera
        
        SelectMonN <<- convert_month()
        
        ## Read in all required Monthly UFO Analyser csv files
        Runscript("Read and Enrich CSV Files.r",Oyear=OutYear,Omonth=SelectMon,Ocamera="ALL",Otype=OutType,orient=Landscape)
        ## Produce Monthly Analysis Tables
        Runscript("Write_Top_5.r",Oyear=OutYear,Omonth=SelectMonN,Ocamera="ALL",Oshower="",Otype=OutType,orient=Landscape)
        Runscript("Write_Fireballs.r",Oyear=OutYear,Omonth=SelectMon,Ocamera="ALL",Otype=OutType,orient=Landscape)
        Runscript("Write_Shower_summary.r",Oyear=OutYear,Omonth=SelectMon,Ocamera="ALL",Otype=OutType,orient=Landscape)
        Runscript("Write_Active_Showers.r",Oyear=OutYear,Omonth=SelectMon,Ocamera="ALL",Olimit=5,Otype=OutType,orient=Landscape)
        
        ## Read in all required YTD UFO Analyser csv files  *** TEMP FIX FOR YTD ISSUE ***
        Runscript("Read and Enrich CSV Files.r",Oyear=OutYear,Omonth="ALL",Ocamera="ALL",Otype=OutType,orient=Landscape)
        Runscript("Write_Top_5.r",Oyear=OutYear,Omonth="YTD",Ocamera="ALL",Oshower="",Otype=OutType,orient=Landscape)
        Runscript("Write_Fireballs.r",Oyear=OutYear,Omonth="YTD",Ocamera="ALL",Otype=OutType,orient=Landscape)
        Runscript("Write_Shower_summary.r",Oyear=OutYear,Omonth="YTD",Ocamera="ALL",Otype=OutType,orient=Landscape)
        
        cat("Reporting complete",format(Sys.time(), "%a %b %d %Y %H:%M:%S"))
      

    
    
    
