Move_Accepted_Clips <<- function(Year,Month,Camera,DDL,From_Folder="Meteor Watch",To_Folder="TEMPOUT") {
        #######################################################################################################
        ## Version 1
        ## This function reads the UFO data for the specified Year, Month and Camera
        ## and MOVES all "sets" of files for clips that meet the SD - cDeg Quality criteria
        ##
        ## Here's the theory ...
        ## 1. First process the month's data using UFO Analyse All with DDL= 5 etc. This associates profiles 
        ##    with each clip making what follows possible
        ##
        ## 2. Run this script to strip out the clips that have passed muster
        ##
        ## 3. Rerun UFO Analyse All with a different value of DDL (the value to be taken from an analysis 
        ##    of the frequency of DDL matches ... say 8,12,15,18,21,24,3,2,1 ... or something similar)
        ##
        ## 4. Rerun this script to strip out any clips that have passed muster this time
        ##
        ## 5. Check the input data to see how many clips remain to be processed (this should probably be
        ##    done by script). 
        ##
        ## 6. If there are still many remaining, repeat steps 3,4,5 until only the stubborn ones remain.
        ##    They will have to be processed individually ... manually of course.
        ##
        ## 7. Merge the two sets of data back into a single file structure (maybe another script using
        ##    file.rename again)
        ##
        ## sample call: Move_Accepted_Clips(2016,"Dec","NORTH WEST","5","MeteorsIn","MeteorsOut")
        ## or for final run ...
        ## sample call: Move_Accepted_Clips(2016,"Dec","NORTH WEST","LAST","MeteorsIn","MeteorsOut")
        #######################################################################################################
  cat("Processing started",format(Sys.time(), "%a %b %d %Y %H:%M:%S")) 
  cat("")
  ## Set Working Directory
  #setwd("D:/Meteor Watch/UFO/UFOData")
  x <<- paste("D:/MeteorsIn/UFO/UFOData/",sep="")
  setwd(x)
  
  ## Set libraries
  library(dplyr)
  library(lubridate)
  library(XML)
  
  ##############################################################################################################
  Invoke_Read <<- function(data_path,newdata,newtxtdata) {
    ## Invoke_Read ###############################################################################################        
    nxframe <<- data.frame()
    sxframe <<- data.frame()
    nsxframe <<- data.frame ()
    ntframe <<- data.frame()
    stframe <<- data.frame()
    nstframe <<- data.frame ()
    
    ## call Read_in_XML_Data to get requested data
    nsxframe <<- Read_in_XML_Data(data_path,newdata)
    newdata <<- rbind(newdata,nsxframe)
    
    ## call Read_in_txt_Data to get requested data
    nstframe <<- Read_in_txt_Data(data_path,newtxtdata)
    newtxtdata <<- rbind(newtxtdata,nstframe)
    
  } ## end of function Invoke_Read ############################################################################
        
  ############################################################################################################
  Read_in_XML_Data <<- function(data_path,newdata) {
    ## Read in XML Data ########################################################################################
    
    ## first setup 'listlen2' as an integer vector indicating the number of files to be read in
    file_list <<- list.files(path = data_path, full.names = TRUE)       ## obtain full names of files to be read
    file_list2 <<- list.files(path = data_path)                         ## obtain short names of files to be read
    listlen2 <<- length(file_list2)
    if (listlen2 == 0) {
      #print(paste("WARNING: No XML data available for path: ",data_path))
      rxframe <<- data.frame()
    }
    else {
      file_list3 <<- NULL
      for (c in 1: listlen2) {
        if (grepl("NWA.xml$", file_list2[c], ignore.case=TRUE) ) {        
          file_list3 <<- c(file_list3,file_list2[c])
        }
        if (grepl("SEA.xml$", file_list2[c], ignore.case=TRUE) ) {        
          file_list3 <<- c(file_list3,file_list2[c])
        }
        if (grepl("NEA.xml$", file_list2[c], ignore.case=TRUE) ) {        
          file_list3 <<- c(file_list3,file_list2[c])
        }
        
      }
      
      xdata <<- data.frame()                                          ## initialise data frame 'data'
      rxframe <<- data.frame()
      
      ## listlen3 is an integer vector denoting the number of input files selected for processing
      ## file_list3 is a character vector containing the names of the files to be read
      listlen3 <<- length(file_list3)
      if (listlen3 == 0) {
        # Block Warning # print(paste("WARNING: No XML analysis data available for path: ",data_path))
      }
      else {
        for (i in 1:listlen3) {                                 ## loop for specified number of id's
          disc_file <<- paste(data_path,file_list3[i],sep="")
          root_xml <<- xmlToList(xmlTreeParse(disc_file,useInternalNodes=TRUE))
          root_data <<- root_xml$.attrs
          clipname <<- as.character(root_data["clip_name"])
          object_xml <<- xmlToList(xmlTreeParse(disc_file,useInternalNodes=TRUE))
          analysis_data <<- object_xml$ua2_objects$ua2_object$.attrs
          
          cdegval <<- as.numeric(analysis_data["cdeg"])
          
          ## bring required data together
          xxdata <<- c("path"= data_path,"clip"=clipname,"ddl"=DDL,"cdeg"=as.numeric(cdegval))
          
          xdata <<- rbind(xdata,xxdata)
          colnames(xdata) <<- c("path","clip","ddl","cdeg")
          
           rxframe <<- rbind(rxframe,xdata)                  ## bind 'data' onto end of 'rxframe'
          
          xdata <<- data.frame()                          ## reinitialise data frame 'xdata'
        }                                                       ## end of loop
      } ## end of else listlen3 != 0
      
    } ## end of else listlen2 != 0
    rxframe
  } ## end of function Read in XML Data ########################################################################

  ############################################################################################################
  Read_in_txt_Data <<- function(data_path,newtxtdata) {
    ## Read in txt Data ########################################################################################
    
    ## first setup 'listlen2' as an integer vector indicating the number of files to be read in
    file_list <<- list.files(path = data_path, full.names = TRUE)       ## obtain full names of files to be read
    file_list2 <<- list.files(path = data_path)                         ## obtain short names of files to be read
    listlen2 <<- length(file_list2)                                     ## listlen2 = no of files in day folder        
    if (listlen2 == 0) {
      #print(paste("WARNING: No txt data available for path: ",data_path))
      rtframe <<- data.frame()
    }
    else {
      file_list3 <<- NULL                                         ## listlen3 = no of txt files in day folder        
      for (c in 1: listlen2) {
        if (grepl("NW.txt$", file_list2[c], ignore.case=TRUE) ) {        
          file_list3 <<- c(file_list3,file_list2[c])
        }
        if (grepl("SE.txt$", file_list2[c], ignore.case=TRUE) ) {        
          file_list3 <<- c(file_list3,file_list2[c])
        }
        if (grepl("NE.txt$", file_list2[c], ignore.case=TRUE) ) {        
          file_list3 <<- c(file_list3,file_list2[c])
        }
      }
      
      tdata <<- data.frame()                                          ## initialise data frame 'data'
      rtframe <<- data.frame()
      
      ## listlen3 is an integer vector denoting the number of input files selected for processing
      ## file_list3 is a character vector containing the names of the files to be read
      listlen3 <<- length(file_list3)
      if (listlen3 == 0) {
        # Block Warning # print(paste("WARNING: No txt analysis data available for path: ",data_path))
      }
      else {
        for (i in 1:listlen3) {                                 ## loop for specified number of id's
          text_file <<- paste(data_path,file_list3[i],sep="")
          Txt <<- readLines(text_file,-1L)
          txt_len <<- length(Txt)
          clipname <<- as.character(substring(Txt[1],3,31)) ## improve with grepl ?
          SDval <<- 99                                      ## default to 99 in case no *fno row exists
          for (t in 2: txt_len) {
            if (grepl("\\*fno=", Txt[t], ignore.case=TRUE) &
                grepl("sd=", Txt[t], ignore.case=TRUE)) {
              
              SDstring <<- Txt[t]
              SDval <<- sub(") \\*","",sub(".*sd=","",SDstring))
            }
          }
          
          ## bring required data together
          ttdata <<- c("path"= data_path,"clip"= clipname,"SD"= as.numeric(SDval))
          ##print(c("ttdata",ttdata))
          tdata <<- rbind(tdata,ttdata)
          colnames(tdata) <<- c("path","clip","SD")
          ##print (c("tdata",tdata))
          rtframe <<- rbind(rtframe,tdata)                ## bind 'data' onto end of 'rtframe'
          tdata <<- data.frame()                          ## reinitialise data frame 'tdata'
        }                                                       ## end of loop
      } ## end of else listlen3 != 0
      
    } ## end of else listlen2 != 0
    rtframe
  } ## end of function Read in txt Data ########################################################################
  
    
  ##############################################################################################################
  ##                                    Start of Main logic
  ##############################################################################################################
  
  ## setup vectors of permitted values
  valid_years <<- c(2014,2015,2016,2017)
  valid_months <<- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  valid_cameras <<- c("NORTH WEST", "SOUTH EAST", "NORTH EAST")
  num_df <<- c("01","02","03","04","05","06","07","08","09","10","11","12")
  dom_df <<- c("31","29","31","30","31","30","31","31","30","31","30","31")
  day_df <<- c("01","02","03","04","05","06","07","08","09","10",
               "11","12","13","14","15","16","17","18","19","20", 
               "21","22","23","24","25","26","27","28","29","30","31") 
  
  ## Extract Current Date #####################################################################################
  Curr_Month <<- as.numeric(month(today()))
  Curr_Year <<- as.numeric(year(today()))
  ## end Extract Current Date #################################################################################
  
  ## Validate Year ############################################################################################
  year_len <<- length(valid_years)
  y <<- 0
  y_valid <<- FALSE
  while (y < year_len) {
    y <<- y + 1
    if (as.character(Year) != valid_years[y]) { 
      ## no match found
    }
    else {
      ## match found
      y_valid <<-- TRUE
      y <<- year_len
    }        
  }
  if (!y_valid) {
    stop("Invalid Year")
  }
  if (Year != "ALL" & Year > Curr_Year)
    stop("Invalid Future Year")
  ## end Validate Year ########################################################################################
  
  ## Validate Month ###########################################################################################
  Months_len <<- length(valid_months)
  d <<- 0
  d_valid <<- FALSE
  while (d < Months_len) {
    d <<- d + 1
    if (as.character(Month) != valid_months[d]) { 
      ## no match found
    }
    else {
      ## match found
      d_valid <<-- TRUE
      month_index <<- d
      d <<- Months_len
    }        
  }
  if (!y_valid) {
    stop("Invalid Month")
  }
  ## end Validate Month #######################################################################################
  
  ## Validate Camera ##########################################################################################
  camera_len <<- length(valid_cameras)
  cam <<- 0
  c_valid <<- FALSE
  while (cam < camera_len) {
    cam <<- cam + 1
    if (Camera != valid_cameras[cam]) { 
      ## no match found
    }
    else {
      ## match found
      c_valid <<-- TRUE
      cam <<- camera_len
    }        
  }
  if (!c_valid) {
    stop("Invalid Camera")
  }
  ## end Validate Camera ######################################################################################
  
  ## define a data.frame called Passed_Clips
  Passed_Clips <<- data.frame()
  
  #############################################################################################################
  ## Setup paths required to read data
  
  ## start with main path
  nw_path <<- paste("D:/",From_Folder,"/UFO/UFOData/",sep="")
  se_path <<- paste("D:/",From_Folder,"/UFO/UFOData/",sep="")
  ne_path <<- paste("D:/",From_Folder,"/UFO/UFOData/",sep="")
  
  ## add camera folder
  nw_path <<- paste(nw_path, "NORTH WEST/", sep = "")
  se_path <<- paste(se_path, "SOUTH EAST/", sep = "")
  ne_path <<- paste(ne_path, "NORTH EAST/", sep = "")
  
  ## add year folder
  nw_path <<- paste(nw_path,Year,"/",sep = "")
  se_path <<- paste(se_path,Year,"/",sep = "") 
  ne_path  <<- paste(ne_path,Year,"/",sep = "") 
  
  ## add month folder and invoke functions to read required data
  newdata <<- data.frame()                                                ## initialise data frame 'newdata'
  newtxtdata <<- data.frame()                                                ## initialise data frame 'newtxtdata'
  
  mmax <<- length(valid_months)
  for (mm in 1:mmax) {
    if(tolower(Month) == tolower(valid_months[mm])) {
      numeric_month <<- num_df[mm]
      number_of_days <<- dom_df[mm]
      mm <<- length(valid_months)
    }
  }
  if (nw_path != "") {
    nw_path <<- paste(nw_path,Year,numeric_month,"/",sep = "")
  }
  if (se_path != "") {
    se_path <<- paste(se_path,Year,numeric_month,"/",sep = "")        
  }
  if (ne_path != "") {
    ne_path <<- paste(ne_path,Year,numeric_month,"/",sep = "")        
  }
  

  if (Camera == "NORTH WEST" ) {
    for(Next_Day in 1:number_of_days) {
      daily_path <<- nw_path                                        ## this is required to reinitialise path
      if (nw_path != "") {
        daily_path <<- paste(nw_path,Year,numeric_month,day_df[Next_Day],"/",sep = "")
      }      
      data_path <<- daily_path
      Invoke_Read(data_path,newdata,newtxtdata)                     ## read data for requested month
    }
  }

  if (Camera == "SOUTH EAST" ) {
    for(Next_Day in 1:number_of_days) {
      daily_path <<- se_path                                        ## this is required to reinitialise path
      if (se_path != "") {   
        daily_path <<- paste(se_path,Year,numeric_month,day_df[Next_Day],"/",sep = "")        
      }
      data_path <<- daily_path
      ##newdata <<- 
      Invoke_Read(data_path,newdata,newtxtdata)                     ## read data for requested month
    }
  }

  if (Camera == "NORTH EAST" ) {
    for(Next_Day in 1:number_of_days) {
      daily_path <<- ne_path                                        ## this is required to reinitialise path
      if (ne_path != "") {
        daily_path <<- paste(ne_path,Year,numeric_month,day_df[Next_Day],"/",sep = "")
      }      
      data_path <<- daily_path
      Invoke_Read(data_path,newdata,newtxtdata)                     ## read data for requested month
    }
  }
  print(paste("Number of clips found for processing: ",dim(newdata)[1],sep=""))
  
  Merged_Clips <<- merge(newdata,newtxtdata, by.x = c("path","clip"), by.y = c("path","clip"))
  
  if (DDL != "LAST") { 
      Passed_Clips <<- filter(Merged_Clips,as.numeric(as.character(SD)) < 0.3 
                            & as.numeric(as.character(cdeg)) < 0.02)
  }
  ## force the copy of any outstanding clips to the output folder tree
  else { 
    Passed_Clips <<- Merged_Clips
  }
 
 ## Write out Passed_Clips for DDL reporting  / verification
 PathDir <<- "D:/R Libraries/MeteorData/"  
 write.csv(Passed_Clips, file = paste(PathDir,Year,Month,Camera,DDL, " Passed Clips Report.csv",sep=""), row.names=FALSE)
 
  ## For each row of Passed_Clips     ==========================================================
 DIMPC <<- dim(Passed_Clips)
 ROWPC <<- DIMPC[1]
 ## Move the set of seven files for this clip to a new file structure, creating the folders
 ## whenever necessary. (the move is done using file.rename)

 ########################################################################
 ## first setup the output paths
 ######################################################################## 
 Passed_Out_Clips <<- as.data.frame(lapply(Passed_Clips,
          function(x) if(is.character(x)|is.factor(x)) gsub(From_Folder,To_Folder,x) else x))
 
 ########################################################################
 ## this function performs the rename , creating folders if required
 ########################################################################
 my.file.rename <- function(from, to) {
   todir <<- dirname(to)
   if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
   file.rename(from = from,  to = to)
 }
 ########################################################################
 
 for (i in 1:ROWPC) {
   ## 
   from1 <<- paste(Passed_Clips$path[i],Passed_Clips$clip[i],".avi",sep="")
   to1   <<- paste(Passed_Out_Clips$path[i],Passed_Out_Clips$clip[i],".avi",sep="") 
   my.file.rename(from = from1,to1)
   from2 <<- paste(Passed_Clips$path[i],Passed_Clips$clip[i],".txt",sep="")
   to2   <<- paste(Passed_Out_Clips$path[i],Passed_Out_Clips$clip[i],".txt",sep="") 
   my.file.rename(from = from2,to2)
   from3 <<- paste(Passed_Clips$path[i],Passed_Clips$clip[i],".xml",sep="")
   to3   <<- paste(Passed_Out_Clips$path[i],Passed_Out_Clips$clip[i],".xml",sep="") 
   my.file.rename(from = from3,to3)
   from4 <<- paste(Passed_Clips$path[i],Passed_Clips$clip[i],"A.XML",sep="")
   to4   <<- paste(Passed_Out_Clips$path[i],Passed_Out_Clips$clip[i],"A.XML",sep="") 
   my.file.rename(from = from4,to4)
   from5 <<- paste(Passed_Clips$path[i],Passed_Clips$clip[i],"M.bmp",sep="")
   to5   <<- paste(Passed_Out_Clips$path[i],Passed_Out_Clips$clip[i],"M.bmp",sep="") 
   my.file.rename(from = from5,to5)
   from6 <<- paste(Passed_Clips$path[i],Passed_Clips$clip[i],"P.jpg",sep="")
   to6   <<- paste(Passed_Out_Clips$path[i],Passed_Out_Clips$clip[i],"P.jpg",sep="") 
   my.file.rename(from = from6,to6)
   from7 <<- paste(Passed_Clips$path[i],Passed_Clips$clip[i],"T.jpg",sep="")
   to7   <<- paste(Passed_Out_Clips$path[i],Passed_Out_Clips$clip[i],"T.jpg",sep="") 
   my.file.rename(from = from7,to7)
 }
 print(paste("Number of clips remaining for processing ",(dim(newdata)[1]-ROWPC),sep=""))
 ## End For each row of Passed_Clips ==========================================================
 cat("Processing completed",format(Sys.time(), "%a %b %d %Y %H:%M:%S")) 
}        