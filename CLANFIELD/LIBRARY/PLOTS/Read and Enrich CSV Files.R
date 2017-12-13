#Write_Top_5 <<- function(Ocamera,Oyear,Omonth) {
{
        ##
        ## This is version 2
        ## Updated in line with 2016 shower_Summary to process new camera names
        ##
        

        ##############################################################################################################
        Invoke_Read <<- function(north_path,south_path,east_path,stae_path,nw_path,ne_path,se_path,newdata) {
        ## Invoke_Read ###############################################################################################        
        nframe <<- data.frame()
        sframe <<- data.frame()
        eframe <<- data.frame()
        nwframe <<- data.frame()
        seframe <<- data.frame()
        neframe <<- data.frame()
        staeframe <<- data.frame ()
        nseframe <<- data.frame ()
        ## call Read_in_Data to get requested data
        if (north_path != "") {
                nframe <<- Read_in_Data(north_path,newdata)
        }
        if (south_path != "") {
                sframe <<- Read_in_Data(south_path,newdata)
        }
        if (east_path != "") {
                eframe <<- Read_in_Data(east_path,newdata)
        }
        if (nw_path != "") {
                nwframe <<- Read_in_Data(nw_path,newdata)
        }
        if (se_path != "") {
                seframe <<- Read_in_Data(se_path,newdata)
        }
        if (ne_path != "") {
                neframe <<- Read_in_Data(ne_path,newdata)
        }
        if (stae_path != "") {
                staeframe <<- Read_in_Data(stae_path,newdata)
        }
                
        nseframe <<- rbind(nframe,sframe)
        nseframe <<- rbind(nseframe,eframe)
        nseframe <<- rbind(nseframe,staeframe)
        nseframe <<- rbind(nseframe,nwframe)
        nseframe <<- rbind(nseframe,neframe)
        nseframe <<- rbind(nseframe,seframe)
        if (dim(nseframe)[1] == 0 & tolower(Omonth) != "all") { 
        ## NB. month != all test avoids missing months data (eg camera outage) but does require some data in year
                stop("No data for current request")
        }
        newdata <<- rbind(newdata,nseframe)
        } ## end of function Invoke_Read ############################################################################
        
        ############################################################################################################
        Read_in_Data <<- function(data_path,newdata) {
        ## Read in Data ############################################################################################
                
        ## first setup 'listlen2' as an integer vector indicating the number of files to be read in
        file_list <<- list.files(path = data_path, full.names = TRUE)       ## obtain full names of files to be read
        file_list2 <<- list.files(path = data_path)                         ## obtain short names of files to be read
        listlen2 <<- length(file_list2)
        if (listlen2 == 0) {
                print(paste("WARNING: No data available for path: ",data_path))
                rframe <<- data.frame()
        }
        else {
                file_list3 <<- NULL
                
                for (c in 1: listlen2) {
                        if (nchar(file_list2[c]) == 33) {
                                file_list3 <<- c(file_list3,file_list2[c])
                        }
                }
                
                
                
                xdata <<- data.frame()                                          ## initialise data frame 'data'
                rframe <<- data.frame()
                
                ## listlen3 is an integer vector denoting the number of input files selected for processing
                ## file_list3 is a character vector containing the names of the files to be read
                listlen3 <<- length(file_list3)
                if (listlen3 == 0) {
                        print(paste("WARNING: No analysis data available for path: ",data_path))
                }
                else {
                        for (i in 1:listlen3) {                                 ## loop for specified number of id's
                                ## Currently this tries to read data from Meteordata folder, not normal file structure ! ## <======= OUCH !
                                fully_qualified_address <<- paste (data_path,file_list3[i],sep="")
                                xdata <<- rbind(xdata, read.csv(fully_qualified_address)) ## read next file into 'data'
                                rframe <<- rbind(rframe,xdata)                  ## bind 'data' onto end of 'newdata'
                                xdata <<- data.frame()                          ## reinitialise data frame 'data'
                        }                                                       ## end of loop
                } ## end of else listlen3 != 0
                        
        } ## end of else listlen2 != 0
        rframe
        } ## end of function Read in Data ############################################################################
        
        
        ## validate parameters against the following permitted values
        valid_cameras <<- c("ALL","NORTH","SOUTH","EAST","STAE","NE","NW","SE")
        valid_years <<- c("ALL",2012,2013,2014,2015,2016,2017)
        valid_months <<- c("ALL","YTD","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
        full_months <<- c("ALL","YTD","January","February","March","April","May","June","July",
                          "August","September","October","November","December")
        
        ## Extract Current Date #####################################################################################
        Curr_month <<- as.numeric(month(today()))
        Curr_year <<- as.numeric(year(today()))
        ## end Extract Current Date #################################################################################
        

        #print(paste(Oyear,Curr_year,Omonth,sep=" "))
        ## force YTD if ALL requested for current year
        if (Oyear == Curr_year & tolower(as.character(Omonth)) == "all") {
                Omonth <<- "YTD"
        }
        ## force ALL if YTD requested for last year
        if (Oyear < Curr_year & tolower(as.character(Omonth)) == "ytd") {
                Omonth <<- "ALL"
        }
        
        ## convert date to numeric character
        
        ## end Validate Omonth #######################################################################################
        
        
        
        
        #############################################################################################################
        ## Setup paths required to read data
        
        ## start with main path
        north_path <<- "D:/Meteor Watch/UFO/UFOData/"
        south_path <<- "D:/Meteor Watch/UFO/UFOData/"
        east_path <<- "D:/Meteor Watch/UFO/UFOData/"
        nw_path <<- "D:/Meteor Watch/UFO/UFOData/"
        se_path <<- "D:/Meteor Watch/UFO/UFOData/"
        ne_path <<- "D:/Meteor Watch/UFO/UFOData/"
        stae_path <<- "D:/Meteor Watch/UFO/UFOData/"
                
        ## add camera folder
        if (tolower(Ocamera) == "all") {
                north_path <<- paste(north_path, "NORTH/", sep = "")
                south_path <<- paste(south_path, "SOUTH/", sep = "")
                east_path <<- paste(east_path, "EAST/", sep = "")
                nw_path <<- paste(nw_path, "NORTH WEST/", sep = "")
                se_path <<- paste(se_path, "SOUTH EAST/", sep = "")
                ne_path <<- paste(ne_path, "NORTH EAST/", sep = "")
                stae_path <<- paste(stae_path, "8STAE/", sep = "")
        }
        else {
                if (tolower(Ocamera) == "north") {
                        north_path <<- paste(north_path, "NORTH/", sep = "")
                        south_path <<- ""
                        east_path <<- ""
                        nw_path <<- ""
                        se_path <<- ""
                        ne_path <<- ""
                        stae_path <<- ""
                }        
                if (tolower(Ocamera) == "south") {
                        north_path <<- ""
                        south_path <<- paste(south_path, "SOUTH/", sep = "")
                        east_path <<- ""
                        nw_path <<- ""
                        se_path <<- ""
                        ne_path <<- ""
                        stae_path <<- ""
                }
                if (tolower(Ocamera) == "east") {
                        north_path <<- ""
                        south_path <<- ""
                        east_path <<- paste(east_path, "EAST/", sep = "")
                        nw_path <<- ""
                        se_path <<- ""
                        ne_path <<- ""
                        stae_path <<- ""
                }
                if (tolower(Ocamera) == "nw") {
                        north_path <<- ""
                        south_path <<- ""
                        east_path <<- ""
                        nw_path <<- paste(nw_path, "NORTH WEST/", sep = "")
                        se_path <<- ""
                        ne_path <<- ""
                        stae_path <<- ""
                }        
                if (tolower(Ocamera) == "se") {
                        north_path <<- ""
                        south_path <<- ""
                        east_path <<- ""
                        nw_path <<- ""
                        se_path <<- paste(se_path, "SOUTH EAST/", sep = "")
                        ne_path <<- ""
                        stae_path <<- ""
                }
                if (tolower(Ocamera) == "ne") {
                        north_path <<- ""
                        south_path <<- ""
                        east_path <<- ""
                        nw_path <<- ""
                        se_path <<- ""
                        ne_path <<- ne_path <<- paste(ne_path, "NORTH EAST/", sep = "")
                        stae_path <<- ""
                }
                if (tolower(Ocamera) == "8stae") {
                        north_path <<- ""
                        south_path <<- ""
                        east_path <<- ""
                        nw_path <<- ""
                        se_path <<- ""
                        ne_path <<- ""
                        stae_path <<- paste(stae_path, "8STAE/", sep = "")
                }
        }        
                
        ## add year folder
        if (tolower(Oyear) != "all") {
                if (north_path != "") {
                        north_path <<- paste(north_path,Oyear,"/",sep = "")
                }        
                if (south_path != "") {
                        south_path <<- paste(south_path,Oyear,"/",sep = "")        
                }
                if (east_path != "") {
                        east_path <<- paste(east_path,Oyear,"/",sep = "")        
                }
                if (nw_path != "") {
                        nw_path <<- paste(nw_path,Oyear,"/",sep = "")
                }        
                if (se_path != "") {
                        se_path <<- paste(se_path,Oyear,"/",sep = "")        
                }
                if (ne_path != "") {
                        ne_path <<- paste(ne_path,Oyear,"/",sep = "")        
                }
                if (stae_path != "") {
                        stae_path <<- paste(stae_path,Oyear,"/",sep = "")        
                }
        }
        else {
                stop("Logic for all years not yet developed")
        }
        
        ## add month folder and invoke functions to read required data
        newdata <<- data.frame()                                                ## initialise data frame 'newdata'
        num_df <<- c("AL","YT","01","02","03","04","05","06","07","08","09","10","11","12")
        
        if (tolower(Omonth) != "all" & tolower(Omonth) != "ytd") {
                all_north_path <<- ""
                all_south_path <<- ""
                all_east_path <<- ""
                all_nw_path <<- ""
                all_se_path <<- ""
                all_ne_path <<- ""
                all_stae_path <<- ""
                mmax <<- length(valid_months)
                #print(class(Omonth))
                for (mm in 3:mmax) {
                        #print(paste(mm,Omonth,sep=" "))
                        if(tolower(Omonth) == tolower(valid_months[mm])) {
                                #print("match found")
                                numeric_month <<- num_df[mm]
                                mm <<- length(valid_months)
                        }
                }
                if (north_path != "") {
                        north_path <<- paste(north_path,Oyear,numeric_month,"/",sep = "")
                }        
                if (south_path != "") {
                        south_path <<- paste(south_path,Oyear,numeric_month,"/",sep = "")        
                }
                if (east_path != "") {
                        east_path <<- paste(east_path,Oyear,numeric_month,"/",sep = "")        
                }
                if (nw_path != "") {
                        nw_path <<- paste(nw_path,Oyear,numeric_month,"/",sep = "")
                }        
                if (se_path != "") {
                        se_path <<- paste(se_path,Oyear,numeric_month,"/",sep = "")        
                }
                if (ne_path != "") {
                        ne_path <<- paste(ne_path,Oyear,numeric_month,"/",sep = "")        
                }
                if (stae_path != "") {
                        stae_path <<- paste(stae_path,Oyear,numeric_month,"/",sep = "")        
                }
                newdata <<- Invoke_Read(north_path,south_path,east_path,stae_path,nw_path,ne_path,se_path,newdata)                      ## read data for requested month
                
        }
        else {
                ## Omonth = All for past years, or YTD for current year
                if (Oyear == Curr_year) {
                        if (Curr_month == as.numeric(num_df[3])) {
                                stop("No analysis data for January of current year")
                        }
                        else {
                                ## set mmax index to collect data up to end of previous calendar month
                                mmax <<- as.numeric(month(today())) -1 +2     ## plus 2 is to skip ALL and YTD entries in month array
                                }        
                }
                else {
                        mmax <<- 14                         
                }
                
                ## add month to path and pass to Invoke_Read
                all_north_path <<- north_path
                all_south_path <<- south_path
                all_east_path <<- east_path
                all_nw_path <<- nw_path
                all_se_path <<- se_path
                all_ne_path <<- ne_path
                all_stae_path <<- stae_path
                for (mm in 3:mmax) {
                        numeric_month <<- num_df[mm]
                        if (all_north_path != "") {
                                all_north_path <<- paste(north_path,Oyear,numeric_month,"/",sep = "")
                        }        
                        if (all_south_path != "") {
                                all_south_path <<- paste(south_path,Oyear,numeric_month,"/",sep = "")        
                        }
                        if (all_east_path != "") {
                                all_east_path <<- paste(east_path,Oyear,numeric_month,"/",sep = "")        
                        }
                        if (all_nw_path != "") {
                                all_nw_path <<- paste(nw_path,Oyear,numeric_month,"/",sep = "")
                        }        
                        if (all_se_path != "") {
                                all_se_path <<- paste(se_path,Oyear,numeric_month,"/",sep = "")        
                        }
                        if (all_ne_path != "") {
                                all_ne_path <<- paste(ne_path,Oyear,numeric_month,"/",sep = "")        
                        }
                        if (all_stae_path != "") {
                                all_stae_path <<- paste(stae_path,Oyear,numeric_month,"/",sep = "")        
                        }
                        newdata <<- Invoke_Read(all_north_path,all_south_path,all_east_path,all_stae_path,
                                                all_nw_path,all_ne_path,all_se_path,newdata) ## read data for each month
                }
        }
        ##newdata <<- group_by(newdata,Loc_Cam)
        ##stop("bail out")
        ## check for and remove duplicate rows (should not be any but...)
        ## unewdata <<- unique(newdata)
        ## tidy up data
        ## strip out leading character from Group ID
        newdata[,2] <<- substring(newdata$Group,2)
        DIMD <<- dim(newdata)
        ROWSD <<- DIMD[1]
        COLSD <<- DIMD[2]
        
        ## now strip out "J5_" and "J8_" by scanning for showers starting with J5 or J8
        for (j in 1:ROWSD) { 
        ##      if (newdata[j,2] >= "J5_AAA" & newdata[j,2] <= "J8_ZZZ") {
                if (grepl("^J5", newdata[j,2], ignore.case=TRUE) | grepl("^J8",newdata[j,2], ignore.case=TRUE)) {        
                        newdata[j,2] <<- substring(newdata[j,2],4)
                }
        }
        


        #######################################################################################################
        ## CHANGE LOG
        ## 6 Mar 2016
        ## Initial version split out from Chart_Perseids V2 
        ########################################################################################################
        
}
