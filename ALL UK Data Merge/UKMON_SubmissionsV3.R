UKMON_Submissions <<- function() {
        ##
        ## This function reads in a consolidated .csv file of UKMON data and creates a table of the number
        ## of records each camera has submitted, per month
        ## Depending on whether or not this is run on the same day as the data download, lines 26/27 should be flipped
        
        ## Set Working Directory
        setwd("D:/R Libraries/MeteorData/R UKMON Reports/")
        
        ## Set libraries
        library(dplyr)
        library(lubridate)
        
        #############################################################################################################
        ## Start of Main processing
        #############################################################################################################
        
        ## Extract Current Date #####################################################################################
        Curr_Day <<- as.numeric(day(today()))
        Curr_Month <<- as.numeric(month(today()))
        Curr_Year <<- as.numeric(year(today()))
        ## end Extract Current Date #################################################################################
        
        ALL_Data <<- read.csv(paste("UKMON Unique Consolidated Data_",Curr_Year,Curr_Month,Curr_Day, ".csv",sep=""))
        #ALL_Data <<- read.csv(paste("UKMON Unique Consolidated Data_2017216.csv",sep=""))
        No_of_Meteors <<- dim(ALL_Data)[1]
        
        Selection <<- select(ALL_Data,Loc_Cam)
        Cameras <<- unique(Selection)
        No_of_Cameras <<- dim(Cameras)[1]
        
        for (i in 1:72) {
                Cameras <<- cbind(Cameras,"Header" = as.numeric(0))
        }
        Columns <<- c("Camera","20121","20122","20123","20124","20125","20126","20127","20128","20129","201210","201211","201212",
                               "20131","20132","20133","20134","20135","20136","20137","20138","20139","201310","201311","201312",
                               "20141","20142","20143","20144","20145","20146","20147","20148","20149","201410","201411","201412",
                               "20151","20152","20153","20154","20155","20156","20157","20158","20159","201510","201511","201512",
                               "20161","20162","20163","20164","20165","20166","20167","20168","20169","201610","201611","201612",
                               "20171","20172","20173","20174","20175","20176","20177","20178","20179","201710","201711","201712")
        colnumbers <<- c(1:73)
        colnames(Cameras) <<- Columns
        
        for (m in 1:No_of_Meteors) {
                Meteor <<- ALL_Data[m,]
                Location <<- Meteor$Loc_Cam
                ## sort out this month end year end logic later !
                #if (Meteor$D.UT. > 1) {
                #        Period <<- paste(Meteor$Y.UT.,Meteor$M.UT.,sep="")
                #}
                #else {
                #        Period <<- paste(Meteor$Y.UT.,Meteor$M.UT.-1,sep="")  
                #}
                Period <<- paste(Meteor$Y.UT.,Meteor$M.UT.,sep="")
                
                for(p in 1:73) {
                        if(Period == Columns[p]) {
                                colindex <<- p
                                p <<- 73
                        }
                }
                for (cams in 1:No_of_Cameras) {
                        if (as.character(Location) == as.character(Cameras[cams,1])) {
                                Cameras[cams,colindex] <<- Cameras[cams,colindex] +1
                                cams <<- No_of_Cameras
                        }
                }
                
        }
        write.csv(Cameras,file = paste("UKMON Events Submitted per Camera per Month.csv",sep=""), quote=FALSE,row.names=FALSE)
        
        
        #############################################################################################################
        ## End of Main processing
        #############################################################################################################        
        

        #######################################################################################################
        ## CHANGE LOG
        ## 30 Mar 2016
        ## Initial version  
        ########################################################################################################
        
}