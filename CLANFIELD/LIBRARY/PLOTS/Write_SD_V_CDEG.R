#Write_SD_V_CDEG <<- function(Cam="",Yr="",Mnth="") {
{
        #######################################################################################################
        ## Version 1
        ## This function takes the consolidated data for all cameras and assesses the
        ## quality of the data according to the requested filters
        #######################################################################################################
        
        YearO <<- Oyear
        MonthO <<- Omonth
        CameraO <<- Ocamera
        
        All11Data <<- CleanData
        if (dim(All11Data)[1] == 0) {
          stop("Filter 1 has reduced data to zero rows")
        }

        All11Data <<- mutate (All11Data,Date = substr(Clip_Name,2,9))
        All11Data <<- mutate (All11Data,Year = substr(Date,1,4))
        All11Data <<- mutate (All11Data,Month = substr(Date,5,6))
        All11Data <<- mutate (All11Data,Day = substr(Date,7,8))
        All11Data <<- mutate (All11Data,Time = substr(Clip_Name,11,16))
        All11Data <<- mutate (All11Data,Hour = substr(Time,1,2))
        All11Data <<- select (All11Data,-DDL,-Leap,-F1,-F2,-Resolved)
        All11Data <<- arrange(All11Data,Date)
        print(paste("Write_SD_V_CDEG line 18",YearO,MonthO,CameraO,sep=" : "))
        ##Apply filters if any specified
        if (CameraO != "ALL") {
                All11Data <<- filter(All11Data,Camera == CameraO)
                if (dim(All11Data)[1] == 0) {
                  stop("Filter 2 has reduced data to zero rows")
                }
        }
        Nyear <<- as.integer(YearO)
        Nmonth <<- as.integer(MonthO)
        if (Nmonth > 12) {
          Nmonth <<- 12
          MonthO <<- "ALL"
        }
        if (nchar(MonthO) == 1) {
          MonthO <<- paste("0",MonthO,sep="")
          print(paste("Write_SD_V_CDEG line 42","MonthO = ",MonthO,sep=" : "))
        }
        if (YearO != "ALL" & MonthO != "ALL") {
                print(paste("Write_SD_V_CDEG line 33",YearO,MonthO,sep=" : "))
                if(MonthO == "12") {
                  
                  ## Include all records for selected year and month plus first half of first day of Jan. next year
                  All11Data <<- filter(All11Data, (Year == YearO & Month == MonthO) | 
                            (Year == as.character(Nyear+1) & Month == "01" & Day == "01" & as.integer(Hour) < 12))
                  if (dim(All11Data)[1] == 0) {
                    stop("Filter 3 has reduced data to zero rows")
                  }
                  ## Filter out first half of Day 1 which were processed as November          
                  All11Data <<- filter(All11Data, !(Year == YearO & Month == MonthO & Day == "01" & as.integer(Hour) < 12))          
                  if (dim(All11Data)[1] == 0) {
                    stop("Filter 4 has reduced data to zero rows")
                  }
                }
                else {
                  
                  ## Select records for chosen year
                  All11Data <<- filter(All11Data,Year == YearO)
                  if (dim(All11Data)[1] == 0) {
                    stop("Filter 5 has reduced data to zero rows")
                  }
                  ## select records for chosen month and the following one
                  print(paste("Write_SD_V_CDEG line 67",All11Data[1,]$Month,MonthO,sep=" : "))
                  All11Data <<- filter(All11Data,Month == MonthO | Month == as.character(Nmonth+1))
                  if (dim(All11Data)[1] == 0) {
                    stop("Filter 6 has reduced data to zero rows")
                  }
                  ## Filter out first half of Day 1 of selected month which were processed as the previous month          
                  All11Data <<- filter(All11Data, !(Month == MonthO & Day == "01" & as.integer(Hour) < 12))
                  if (dim(All11Data)[1] == 0) {
                    stop("Filter 7 has reduced data to zero rows")
                  }
                  ## Filter out all bar the first half of Day 1 of following month which will be processed with this month
                  All11Data <<- filter(All11Data, !(Month == as.character(Nmonth+1) & Day == "01" & as.integer(Hour) >= 12)
                                                    | (Month == as.character(Nmonth+1) & as.integer(Day) >  1))
                  if (dim(All11Data)[1] == 0) {
                    stop("Filter 8 has reduced data to zero rows")
                  }
                }
               
        }
        ## For a specific year
        if (YearO != "ALL" & MonthO == "ALL") {
          
            All11Data <<- filter(All11Data,Year == YearO)
            if (dim(All11Data)[1] == 0) {
              stop("Filter 9 has reduced data to zero rows")
            }
            
          }
        ## For all years no further filtering is necessary   
        
        
        Both <<- filter(All11Data,SD <= 0.3 & cDeg <= 0.02)
        SD_Only <<- filter(All11Data,SD <= 0.3 & cDeg >  0.02)
        cDeg_Only <<- filter(All11Data, SD > 0.3 & cDeg <= 0.02)
        Neither <<- filter(All11Data,SD > 0.3 & cDeg > 0.02)
        
        CountA <<- dim(All11Data)[1]
        CountB <<- dim(Both)[1]
        CountS <<- dim(SD_Only)[1]
        CountC <<- dim(cDeg_Only)[1]
        CountN <<- dim(Neither)[1]
        
        PercentB <<- round(CountB / CountA * 100 ,1)
        PercentS <<- round(CountS / CountA * 100 ,1)
        PercentC <<- round(CountC / CountA * 100 ,1)
        PercentN <<- round(CountN / CountA * 100 ,1)
        
        Quality <<- data.frame(Numer_of_Meteor_Events=integer(),Criteria=character(),Percentage=numeric())
        Single_Row <<- data.frame(Number_of_Meteor_Events=CountB,Criteria="Pass UKMON : Pass Nemetode",Percentage=PercentB)
        Quality <<- rbind(Quality,Single_Row)
        Single_Row <<- data.frame(Number_of_Meteor_Events=CountS,Criteria="Pass UKMON : Fail Nemetode",Percentage=PercentS)
        Quality <<- rbind(Quality,Single_Row)
        Single_Row <<- data.frame(Number_of_Meteor_Events=CountC,Criteria="Fail UKMON : Pass Nemetode",Percentage=PercentC)
        Quality <<- rbind(Quality,Single_Row)
        Single_Row <<- data.frame(Number_of_Meteor_Events=CountN,Criteria="Fail UKMON : Fail Nemetode",Percentage=PercentN)
        Quality <<- rbind(Quality,Single_Row)

        
        setwd(ReportDir)
        write.csv(Quality,file = paste(YearO,MonthO,CameraO,NA,"Data_Quality.csv",sep=""), row.names=FALSE)
        
}        
