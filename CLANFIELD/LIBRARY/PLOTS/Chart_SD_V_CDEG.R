#Chart_SD_V_CDEG <<- function() {
{
        #######################################################################################################
        ## Version 1
        ## This function takes the consolidated files for each camera and charts the
        ## quality of the data
        #######################################################################################################
        
        YearO <<- Oyear
        MonthO <<- Omonth
        CameraO <<- Ocamera
  
        All12Data <<- CleanData
        
        if (CameraO != "ALL") {
          All12Data <<- filter(All12Data,Camera == CameraO)
        }

        All12Data <<- mutate (All12Data,Date = substr(Clip_Name,2,9))
        All12Data <<- mutate (All12Data,Year = substr(Date,1,4))
        All12Data <<- mutate (All12Data,Month = substr(Date,5,6))
        All12Data <<- mutate (All12Data,YearMonth = substr(Date,1,6))
        All12Data <<- mutate (All12Data,Day = substr(Date,7,8))
        All12Data <<- mutate (All12Data,Time = substr(Clip_Name,11,16))
        All12Data <<- mutate (All12Data,Hour = substr(Time,1,2))
        All12Data <<- select (All12Data,-DDL,-Leap,-F1,-F2,-Resolved)
        All12Data <<- arrange(All12Data,Date)
        
        Nyear <<- as.integer(YearO)
        Nmonth <<- as.integer(MonthO)
        if (Nmonth > 12) {
          Nmonth <<- 12
          MonthO <<- "ALL"
        }
        if (nchar(MonthO) == 1) {
          MonthO <<- paste("0",MonthO,sep="")
        }
        if (SelectYr != "ALL" & SelectMon != "ALL" & SelectMon != "YTD") {
          print(paste(YearO,MonthO,sep=" "))
          if(MonthO == "12" ) {
            
            ## Include all records for selected year and month plus first half of first day of Jan. next year
            All12Data <<- filter(All12Data, (Year == YearO & Month == MonthO) | 
                                   (Year == as.character(Nyear+1) & Month == "01" & Day == "01" & as.integer(Hour) < 12))
            ## Filter out first half of Day 1 which were processed as November          
            All12Data <<- filter(All12Data, !(Year == YearO & Month == MonthO & Day == "01" & as.integer(Hour) < 12))          
          }
          else {
            
            ## Select records for chosen year
            All12Data <<- filter(All12Data,Year == YearO)
            ## select records for chosen month and the following one
            All12Data <<- filter(All12Data,Month == MonthO | Month == as.character(Nmonth+1))
            ## Filter out first half of Day 1 of selected month which were processed as the previous month          
            All12Data <<- filter(All12Data, !(Month == MonthO & Day == "01" & as.integer(Hour) < 12))
            ## Filter out all bar the first half of Day 1 of following month which will be processed with this month
            All12Data <<- filter(All12Data, !(Month == as.character(Nmonth+1) & Day == "01" & as.integer(Hour) >= 12)
                                 | (Month == as.character(Nmonth+1) & as.integer(Day) >  1))
          }
          
        }
        else { 
          if (SelectYr != "ALL") {
            ## Include all records for selected year and month plus first half of first day of Jan. next year
            All12Data <<- filter(All12Data, (Year == YearO ) | 
                                   (Year == as.character(Nyear+1) & Month == "01" & Day == "01" & as.integer(Hour) < 12))
            ## Filter out first half of Day 1 which were processed as the previous year          
            All12Data <<- filter(All12Data, !(Year == YearO & Month == "01" & Day == "01" & as.integer(Hour) < 12))      
          }
          
        }
        
        ALL <<- data.frame(Month=character(),Both=numeric(),SD_Only=numeric(),cDeg_Only=numeric(),Neither=numeric())
        Months <<- unique(All12Data$YearMonth)
        MLen <<- length(Months)

        for (i in 1:MLen) {
                ALLMonth <<- filter(All12Data, YearMonth == Months[i])
                
                Both <<- filter(ALLMonth,YearMonth == Months[i] & SD <= 0.3 & cDeg <= 0.02)
                SD_Only <<- filter(ALLMonth,YearMonth == Months[i] & SD <= 0.3 & cDeg > 0.02)
                cDeg_Only <<- filter(ALLMonth,YearMonth == Months[i] &  SD > 0.3 & cDeg <= 0.02)
                Neither <<- filter(ALLMonth,YearMonth == Months[i] & SD > 0.3 & cDeg > 0.02)
                
                CountA <<- dim(ALLMonth)[1]
                CountB <<- dim(Both)[1]
                CountS <<- dim(SD_Only)[1]
                CountC <<- dim(cDeg_Only)[1]
                CountN <<- dim(Neither)[1]
                
                PercentB <<- round(CountB / CountA * 100 ,1)
                PercentS <<- round(CountS / CountA * 100 ,1)
                PercentC <<- round(CountC / CountA * 100 ,1)
                PercentN <<- round(CountN / CountA * 100 ,1)
                
                Row <<- data.frame(Month=Months[i],Match="Both",Percentage=PercentB)
                ALL <<- rbind(ALL,Row)
                Row <<- data.frame(Month=Months[i],Match="SD_Only",Percentage=PercentS)
                ALL <<- rbind(ALL,Row)
                Row <<- data.frame(Month=Months[i],Match="cDeg_Only",Percentage=PercentC)
                ALL <<- rbind(ALL,Row)
                Row <<- data.frame(Month=Months[i],Match="Neither",Percentage=PercentN)
                ALL <<- rbind(ALL,Row)
                
        }
        
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
        # Select and configure the output device
        select_dev(Outfile, Otype=output_type, wd= paper_width, ht=paper_height, pp=paper_orientation)
        par(mai=c(1.0,1.5,0.5,1.0))
        #tmptab <- table(Bar_Totals$Category)
        #tmpfrm <-as.data.frame(tmptab)
        
        setwd(ReportDir)
        #####################################################################################
        ## Now Plot SD v cDeg Quality Chart
        #####################################################################################
        P <<- ggplot(ALL[rev(order(ALL$Match)),], aes(Month,Percentage, colour=Match, fill = Match ),environment = environment()) 
        P <<- P + geom_bar(stat = "identity",position = position_fill(reverse = TRUE))
        P <<- P + ggtitle(paste("Clanfield Performance v UKMON and NEMETODE Quality Criteria ",CameraO," ",SelectMon," ",SelectYr,sep=""))
        P <<- P + theme(plot.title = element_text(size = rel(1.5), colour = "blue"))
        P <<- P + theme(axis.text.x =
                                element_text(size  = 5, colour = "black",
                                             angle = 0,
                                             ##hjust = 1,
                                             vjust = 1))
        P <<- P + theme(axis.text.y =
                                element_text(size  = 5, colour = "black",
                                             angle = 0))
        print(P)
        
}        
