#Write_Top_5 <<- function(Ocamera,Oyear,Omonth,Oshower) {
{
        ##
        ## This is version 2
        ## Updated in line with 2016 Shower_Summary to process new camera names
        ##
        

        
        if (Oshower == "" ) {                    ## this permits plot of all data by skipping filter
            filtered_data <<- newdata        
        }
        else {
                filtered_data <<- filter(newdata,Group == Oshower)
                filtered_data <<- filter(filtered_data,"Vo.km.s." != -1.0)
        }
        #if (Omonth == "YTD" | Omonth == "ALL") {}
        #else {
        #  print(Omonth)
        #  filtered_data <<- filter(filtered_data,"M.UT." == as.integer(Omonth))
        #}
  
        ## reset array counts after possibly deleting unmatched rows
        DIMD <<- dim(newdata)
        ROWSD <<- DIMD[1]
        COLSD <<- DIMD[2]
        ## count the number of events for each shower
        counts <<- count(newdata,Group)
        ## drop showers if less events than required are present - also drop sporadics
        doubledigits <<- filter(counts,n>4 & Group != "spo")
        
        ## sort counts data alphabetically by Group ID
        doubledigits <<- arrange(doubledigits, desc(n))
        ## extract most numerous "Top 5" showers
        DIMDD <<- dim(doubledigits)
        ROWDD <<- DIMDD[1]
        
        Top5 <<- data.frame()
        if (ROWDD >= 5) {
                FIVE <<- 5
        }
        else {
                FIVE <<- ROWDD
        }
        for (i in 1:FIVE) {
                Top5 <<- rbind(Top5,doubledigits[i,])
        }
                
        Top5 <<- arrange(Top5,desc(n))
        if (ROWDD <= 5) {}
        
        else {if (ROWDD >= 10) {
                TEN <<- 10
                }
              else {
                 TEN <<- ROWDD     
              }
                for (i in 6:TEN) {
                        if (doubledigits[i,2] < Top5[5,2]) {
                                i <<- 11
                        }
                        else {        
                                Top5 <<- rbind(Top5,doubledigits[i,])
                        }        
                }
        }        
        
        ## enrich data with full name of shower extracting this from J8_Shower_Names.csv created previously by program Shower.R
        setwd(DataDir)
        Register <<- read.csv("J8plus_Shower_Names.csv")
        Register_of_Showers <<- tbl_df(Register)
        Full_Top5 <<- tbl_df(Top5)
        Top5_Output <<- merge(Full_Top5,Register_of_Showers,all.x=TRUE,all.y=FALSE, by.x="Group",by.y="Stream")
        Top5_Output <<- arrange(Top5_Output, desc(n)) 
        Top5_Output <<- mutate(Top5_Output,Shower = paste(Name,"(",Group,")",sep=""),Count = n)
        Top5_Output <<- select(Top5_Output,Shower,Count)
        
        setwd(ReportDir)
        write.csv(Top5_Output, file = paste(Ocamera," ",Oyear," ",Omonth, "_Top5.csv",sep=""), quote=FALSE,row.names=FALSE)
        

        #######################################################################################################
        ## CHANGE LOG
        ## 6 Mar 2016
        ## Initial version split out from Chart_Perseids V2 
        ########################################################################################################
        
}