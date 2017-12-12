#Write_Active_Showers <<- function(Ocamera,Oyear,Omonth,Olimit=5) {
{
        ########################################################################################################
        ## This is version 1
        ## This function extracts all showers for which more than 5 events have been detected and sorts them
        ## in descending order, meaning it can be used for YTD top 5, or monthly shower highlights (by 
        ## specifying the month required)
        ########################################################################################################
        

        ## Read in Summary Data#################################################################################        
        setwd(ReportDir)
        summarydata <<- read.csv(paste(Oyear,Omonth,Ocamera, "_Shower_summary.csv",sep=""))
        
        ## Extract Showers with more than 5 events in the current month
        ## NB. The logic interrogates the Tot column so that when a specific month is specified, any events 
        ##     recorded on the morning of the following month will be included. When YTD is requested, all
        ##     events for the selected shower are considered so this can be used to determine the top 5.
        FSummary <<- filter(summarydata, Shower != "spo" & Shower != "Totals" & Tot > Olimit)
        
        ## Arrange in descending order
        ASummary <<- arrange(FSummary, desc(Tot))
        setwd(ReportDir)
        write.csv(ASummary,file = paste(Oyear,Omonth,Ocamera, "Showers_With_More_Than_",Olimit,".csv",sep=""), quote=FALSE,row.names=FALSE)


        ########################################################################################################
        ## CHANGE LOG
        ## 24 Jan 16 Initial Draft
        ########################################################################################################
        
}