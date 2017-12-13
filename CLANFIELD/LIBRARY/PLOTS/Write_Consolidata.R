#Consolidata <<- function() {
{
        #######################################################################################################
        ## Version 1
        ## This function reads in all the Monthly Analysis Reports and writes out consolidated files for 
        ## each camera 
        #######################################################################################################
        
        ## Reset Working Directory to avoid having to move input files
        setwd("D:/Meteor camera Management/Analysis Data/")
        #setwd("D:/R Libraries/MeteorData/R analysis Reports/")
  
        sheet <<- 1
        ## Read in Analysis Data
        
        ##2017
        AnalysisData48 <<- read.ods (file="2017Nov Analysis Report.ods",sheet)
        AnalysisData47 <<- read.ods (file="2017Oct Analysis Report.ods",sheet)
        AnalysisData46 <<- read.ods (file="2017Sep Analysis Report.ods",sheet)
        AnalysisData45 <<- read.ods (file="2017Aug Analysis Report.ods",sheet)
        AnalysisData44 <<- read.ods (file="2017Jul Analysis Report.ods",sheet)
        AnalysisData43 <<- read.ods (file="2017Jun Analysis Report.ods",sheet)
        AnalysisData42 <<- read.ods (file="2017May Analysis Report.ods",sheet)
        AnalysisData41 <<- read.ods (file="2017Apr Analysis Report.ods",sheet)
        AnalysisData40 <<- read.ods (file="2017Mar Analysis Report.ods",sheet)
        AnalysisData39 <<- read.ods (file="2017Feb Analysis Report.ods",sheet)
        AnalysisData38 <<- read.ods (file="2017Jan Analysis Report.ods",sheet)
        
        ##2016
        AnalysisData37 <<- read.ods (file="2016Dec Analysis Report.ods",sheet)
        AnalysisData36 <<- read.ods (file="2016Nov Analysis Report.ods",sheet)
        AnalysisData35 <<- read.ods (file="2016Oct Analysis Report.ods",sheet)
        AnalysisData34 <<- read.ods (file="2016Sep Analysis Report.ods",sheet)
        AnalysisData33 <<- read.ods (file="2016Aug Analysis Report_V2.ods",sheet)
        AnalysisData32 <<- read.ods (file="2016Jul Analysis Report_V2.ods",sheet)
        AnalysisData31 <<- read.ods (file="2016Jun Analysis Report_V2.ods",sheet)
        AnalysisData30 <<- read.ods (file="2016May Analysis Report_V2.ods",sheet)
        AnalysisData29 <<- read.ods (file="2016Apr Analysis Report_V2.ods",sheet)
        AnalysisData28 <<- read.ods (file="2016Mar Analysis Report_V2.ods",sheet)
        AnalysisData27 <<- read.ods (file="2016Feb Analysis Report_V2.ods",sheet)
        AnalysisData26 <<- read.ods (file="2016Jan Analysis Report_V2.ods",sheet)
        
        ##2015
        AnalysisData25 <<- read.ods (file="2015Dec Analysis Report_15-31.ods",sheet)
        AnalysisData24 <<- read.ods (file="2015Dec Analysis Report_01-15.ods",sheet)
        AnalysisData23 <<- read.ods (file="2015Nov Analysis Report_V2.ods",sheet)
        AnalysisData22 <<- read.ods (file="2015Oct Analysis Report_V2.ods",sheet)
        AnalysisData21 <<- read.ods (file="2015Sep01_30 Analysis Report_V2.ods",sheet)
        AnalysisData20 <<- read.ods (file="2015Aug Analysis Report_V2.ods",sheet)
        AnalysisData19 <<- read.ods (file="2015Jul Analysis Report_8STAE.ods",sheet)
        AnalysisData18 <<- read.ods (file="2015Jun Analysis Report_8STAE.ods",sheet)
        AnalysisData17 <<- read.ods (file="2015May Analysis Report_8STAE.ods",sheet)
        AnalysisData16 <<- read.ods (file="201507 Analysis Report.ods",sheet)
        AnalysisData15 <<- read.ods (file="201506 Analysis Report.ods",sheet)
        AnalysisData14 <<- read.ods (file="201505 Analysis Report.ods",sheet)
        AnalysisData13 <<- read.ods (file="201504 Analysis Report.ods",sheet)
        AnalysisData12 <<- read.ods (file="201503 Analysis Report.ods",sheet)
        AnalysisData11 <<- read.ods (file="201502 Analysis Report.ods",sheet)
        AnalysisData10 <<- read.ods (file="201501 Analysis Report.ods",sheet)
        ##2014
        AnalysisData09 <<- read.ods (file="2014 Analysis Report ALL with cDeg.ods",sheet)
        
        ## bind data frames        
        AnalysisData <<- rbind(AnalysisData26,AnalysisData25)
        AnalysisData <<- rbind(AnalysisData,AnalysisData24)
        AnalysisData <<- rbind(AnalysisData,AnalysisData23)
        AnalysisData <<- rbind(AnalysisData,AnalysisData22)
        AnalysisData <<- rbind(AnalysisData,AnalysisData21)
        AnalysisData <<- rbind(AnalysisData,AnalysisData20)
        AnalysisData <<- rbind(AnalysisData,AnalysisData19)
        AnalysisData <<- rbind(AnalysisData,AnalysisData18)
        AnalysisData <<- rbind(AnalysisData,AnalysisData17)
        AnalysisData <<- rbind(AnalysisData,AnalysisData16)
        AnalysisData <<- rbind(AnalysisData,AnalysisData15)
        AnalysisData <<- rbind(AnalysisData,AnalysisData14)
        AnalysisData <<- rbind(AnalysisData,AnalysisData13)
        AnalysisData <<- rbind(AnalysisData,AnalysisData12)
        AnalysisData <<- rbind(AnalysisData,AnalysisData11)
        AnalysisData <<- rbind(AnalysisData,AnalysisData10)
        AnalysisData <<- rbind(AnalysisData,AnalysisData09)
        AnalysisData <<- rbind(AnalysisData,AnalysisData27)
        AnalysisData <<- rbind(AnalysisData,AnalysisData28)
        AnalysisData <<- rbind(AnalysisData,AnalysisData29)
        AnalysisData <<- rbind(AnalysisData,AnalysisData30)
        AnalysisData <<- rbind(AnalysisData,AnalysisData31)
        AnalysisData <<- rbind(AnalysisData,AnalysisData32)
        AnalysisData <<- rbind(AnalysisData,AnalysisData33)
        AnalysisData <<- rbind(AnalysisData,AnalysisData34)
        AnalysisData <<- rbind(AnalysisData,AnalysisData35)
        AnalysisData <<- rbind(AnalysisData,AnalysisData36)
        AnalysisData <<- rbind(AnalysisData,AnalysisData37)
        AnalysisData <<- rbind(AnalysisData,AnalysisData38)
        AnalysisData <<- rbind(AnalysisData,AnalysisData39)
        AnalysisData <<- rbind(AnalysisData,AnalysisData40)
        AnalysisData <<- rbind(AnalysisData,AnalysisData41)
        AnalysisData <<- rbind(AnalysisData,AnalysisData42)
        AnalysisData <<- rbind(AnalysisData,AnalysisData43)
        AnalysisData <<- rbind(AnalysisData,AnalysisData44)
        AnalysisData <<- rbind(AnalysisData,AnalysisData45)
        AnalysisData <<- rbind(AnalysisData,AnalysisData46)
        AnalysisData <<- rbind(AnalysisData,AnalysisData47)
        AnalysisData <<- rbind(AnalysisData,AnalysisData48)

        DIMD <<- dim(AnalysisData)
        ROWAD <<- DIMD[1]
        COLAD <<- DIMD[2]
        
        
        JustData <<- data.frame()
        for (j in 1:ROWAD) { 
                ## discard everything apart from good data records
                if (grepl("^M", AnalysisData[j,1], ignore.case=TRUE)) {
                        ## discard records with false Group IDs
                        if (grepl("^_",AnalysisData[j,9],ignore.case=TRUE)) {
                               # print(paste(AnalysisData[j,1]," ",AnalysisData[j,9]))
                        }
                        else {
                                JustData <<- rbind(JustData,AnalysisData[j,])
                        }
                }        
        }
        JustData <<- rename(JustData,Clip_Name = A,
                            DDL = B, Leap = C, F1 = D, F2 = E,
                            Resolved = F,
                            SD = G, cDeg = H,
                            Class = I, Mag = J, Camera = K
        )
        CleanData <<- select(JustData,Clip_Name,DDL,Leap,F1,F2,Resolved,SD,cDeg,Class,Mag,Camera)
        NEData <<- filter(CleanData, Camera == "NE")
        NOData <<- filter(CleanData, Camera == "NO")
        NWData <<- filter(CleanData, Camera == "NW")
        SEData <<- filter(CleanData, Camera == "SE")
        SOData <<- filter(CleanData, Camera == "SO")
        
        ## Reset Working Directory back to default
        setwd(ReportDir)

        ## Write out Data Files for use by other reporting programs
        write.csv(NEData, file = paste("Consolidated_NE_Data.csv",sep=""), row.names=FALSE)
        write.csv(NOData, file = paste("Consolidated_NO_Data.csv",sep=""), row.names=FALSE)
        write.csv(NWData, file = paste("Consolidated_NW_Data.csv",sep=""), row.names=FALSE)
        write.csv(SEData, file = paste("Consolidated_SE_Data.csv",sep=""), row.names=FALSE)
        write.csv(SOData, file = paste("Consolidated_SO_Data.csv",sep=""), row.names=FALSE)
}        
