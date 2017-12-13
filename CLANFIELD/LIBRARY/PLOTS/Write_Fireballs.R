#Write_Fireballs <<- function(Ocamera,Oyear,Omonth) {
{
        ##
        ## This Program retrieves all Fireballs detected for the Ocamera and Period specified
        ## It also uses this data to produce a summary table of counts of fireballs, bright meteors and others
        ##
        
        ## enrich data with full name of shower extracting this from J8_Shower_Names.csv created previously by program Shower.R
        setwd(DataDir)
        Register <<- read.csv("J8plus_Shower_Names.csv")
        Register_of_Showers <<- tbl_df(Register)
        Sporadic_Record <<- data.frame(Stream = "spo", IAU = "NA", Name = "Sporadic")
        Register_of_Showers <<- rbind(Register_of_Showers,Sporadic_Record)
        Register_ofShowers <<- arrange(Register_of_Showers,Stream)
        
        Analysis <<- merge(newdata,Register_of_Showers,all.x=TRUE,all.y=FALSE, by.x="Group",by.y="Stream")
        ASelect <<- select(Analysis,LocalTime, Loc_Cam, Group, Name, Mag)
        FShower <<- filter(ASelect, Group != "Totals" & Group != "_flash" & Mag <= -3.0)
        DIMF <<- dim(FShower)
        ROWFS <<- DIMF[1]
        COLFS <<- DIMF[2]
        if (ROWFS == 0) {
                print("No Fireballs in the Period selected")
        }
        else {
                AShower <<- arrange(FShower,LocalTime)
        }        
        
        ## Now setup table of counts of fireballs, bright, other meteors plus min Mag
        NOTable <<- filter(ASelect, Group != "Totals" & Group != "_flash" & Loc_Cam == " Clanfield_NO" )
        NOTot <<- dim(NOTable)[1]                
        if (NOTot != 0) {
                NOFireball <<- filter(NOTable, Mag <= -3.0)
                NOTotFireball <<- dim(NOFireball)[1]
                NOBright <<- filter(NOTable, Mag > -3.0 & Mag <= 0.0)
                NOTotBright <<- dim(NOBright)[1]
                NOTotOther <<- NOTot - NOTotFireball - NOTotBright
                NOMax <<- max(NOTable$Mag)
                NOMin <<- min(NOTable$Mag)
        }
        
        ## Now setup table of counts of fireballs, bright, other meteors plus min Mag
        NWTable <<- filter(ASelect, Group != "Totals" & Group != "_flash" & Loc_Cam == " Clanfield_NW" )
        NWTot <<- dim(NWTable)[1]                
        if (NWTot != 0) {
                NWFireball <<- filter(NWTable, Mag <= -3.0)
                NWTotFireball <<- dim(NWFireball)[1]
                NWBright <<- filter(NWTable, Mag > -3.0 & Mag <= 0.0)
                NWTotBright <<- dim(NWBright)[1]
                NWTotOther <<- NWTot - NWTotFireball - NWTotBright
                NWMax <<- max(NWTable$Mag)
                NWMin <<- min(NWTable$Mag)
        }
        
        NETable <<- filter(ASelect, Group != "Totals" & Group != "_flash" & (Loc_Cam == " Clanfield_NE"))
        NETot <<- dim(NETable)[1]                
        if (NETot != 0) {
                NEFireball <<- filter(NETable, Mag <= -3.0)
                NETotFireball <<- dim(NEFireball)[1]
                NEBright <<- filter(NETable, Mag > -3.0 & Mag <= 0.0)
                NETotBright <<- dim(NEBright)[1]
                NETotOther <<- NETot - NETotFireball - NETotBright
                NEMax <<- max(NETable$Mag)
                NEMin <<- min(NETable$Mag)
        }
        
        SETable <<- filter(ASelect, Group != "Totals" & Group != "_flash" & (Loc_Cam == " Clanfield_SE"))
        SETot <<- dim(SETable)[1]                
        if (SETot != 0) {
                SEFireball <<- filter(SETable, Mag <= -3.0)
                SETotFireball <<- dim(SEFireball)[1]
                SEBright <<- filter(SETable, Mag > -3.0 & Mag <= 0.0)
                SETotBright <<- dim(SEBright)[1]
                SETotOther <<- SETot - SETotFireball - SETotBright
                SEMax <<- max(SETable$Mag)
                SEMin <<- min(SETable$Mag)
        }
        
        SOTable <<- filter(ASelect, Group != "Totals" & Group != "_flash" & (Loc_Cam == " Clanfield_SO"))
        SOTot <<- dim(SOTable)[1]                
        if (SOTot != 0) {
                SOFireball <<- filter(SOTable, Mag <= -3.0)
                SOTotFireball <<- dim(SOFireball)[1]
                SOBright <<- filter(SOTable, Mag > -3.0 & Mag <= 0.0)
                SOTotBright <<- dim(SOBright)[1]
                SOTotOther <<- SOTot - SOTotFireball - SOTotBright
                SOMax <<- max(SOTable$Mag)
                SOMin <<- min(SOTable$Mag)
        }
        
        Meteors_Summary <<- data.frame(Camera=character(),
                                       No_of_Fireballs=integer(),
                                       No_of_Bright_Meteors=integer(),
                                       Others=integer(),
                                       Brightest_Magnitude=numeric(),
                                       Faintest_Magnitude=numeric())
        
        if (NOTot != 0) {
                LineNO <<- data.frame(Camera = "North",No_of_Fireballs = NOTotFireball,No_of_Bright_Meteors = NOTotBright,Others = NOTotOther,Brightest_Magnitude = NOMin, Faintest_Magnitude = NOMax)
                Meteors_Summary <<- rbind(Meteors_Summary,LineNO)
        }      
        if (NWTot != 0) {
                LineNW <<- data.frame(Camera = "North West",No_of_Fireballs = NWTotFireball,No_of_Bright_Meteors = NWTotBright,Others = NWTotOther,Brightest_Magnitude = NWMin, Faintest_Magnitude = NWMax)
                Meteors_Summary <<- rbind(Meteors_Summary,LineNW)
        }        
        if (NETot != 0) {
                LineNE <<- data.frame(Camera = "North East",No_of_Fireballs = NETotFireball,No_of_Bright_Meteors = NETotBright,Others = NETotOther,Brightest_Magnitude = NEMin, Faintest_Magnitude = NEMax)
                Meteors_Summary <<- rbind(Meteors_Summary,LineNE)
        }        
        if (SOTot != 0) {
                LineSO <<- data.frame(Camera = "South",No_of_Fireballs = SOTotFireball,No_of_Bright_Meteors = SOTotBright,Others = SOTotOther,Brightest_Magnitude = SOMin, Faintest_Magnitude = SOMax)
                Meteors_Summary <<- rbind(Meteors_Summary,LineSO)
        }        
        if (SETot != 0) {
                LineSE <<- data.frame(Camera = "South East",No_of_Fireballs = SETotFireball,No_of_Bright_Meteors = SETotBright,Others = SETotOther,Brightest_Magnitude = SEMin, Faintest_Magnitude = SEMax)
                Meteors_Summary <<- rbind(Meteors_Summary,LineSE)        
        }
        setwd(ReportDir)
        write.csv(AShower, file = paste(Ocamera,Oyear,Omonth, "_Fireball_List.csv",sep=""), quote=FALSE,row.names=FALSE)
        write.csv(Meteors_Summary, file = paste(Ocamera,Oyear,Omonth, "_Meteors_Summary.csv",sep=""), quote=FALSE,row.names=FALSE)
        #######################################################################################################
        ## CHANGE LOG
        ## 24 Jan 2016 first draft
        ## Version 2
        ## 22 Feb 2016 added logic to create summary table of counts
        ########################################################################################################
        
}
