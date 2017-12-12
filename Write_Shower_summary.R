#Write_Shower_Summary <<- function(Ocamera,Oyear,Omonth) {
{
        ##
        ## This is version 11 xml Request logic removed
        ## NB LIne at end is needed to control inclusion or exclusion of Jan 1st counts (needed for Dec report only)
        ##
        

        
        ## Extract a clean list of unique shower codes, sorted alphabetically
        showercodes <<- newdata[,2:3]
        ## ensure codes are all in upper case
        showercodes$Group <<- toupper(showercodes$Group)
        ## revert SPO and FAST to lower case
        scdim <<- dim(showercodes)
        for (sc in 1:scdim[1]) {
                if (showercodes$Group[sc] == "SPO") {
                        showercodes$Group[sc] <<- tolower(showercodes$Group[sc])
                }
                if (showercodes$Group[sc] == "FAST") {
                        showercodes$Group[sc] <<- tolower(showercodes$Group[sc])
                }
        }
        ## sort codes
        oshowercodes <<- as.factor(sort.int(showercodes$Group))
        L <<- length(oshowercodes)
        us <<- oshowercodes[1]
        ## create list of unique codes
        ushowercodes <<- us
        u <<- 1
        for (k in 2:L) {
                oshower <<- as.character(oshowercodes[k])
                ushower <<- as.character(ushowercodes[u])
                if (oshower != ushower) {
                        ushowercodes[u+1] <<- oshower
                        u <<- u + 1
                }
        }
        ## add an extra row for totals
        CShowers <<- as.character(ushowercodes)
        CTShowers <<- c(CShowers,"Totals")
        ushowercodes <<- as.factor(CTShowers)
        
        ## setup data frame for results
        showerdf <<- data.frame("Shower"=ushowercodes,"Jan"=0,"Feb"=0,"Mar"=0,"Apr"=0,"May"=0,"Jun"=0
                                ,"Jul"=0,"Aug"=0,"Sep"=0,"Oct"=0,"Nov"=0,"Dec"=0,"New"=0,"Tot"=0)
        DIMS <<- dim(showerdf)
        ROWSS <<- DIMS[1]
        COLSS <<- DIMS[2]
        
        ## gather data
        for (i in 1:ROWSD) {
                for (x in 1:ROWSS) {
                        if (toupper(newdata[i,"Group"]) == toupper(showerdf[x,"Shower"])) {
                                if (newdata[i,"M.UT."] == 1) {
                                        if (newdata[i,"Y.UT."] == as.integer(Curr_year) &   ## for Dec data force Jan events into New column
                                            newdata[i,"M.UT."] == as.integer(Curr_month)) { ## for Jan data force Jan events into Jan column
                                                showerdf[x,"New"] <<- showerdf[x,"New"] + 1
                                                showerdf[x,"Tot"] <<- showerdf[x,"Tot"] + 1
                                                showerdf[ROWSS,"New"] <<- showerdf[ROWSS,"New"] + 1
                                        }
                                        else {showerdf[x,"Jan"] <<- showerdf[x,"Jan"] + 1
                                                showerdf[x,"Tot"] <<- showerdf[x,"Tot"] + 1
                                                showerdf[ROWSS,"Jan"] <<- showerdf[ROWSS,"Jan"] + 1
                                        }
                                }
                                if (newdata[i,"M.UT."] == 2) {
                                        showerdf[x,"Feb"] <<- showerdf[x,"Feb"] + 1
                                        showerdf[x,"Tot"] <<- showerdf[x,"Tot"] + 1
                                        showerdf[ROWSS,"Feb"] <<- showerdf[ROWSS,"Feb"] + 1
                                }
                                if (newdata[i,"M.UT."] == 3) {
                                        showerdf[x,"Mar"] <<- showerdf[x,"Mar"] + 1
                                        showerdf[x,"Tot"] <<- showerdf[x,"Tot"] + 1
                                        showerdf[ROWSS,"Mar"] <<- showerdf[ROWSS,"Mar"] + 1
                                }       
                                if (newdata[i,"M.UT."] == 4) {
                                        showerdf[x,"Apr"] <<- showerdf[x,"Apr"] + 1
                                        showerdf[x,"Tot"] <<- showerdf[x,"Tot"] + 1
                                        showerdf[ROWSS,"Apr"] <<- showerdf[ROWSS,"Apr"] + 1
                                }       
                                if (newdata[i,"M.UT."] == 5) {
                                        showerdf[x,"May"] <<- showerdf[x,"May"] + 1
                                        showerdf[x,"Tot"] <<- showerdf[x,"Tot"] + 1
                                        showerdf[ROWSS,"May"] <<- showerdf[ROWSS,"May"] + 1
                                }       
                                if (newdata[i,"M.UT."] == 6) {
                                        showerdf[x,"Jun"] <<- showerdf[x,"Jun"] + 1
                                        showerdf[x,"Tot"] <<- showerdf[x,"Tot"] + 1
                                        showerdf[ROWSS,"Jun"] <<- showerdf[ROWSS,"Jun"] + 1
                                }       
                                if (newdata[i,"M.UT."] == 7) {
                                        showerdf[x,"Jul"] <<- showerdf[x,"Jul"] + 1
                                        showerdf[x,"Tot"] <<- showerdf[x,"Tot"] + 1
                                        showerdf[ROWSS,"Jul"] <<- showerdf[ROWSS,"Jul"] + 1
                                }       
                                if (newdata[i,"M.UT."] == 8) {
                                        showerdf[x,"Aug"] <<- showerdf[x,"Aug"] + 1
                                        showerdf[x,"Tot"] <<- showerdf[x,"Tot"] + 1
                                        showerdf[ROWSS,"Aug"] <<- showerdf[ROWSS,"Aug"] + 1
                                }
                                if (newdata[i,"M.UT."] == 9) {
                                        showerdf[x,"Sep"] <<- showerdf[x,"Sep"] + 1
                                        showerdf[x,"Tot"] <<- showerdf[x,"Tot"] + 1
                                        showerdf[ROWSS,"Sep"] <<- showerdf[ROWSS,"Sep"] + 1
                                }
                                if (newdata[i,"M.UT."] == 10) {
                                        showerdf[x,"Oct"] <<- showerdf[x,"Oct"] + 1
                                        showerdf[x,"Tot"] <<- showerdf[x,"Tot"] + 1
                                        showerdf[ROWSS,"Oct"] <<- showerdf[ROWSS,"Oct"] + 1
                                }
                                if (newdata[i,"M.UT."] == 11) {
                                        showerdf[x,"Nov"] <<- showerdf[x,"Nov"] + 1
                                        showerdf[x,"Tot"] <<- showerdf[x,"Tot"] + 1
                                        showerdf[ROWSS,"Nov"] <<- showerdf[ROWSS,"Nov"] + 1
                                }
                                if (newdata[i,"M.UT."] == 12) {
                                        showerdf[x,"Dec"] <<- showerdf[x,"Dec"] + 1
                                        showerdf[x,"Tot"] <<- showerdf[x,"Tot"] + 1
                                        showerdf[ROWSS,"Dec"] <<- showerdf[ROWSS,"Dec"] + 1
                                }
                                ## increment total of totals
                                showerdf[ROWSS,"Tot"] <<- showerdf[ROWSS,"Tot"] + 1
                        }       
                }
               
        }
        ## enrich data with full name of shower extracting this from J8_Shower_Names.csv created previously by program Shower.R
        setwd(DataDir)
        Register <<- read.csv("J8plus_Shower_Names.csv")
        Register_of_Showers <<- tbl_df(Register)
        Shower_Summary_table <<- tbl_df(showerdf)
        Analysis <<- merge(Shower_Summary_table,Register_of_Showers,all.x=TRUE,all.y=FALSE, by.x="Shower",by.y="Stream")
        ASelect <<- select(Analysis,Shower,IAU, Name,Jan:Tot)
        FShower <<- filter(ASelect, Shower != "spo" & Shower != "Totals")
        SShower <<- filter(ASelect, Shower == "spo")
        SShower <<- mutate(SShower, Name = "Sporadic")
        TShower <<- filter(ASelect, Shower == "Totals")
        TShower <<- mutate(TShower, Name = "Totals for all Showers")
        Result <<- rbind(FShower,SShower)
        Result <<- rbind(Result,TShower)
        
        ########################################################################
        ## temporary change to lose New Column mid year when it is not necessary
        #Result <<- select(Result,-New)
        ########################################################################
        setwd(ReportDir)
        write.csv(Result, file = paste(Oyear,Omonth,Ocamera, "_Shower_summary.csv",sep=""), quote=FALSE,row.names=FALSE)
 
}