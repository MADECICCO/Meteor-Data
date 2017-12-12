#Write_Pareto <<- function(Oyear,Omonth,Ocamera="ALL") 
{
        #######################################################################################################
        ## Version 3
        ## This function reads in the Remote Monitoring Data file and prepares Pareto style analysis
        #######################################################################################################
        
        
        ## validate Oyear
        if (Oyear != "2014" & Oyear != "2015" & Oyear != "2016" & Oyear != "2017" & Oyear != "ALL") {
                stop("Invalid Year")
        }
        
        daysin14 <<- 335 ## account for missing data from 1st - 31st Jan 2014
        daysin15 <<- 365
        daysin16 <<- 366
        daysin17 <<- 365
        
                
        ## setup arrays of valid Months
        valid_months <<- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","ALL","YTD")
        numeric_months <<- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
        ## Validate Month ###########################################################################################
        Months_len <<- length(valid_months)
        m <<- 0
        m_valid <<- FALSE
        while (m < Months_len) {
                m <<- m + 1
                if (as.character(Omonth) != valid_months[m]) { 
                        ## no match found
                }
                else {
                        ## match found
                        m_valid <<-- TRUE
                        month_index <<- m
                        m <<- Months_len
                }        
        }
        if (!m_valid) {
                stop("Invalid Month")
        }
        if (month_index < 13) {
                
        }
        else {
                month_index <<- 12  ### need to sort out YTD and ALL logic later
        }
        ##############################################################################################
        
        
        ## validate Camera ##############################################
        ## ALL denotes all camera ids
        ## NE, NO, NW, SE, SO denote specific camera ids
        ## north denotes NO plus NW camera ids
        ## south denotes SO plus SE camera ids
        ##################################################################
        if (Ocamera != "ALL" & Ocamera != "north" & Ocamera != "south"             
            & Ocamera != "north east" & Ocamera != "north west" & Ocamera != "south east") {
                stop(paste ("Invalid Camera : ", Ocamera))
        }
        ##############################################################################################
        
        if (Oyear == "2014" | Oyear == "ALL") {
                
                #####################################################################################
                ## Read in data for North Camera
                #####################################################################################
                sheet <<- 1
                DataPath <<- paste(DataDir,"/","Clanfield Meteor Camera Remote Monitoring V20141231.ods",sep="")
                InputData2014NO <<- read.ods (file=DataPath,sheet)
                DIM14NO <<- dim(InputData2014NO)
                ROW14NO <<- DIM14NO[1]
                COL14NO <<- DIM14NO[2]
                #########################################        
                # clean North Data (remove non data rows)
                #########################################
                NO14 <<- data.frame()
                for (i in 3:(daysin14+2)) {
                        NO14 <<- rbind(NO14, InputData2014NO[i,])
                }
                # recount rows
                DIM14NO <<- dim(NO14)
                ROW14NO <<- DIM14NO[1]
                COL14NO <<- DIM14NO[2]
                #Split down into Category Counts
                NO14Events <<- data.frame()
                Single_Cat <<- data.frame()
                for (i in 1:ROW14NO) {
                        Event_Date <<- dmy(NO14[i,]$A)
                        # split date
                        Event_Year <<- as.numeric(year(Event_Date))
                        Event_Month <<- as.numeric(month(Event_Date))
                        Event_Day <<- as.numeric(day(Event_Date))
                        
                        ## create single tally record for each category
                        Single_Cat <<- data.frame(Date=NO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Noise",Count=as.numeric(NO14[i,]$H)) ## was Black
                        NO14Events <<- rbind(NO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="White",Count=as.numeric(NO14[i,]$I))
                        NO14Events <<- rbind(NO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Noise",Count=as.numeric(NO14[i,]$J)) ## was Grey
                        NO14Events <<- rbind(NO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Twilight",Count=as.numeric(NO14[i,]$K)) ## was Overexp
                        NO14Events <<- rbind(NO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Noise",Count=as.numeric(NO14[i,]$L))
                        NO14Events <<- rbind(NO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,                          
                                  Category="Aircraft",Count=as.numeric(NO14[i,]$M))
                        NO14Events <<- rbind(NO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Twilight",Count=as.numeric(NO14[i,]$N))
                        NO14Events <<- rbind(NO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Weather",Count=as.numeric(NO14[i,]$O))
                        NO14Events <<- rbind(NO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Moon",Count=as.numeric(NO14[i,]$P))
                        NO14Events <<- rbind(NO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Spider",Count=as.numeric(NO14[i,]$Q))
                        NO14Events <<- rbind(NO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Bird",Count=as.numeric(NO14[i,]$R))
                        NO14Events <<- rbind(NO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Bat",Count=as.numeric(NO14[i,]$S))
                        NO14Events <<- rbind(NO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Insect",Count=as.numeric(NO14[i,]$T))
                        NO14Events <<- rbind(NO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Meteor",Count=as.numeric(NO14[i,]$U))
                        NO14Events <<- rbind(NO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Flash",Count=as.numeric(NO14[i,]$V))
                        NO14Events <<- rbind(NO14Events,Single_Cat)
                        
                                
                }
                # add camera name
                NO14Events <<- data.frame(Camera="NO",NO14Events)
                
                                
                #####################################################################################
                ## Read in data for South Camera
                #####################################################################################
                sheet <<- 2
                DataPath <<- paste(DataDir,"/","Clanfield Meteor Camera Remote Monitoring V20141231.ods",sep="")
                InputData2014SO <<- read.ods (file=DataPath,sheet)
                DIM14SO <<- dim(InputData2014SO)
                ROW14SO <<- DIM14SO[1]
                COL14SO <<- DIM14SO[2]
                #########################################        
                # clean South Data (remove non data rows)
                #########################################
                SO14 <<- data.frame()
                for (i in 3:(daysin14+2)) {
                        SO14 <<- rbind(SO14, InputData2014SO[i,])
                }
                # recount rows
                DIM14SO <<- dim(SO14)
                ROW14SO <<- DIM14SO[1]
                COL14SO <<- DIM14SO[2]
                #Split down into Category Counts
                SO14Events <<- data.frame()
                Single_Cat <<- data.frame()
                for (i in 1:ROW14SO) {
                        Event_Date <<- dmy(SO14[i,]$A)
                        # split date
                        Event_Year <<- as.numeric(year(Event_Date))
                        Event_Month <<- as.numeric(month(Event_Date))
                        Event_Day <<- as.numeric(day(Event_Date))
                        ## create single tally record for each category
                        Single_Cat <<- data.frame(Date=SO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Noise",Count=as.numeric(SO14[i,]$H)) ## was Black
                        SO14Events <<- rbind(SO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="White",Count=as.numeric(SO14[i,]$I))
                        SO14Events <<- rbind(SO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Noise",Count=as.numeric(SO14[i,]$J)) ## was Grey
                        SO14Events <<- rbind(SO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Twilight",Count=as.numeric(SO14[i,]$K)) ## was Overexp
                        SO14Events <<- rbind(SO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Noise",Count=as.numeric(SO14[i,]$L))
                        SO14Events <<- rbind(SO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,                          
                                  Category="Aircraft",Count=as.numeric(SO14[i,]$M))
                        SO14Events <<- rbind(SO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Twilight",Count=as.numeric(SO14[i,]$N))
                        SO14Events <<- rbind(SO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Weather",Count=as.numeric(SO14[i,]$O))
                        SO14Events <<- rbind(SO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Moon",Count=as.numeric(SO14[i,]$P))
                        SO14Events <<- rbind(SO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Spider",Count=as.numeric(SO14[i,]$Q))
                        SO14Events <<- rbind(SO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Bird",Count=as.numeric(SO14[i,]$R))
                        SO14Events <<- rbind(SO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Bat",Count=as.numeric(SO14[i,]$S))
                        SO14Events <<- rbind(SO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Insect",Count=as.numeric(SO14[i,]$T))
                        SO14Events <<- rbind(SO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Meteor",Count=as.numeric(SO14[i,]$U))
                        SO14Events <<- rbind(SO14Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO14[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Flash",Count=as.numeric(SO14[i,]$V))
                        SO14Events <<- rbind(SO14Events,Single_Cat)
                        
                }
                # add camera name
                SO14Events <<- data.frame(Camera="SO",SO14Events)
                                
        }        
        if (Oyear == "2015" | Oyear == "ALL") {
                        
                ######################################################################################
                ## Read in data for North Camera
                ######################################################################################
                sheet <<- 1
                DataPath <<- paste(DataDir,"/","Clanfield Meteor Camera Remote Monitoring V20151231 With East.ods",sep="")
                InputData2015NO <<- read.ods (file=DataPath,sheet)
                DIM15NO <<- dim(InputData2015NO)
                ROW15NO <<- DIM15NO[1]
                COL15NO <<- DIM15NO[2]
                #########################################        
                # clean North Data (remove non data rows)
                #########################################
                NO15 <<- data.frame()
                for (i in 3:(daysin15+2)) {
                        NO15 <<- rbind(NO15, InputData2015NO[i,])
                }
                # recount rows
                DIM15NO <<- dim(NO15)
                ROW15NO <<- DIM15NO[1]
                COL15NO <<- DIM15NO[2]
                #Split down into Category Counts
                NO15Events <<- data.frame()
                Single_Cat <<- data.frame()
                for (i in 1:ROW15NO) {
                        Event_Date <<- dmy(NO15[i,]$A)
                        # split date
                        Event_Year <<- as.numeric(year(Event_Date))
                        Event_Month <<- as.numeric(month(Event_Date))
                        Event_Day <<- as.numeric(day(Event_Date))
                        
                        Single_Cat <<- data.frame(Date=NO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="White",Count=as.numeric(NO15[i,]$H))
                        NO15Events <<- rbind(NO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Noise",Count=as.numeric(NO15[i,]$I))
                        NO15Events <<- rbind(NO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,                          
                                  Category="Aircraft",Count=as.numeric(NO15[i,]$J))
                        NO15Events <<- rbind(NO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Twilight",Count=as.numeric(NO15[i,]$K))
                        NO15Events <<- rbind(NO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Weather",Count=as.numeric(NO15[i,]$L))
                        NO15Events <<- rbind(NO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Moon",Count=as.numeric(NO15[i,]$M))
                        NO15Events <<- rbind(NO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Spider",Count=as.numeric(NO15[i,]$N))
                        NO15Events <<- rbind(NO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Bird",Count=as.numeric(NO15[i,]$O))
                        NO15Events <<- rbind(NO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Bat",Count=as.numeric(NO15[i,]$P))
                        NO15Events <<- rbind(NO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Insect",Count=as.numeric(NO15[i,]$Q))
                        NO15Events <<- rbind(NO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Meteor",Count=as.numeric(NO15[i,]$R))
                        NO15Events <<- rbind(NO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Flash",Count=as.numeric(NO15[i,]$S))
                        NO15Events <<- rbind(NO15Events,Single_Cat)
                        
                        }        
                
                # add camera name
                NO15Events <<- data.frame(Camera="NO",NO15Events)
                
                ######################################################################################
                ## Read in data for South Camera
                ######################################################################################
                sheet <<- 2
                DataPath <<- paste(DataDir,"/","Clanfield Meteor Camera Remote Monitoring V20151231 With East.ods",sep="")
                InputData2015SO <<- read.ods (file=DataPath,sheet)
                DIM15SO <<- dim(InputData2015SO)
                ROW15SO <<- DIM15SO[1]
                COL15SO <<- DIM15SO[2]
                #########################################        
                # clean South Data (remove non data rows)
                #########################################
                SO15 <<- data.frame()
                for (i in 3:(daysin15+2)) {
                        SO15 <<- rbind(SO15, InputData2015SO[i,])
                }
                # recount rows
                DIM15SO <<- dim(SO15)
                ROW15SO <<- DIM15SO[1]
                COL15SO <<- DIM15SO[2]
                #Split down into Category Counts
                SO15Events <<- data.frame()
                Single_Cat <<- data.frame()
                for (i in 1:ROW15SO) {
                        Event_Date <<- dmy(SO15[i,]$A)
                        # split date
                        Event_Year <<- as.numeric(year(Event_Date))
                        Event_Month <<- as.numeric(month(Event_Date))
                        Event_Day <<- as.numeric(day(Event_Date))
                        
                        Single_Cat <<- data.frame(Date=SO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="White",Count=as.numeric(SO15[i,]$H))
                        SO15Events <<- rbind(SO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Noise",Count=as.numeric(SO15[i,]$I))
                        SO15Events <<- rbind(SO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,                          
                                  Category="Aircraft",Count=as.numeric(SO15[i,]$J))
                        SO15Events <<- rbind(SO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Twilight",Count=as.numeric(SO15[i,]$K))
                        SO15Events <<- rbind(SO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Weather",Count=as.numeric(SO15[i,]$L))
                        SO15Events <<- rbind(SO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Moon",Count=as.numeric(SO15[i,]$M))
                        SO15Events <<- rbind(SO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Spider",Count=as.numeric(SO15[i,]$N))
                        SO15Events <<- rbind(SO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Bird",Count=as.numeric(SO15[i,]$O))
                        SO15Events <<- rbind(SO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Bat",Count=as.numeric(SO15[i,]$P))
                        SO15Events <<- rbind(SO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Insect",Count=as.numeric(SO15[i,]$Q))
                        SO15Events <<- rbind(SO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Meteor",Count=as.numeric(SO15[i,]$R))
                        SO15Events <<- rbind(SO15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Flash",Count=as.numeric(SO15[i,]$S))
                        SO15Events <<- rbind(SO15Events,Single_Cat)
                        
                                
                }
                # add camera name
                SO15Events <<- data.frame(Camera="SO",SO15Events)
                
                ######################################################################################
                ## Read in data for East Camera
                ######################################################################################
                sheet <<- 3
                DataPath <<- paste(DataDir,"/","Clanfield Meteor Camera Remote Monitoring V20151231 With East.ods",sep="")
                InputData2015NE <<- read.ods (file=DataPath,sheet)
                DIM15NE <<- dim(InputData2015NE)
                ROW15NE <<- DIM15NE[1]
                COL15NE <<- DIM15NE[2]
                ########################################        
                # clean East Data (remove non data rows)
                ########################################
                NE15 <<- data.frame()
                for (i in 3:(daysin15+2)) {
                        NE15 <<- rbind(NE15, InputData2015NE[i,])
                }
                # recount rows
                DIM15NE <<- dim(NE15)
                ROW15NE <<- DIM15NE[1]
                COL15NE <<- DIM15NE[2]
                #Split down into Category Counts
                NE15Events <<- data.frame()
                Single_Cat <<- data.frame()
                for (i in 1:ROW15NE) {
                        Event_Date <<- dmy(NE15[i,]$A)
                        # split date
                        Event_Year <<- as.numeric(year(Event_Date))
                        Event_Month <<- as.numeric(month(Event_Date))
                        Event_Day <<- as.numeric(day(Event_Date))
                        
                        Single_Cat <<- data.frame(Date=NE15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="White",Count=as.numeric(NE15[i,]$H))
                        NE15Events <<- rbind(NE15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Noise",Count=as.numeric(NE15[i,]$I))
                        NE15Events <<- rbind(NE15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,                          
                                  Category="Aircraft",Count=as.numeric(NE15[i,]$J))
                        NE15Events <<- rbind(NE15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Twilight",Count=as.numeric(NE15[i,]$K))
                        NE15Events <<- rbind(NE15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Weather",Count=as.numeric(NE15[i,]$L))
                        NE15Events <<- rbind(NE15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Moon",Count=as.numeric(NE15[i,]$M))
                        NE15Events <<- rbind(NE15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Spider",Count=as.numeric(NE15[i,]$N))
                        NE15Events <<- rbind(NE15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Bird",Count=as.numeric(NE15[i,]$O))
                        NE15Events <<- rbind(NE15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Bat",Count=as.numeric(NE15[i,]$P))
                        NE15Events <<- rbind(NE15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Insect",Count=as.numeric(NE15[i,]$Q))
                        NE15Events <<- rbind(NE15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Meteor",Count=as.numeric(NE15[i,]$R))
                        NE15Events <<- rbind(NE15Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE15[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Flash",Count=as.numeric(NE15[i,]$S))
                        NE15Events <<- rbind(NE15Events,Single_Cat)
                        
                                
                }
                # add camera name
                NE15Events <<- data.frame(Camera="NE",NE15Events)
        }
        if (Oyear == "2016" | Oyear == "ALL") {
                
                ######################################################################################
                ## Read in data for North and North West Cameras
                ######################################################################################
                sheet <<- 1
                DataPath <<- paste(DataDir,"/","Clanfield Meteor Camera Remote Monitoring for 2016.ods",sep="")
                InputData2016NO <<- read.ods (file=DataPath,sheet)
                DIM16NO <<- dim(InputData2016NO)
                ROW16NO <<- DIM16NO[1]
                COL16NO <<- DIM16NO[2]
                #########################################        
                # clean North Data (remove non data rows)
                #########################################
                NO16 <<- data.frame()
                for (i in 3:(daysin16+2)) {
                        NO16 <<- rbind(NO16, InputData2016NO[i,])
                }
                # recount rows
                DIM16NO <<- dim(NO16)
                ROW16NO <<- DIM16NO[1]
                COL16NO <<- DIM16NO[2]
                #Split down into Category Counts
                NO16Events <<- data.frame()
                Single_Cat <<- data.frame()
                for (i in 1:ROW16NO) {
                        Event_Date <<- dmy(NO16[i,]$A)
                        # split date
                        Event_Year <<- as.numeric(year(Event_Date))
                        Event_Month <<- as.numeric(month(Event_Date))
                        Event_Day <<- as.numeric(day(Event_Date))
                        
                        Single_Cat <<- data.frame(Date=NO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="White",Count=as.numeric(NO16[i,]$F))
                        NO16Events <<- rbind(NO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Noise",Count=as.numeric(NO16[i,]$G))
                        NO16Events <<- rbind(NO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,                          
                                  Category="Aircraft",Count=as.numeric(NO16[i,]$H))
                        NO16Events <<- rbind(NO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Twilight",Count=as.numeric(NO16[i,]$I))
                        NO16Events <<- rbind(NO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Weather",Count=as.numeric(NO16[i,]$J))
                        NO16Events <<- rbind(NO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Moon",Count=as.numeric(NO16[i,]$K))
                        NO16Events <<- rbind(NO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Spider",Count=as.numeric(NO16[i,]$L))
                        NO16Events <<- rbind(NO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Bird",Count=as.numeric(NO16[i,]$M))
                        NO16Events <<- rbind(NO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Bat",Count=as.numeric(NO16[i,]$N))
                        NO16Events <<- rbind(NO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Insect",Count=as.numeric(NO16[i,]$O))
                        NO16Events <<- rbind(NO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Meteor",Count=as.numeric(NO16[i,]$P))
                        NO16Events <<- rbind(NO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Flash",Count=as.numeric(NO16[i,]$Q))
                        NO16Events <<- rbind(NO16Events,Single_Cat)
                        
                                
                }
                # add camera name
                NO16Events <<- data.frame(Camera="NW",NO16Events)
                ## now revert those in the first two weeks to camera "NO"
                levels(NO16Events$Camera) <<- c(levels(NO16Events$Camera), "NO") ## add new permitted value for factor
                for (i in 1:ROW16NO) {
                        if (NO16Events[i,]$Month == 1 & NO16Events[i,]$Day <= 16) {
                                NO16Events[i,]$Camera <<- "NO"
                        }
                }
                
                ######################################################################################
                ## Read in data for East Camera
                ######################################################################################
                sheet <<- 2
                DataPath <<- paste(DataDir,"/","Clanfield Meteor Camera Remote Monitoring for 2016.ods",sep="")
                InputData2016NE <<- read.ods (file=DataPath,sheet)
                DIM16NE <<- dim(InputData2016NE)
                ROW16NE <<- DIM16NE[1]
                COL16NE <<- DIM16NE[2]
                ########################################        
                # clean East Data (remove non data rows)
                ########################################
                NE16 <<- data.frame()
                for (i in 3:(daysin16+2)) {
                        NE16 <<- rbind(NE16, InputData2016NE[i,])
                }
                # recount rows
                DIM16NE <<- dim(NE16)
                ROW16NE <<- DIM16NE[1]
                COL16NE <<- DIM16NE[2]
                #Split down into Category Counts
                NE16Events <<- data.frame()
                Single_Cat <<- data.frame()
                for (i in 1:ROW16NE) {
                        Event_Date <<- dmy(NE16[i,]$A)
                        # split date
                        Event_Year <<- as.numeric(year(Event_Date))
                        Event_Month <<- as.numeric(month(Event_Date))
                        Event_Day <<- as.numeric(day(Event_Date))
                        
                        Single_Cat <<- data.frame(Date=NE16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="White",Count=as.numeric(NE16[i,]$F))
                        NE16Events <<- rbind(NE16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Noise",Count=as.numeric(NE16[i,]$G))
                        NE16Events <<- rbind(NE16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,                          
                                  Category="Aircraft",Count=as.numeric(NE16[i,]$H))
                        NE16Events <<- rbind(NE16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Twilight",Count=as.numeric(NE16[i,]$I))
                        NE16Events <<- rbind(NE16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Weather",Count=as.numeric(NE16[i,]$J))
                        NE16Events <<- rbind(NE16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Moon",Count=as.numeric(NE16[i,]$K))
                        NE16Events <<- rbind(NE16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Spider",Count=as.numeric(NE16[i,]$L))
                        NE16Events <<- rbind(NE16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Bird",Count=as.numeric(NE16[i,]$M))
                        NE16Events <<- rbind(NE16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Bat",Count=as.numeric(NE16[i,]$N))
                        NE16Events <<- rbind(NE16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Insect",Count=as.numeric(NE16[i,]$O))
                        NE16Events <<- rbind(NE16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Meteor",Count=as.numeric(NE16[i,]$P))
                        NE16Events <<- rbind(NE16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=NE16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Flash",Count=as.numeric(NE16[i,]$Q))
                        NE16Events <<- rbind(NE16Events,Single_Cat)
                        
                                
                }
                # add camera name
                NE16Events <<- data.frame(Camera="NE",NE16Events)
                
                #####################################################################################
                ## Read in data for South and South East Cameras
                #####################################################################################
                sheet <<- 3
                DataPath <<- paste(DataDir,"/","Clanfield Meteor Camera Remote Monitoring for 2016.ods",sep="")
                InputData2016SO <<- read.ods (file=DataPath,sheet)
                DIM16NO <<- dim(InputData2016NO)
                ROW16NO <<- DIM16NO[1]
                COL16NO <<- DIM16NO[2]
                ########################################################
                # clean South and South East Data (remove non data rows)
                ########################################################
                SO16 <<- data.frame()
                for (i in 3:(daysin16+2)) {
                        SO16 <<- rbind(SO16, InputData2016SO[i,])
                }
                # recount rows
                DIM16SO <<- dim(SO16)
                ROW16SO <<- DIM16SO[1]
                COL16SO <<- DIM16SO[2]
                #Split down into Category Counts
                SO16Events <<- data.frame()
                Single_Cat <<- data.frame()
                for (i in 1:ROW16SO) {
                        Event_Date <<- dmy(SO16[i,]$A)
                        # split date
                        Event_Year <<- as.numeric(year(Event_Date))
                        Event_Month <<- as.numeric(month(Event_Date))
                        Event_Day <<- as.numeric(day(Event_Date))
                        
                        Single_Cat <<- data.frame(Date=SO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="White",Count=as.numeric(SO16[i,]$F))
                        SO16Events <<- rbind(SO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Noise",Count=as.numeric(SO16[i,]$G))
                        SO16Events <<- rbind(SO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,                          
                                  Category="Aircraft",Count=as.numeric(SO16[i,]$H))
                        SO16Events <<- rbind(SO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Twilight",Count=as.numeric(SO16[i,]$I))
                        SO16Events <<- rbind(SO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Weather",Count=as.numeric(SO16[i,]$J))
                        SO16Events <<- rbind(SO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Moon",Count=as.numeric(SO16[i,]$K))
                        SO16Events <<- rbind(SO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Spider",Count=as.numeric(SO16[i,]$L))
                        SO16Events <<- rbind(SO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Bird",Count=as.numeric(SO16[i,]$M))
                        SO16Events <<- rbind(SO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Bat",Count=as.numeric(SO16[i,]$N))
                        SO16Events <<- rbind(SO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Insect",Count=as.numeric(SO16[i,]$O))
                        SO16Events <<- rbind(SO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Meteor",Count=as.numeric(SO16[i,]$P))
                        SO16Events <<- rbind(SO16Events,Single_Cat)
                        
                        Single_Cat <<- data.frame(Date=SO16[i,]$A,
                                  Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                  Category="Flash",Count=as.numeric(SO16[i,]$Q))
                        SO16Events <<- rbind(SO16Events,Single_Cat)
                        
                                
                }
                # add camera name
                SO16Events <<- data.frame(Camera="SE",SO16Events)
                ## now revert those in the first two weeks to camera "SO"
                levels(SO16Events$Camera) <<- c(levels(SO16Events$Camera), "SO") ## add new permitted value for factor
                for (i in 1:ROW16SO) {
                        if (SO16Events[i,]$Month == 1 & SO16Events[i,]$Day <= 16) {
                                SO16Events[i,]$Camera <<- "SO"
                        }
                }
        }
        
        if (Oyear == "2017" | Oyear == "ALL") {
          
          ######################################################################################
          ## Read in data for North West Cameras
          ######################################################################################
          sheet <<- 1
          DataPath <<- paste(DataDir,"/","Clanfield Meteor Camera Remote Monitoring for 2017.ods",sep="")
          InputData2017NO <<- read.ods (file=DataPath,sheet)
          DIM17NO <<- dim(InputData2017NO)
          ROW17NO <<- DIM17NO[1]
          COL17NO <<- DIM17NO[2]
          ##############################################        
          # clean North West Data (remove non data rows)
          ##############################################
          NO17 <<- data.frame()
          for (i in 3:(daysin17+2)) {
            NO17 <<- rbind(NO17, InputData2017NO[i,])
          }
          # recount rows
          DIM17NO <<- dim(NO17)
          ROW17NO <<- DIM17NO[1]
          COL17NO <<- DIM17NO[2]
          #Split down into Category Counts
          NO17Events <<- data.frame()
          Single_Cat <<- data.frame()
          for (i in 1:ROW17NO) {
            Event_Date <<- dmy(NO17[i,]$A)
            # split date
            Event_Year <<- as.numeric(year(Event_Date))
            Event_Month <<- as.numeric(month(Event_Date))
            Event_Day <<- as.numeric(day(Event_Date))
            
            Single_Cat <<- data.frame(Date=NO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="White",Count=as.numeric(NO17[i,]$F))
            NO17Events <<- rbind(NO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Noise",Count=as.numeric(NO17[i,]$G))
            NO17Events <<- rbind(NO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,                          
                                      Category="Aircraft",Count=as.numeric(NO17[i,]$H))
            NO17Events <<- rbind(NO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Twilight",Count=as.numeric(NO17[i,]$I))
            NO17Events <<- rbind(NO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Weather",Count=as.numeric(NO17[i,]$J))
            NO17Events <<- rbind(NO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Moon",Count=as.numeric(NO17[i,]$K))
            NO17Events <<- rbind(NO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Spider",Count=as.numeric(NO17[i,]$L))
            NO17Events <<- rbind(NO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Bird",Count=as.numeric(NO17[i,]$M))
            NO17Events <<- rbind(NO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Bat",Count=as.numeric(NO17[i,]$N))
            NO17Events <<- rbind(NO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Insect",Count=as.numeric(NO17[i,]$O))
            NO17Events <<- rbind(NO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Meteor",Count=as.numeric(NO17[i,]$P))
            NO17Events <<- rbind(NO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Flash",Count=as.numeric(NO17[i,]$Q))
            NO17Events <<- rbind(NO17Events,Single_Cat)
            
            
          }
          # add camera name
          NO17Events <<- data.frame(Camera="NW",NO17Events)

          
          ######################################################################################
          ## Read in data for North East Camera
          ######################################################################################
          sheet <<- 2
          DataPath <<- paste(DataDir,"/","Clanfield Meteor Camera Remote Monitoring for 2017.ods",sep="")
          InputData2017NE <<- read.ods (file=DataPath,sheet)
          DIM17NE <<- dim(InputData2017NE)
          ROW17NE <<- DIM17NE[1]
          COL17NE <<- DIM17NE[2]
          ##############################################        
          # clean North East Data (remove non data rows)
          ##############################################
          NE17 <<- data.frame()
          for (i in 3:(daysin17+2)) {
            NE17 <<- rbind(NE17, InputData2017NE[i,])
          }
          # recount rows
          DIM17NE <<- dim(NE17)
          ROW17NE <<- DIM17NE[1]
          COL17NE <<- DIM17NE[2]
          #Split down into Category Counts
          NE17Events <<- data.frame()
          Single_Cat <<- data.frame()
          for (i in 1:ROW17NE) {
            Event_Date <<- dmy(NE17[i,]$A)
            # split date
            Event_Year <<- as.numeric(year(Event_Date))
            Event_Month <<- as.numeric(month(Event_Date))
            Event_Day <<- as.numeric(day(Event_Date))
            
            Single_Cat <<- data.frame(Date=NE17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="White",Count=as.numeric(NE17[i,]$F))
            NE17Events <<- rbind(NE17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NE17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Noise",Count=as.numeric(NE17[i,]$G))
            NE17Events <<- rbind(NE17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NE17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,                          
                                      Category="Aircraft",Count=as.numeric(NE17[i,]$H))
            NE17Events <<- rbind(NE17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NE17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Twilight",Count=as.numeric(NE17[i,]$I))
            NE17Events <<- rbind(NE17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NE17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Weather",Count=as.numeric(NE17[i,]$J))
            NE17Events <<- rbind(NE17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NE17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Moon",Count=as.numeric(NE17[i,]$K))
            NE17Events <<- rbind(NE17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NE17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Spider",Count=as.numeric(NE17[i,]$L))
            NE17Events <<- rbind(NE17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NE17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Bird",Count=as.numeric(NE17[i,]$M))
            NE17Events <<- rbind(NE17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NE17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Bat",Count=as.numeric(NE17[i,]$N))
            NE17Events <<- rbind(NE17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NE17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Insect",Count=as.numeric(NE17[i,]$O))
            NE17Events <<- rbind(NE17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NE17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Meteor",Count=as.numeric(NE17[i,]$P))
            NE17Events <<- rbind(NE17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=NE17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Flash",Count=as.numeric(NE17[i,]$Q))
            NE17Events <<- rbind(NE17Events,Single_Cat)
            
            
          }
          # add camera name
          NE17Events <<- data.frame(Camera="NE",NE17Events)
          
          #####################################################################################
          ## Read in data for South East Cameras
          #####################################################################################
          sheet <<- 3
          DataPath <<- paste(DataDir,"/","Clanfield Meteor Camera Remote Monitoring for 2017.ods",sep="")
          InputData2017SO <<- read.ods (file=DataPath,sheet)
          DIM17NO <<- dim(InputData2017NO)
          ROW17NO <<- DIM17NO[1]
          COL17NO <<- DIM17NO[2]
          ##############################################
          # clean South East Data (remove non data rows)
          ##############################################
          SO17 <<- data.frame()
          for (i in 3:(daysin17+2)) {
            SO17 <<- rbind(SO17, InputData2017SO[i,])
          }
          # recount rows
          DIM17SO <<- dim(SO17)
          ROW17SO <<- DIM17SO[1]
          COL17SO <<- DIM17SO[2]
          #Split down into Category Counts
          SO17Events <<- data.frame()
          Single_Cat <<- data.frame()
          for (i in 1:ROW17SO) {
            Event_Date <<- dmy(SO17[i,]$A)
            # split date
            Event_Year <<- as.numeric(year(Event_Date))
            Event_Month <<- as.numeric(month(Event_Date))
            Event_Day <<- as.numeric(day(Event_Date))
            
            Single_Cat <<- data.frame(Date=SO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="White",Count=as.numeric(SO17[i,]$F))
            SO17Events <<- rbind(SO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=SO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Noise",Count=as.numeric(SO17[i,]$G))
            SO17Events <<- rbind(SO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=SO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,                          
                                      Category="Aircraft",Count=as.numeric(SO17[i,]$H))
            SO17Events <<- rbind(SO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=SO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Twilight",Count=as.numeric(SO17[i,]$I))
            SO17Events <<- rbind(SO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=SO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Weather",Count=as.numeric(SO17[i,]$J))
            SO17Events <<- rbind(SO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=SO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Moon",Count=as.numeric(SO17[i,]$K))
            SO17Events <<- rbind(SO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=SO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Spider",Count=as.numeric(SO17[i,]$L))
            SO17Events <<- rbind(SO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=SO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Bird",Count=as.numeric(SO17[i,]$M))
            SO17Events <<- rbind(SO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=SO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Bat",Count=as.numeric(SO17[i,]$N))
            SO17Events <<- rbind(SO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=SO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Insect",Count=as.numeric(SO17[i,]$O))
            SO17Events <<- rbind(SO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=SO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Meteor",Count=as.numeric(SO17[i,]$P))
            SO17Events <<- rbind(SO17Events,Single_Cat)
            
            Single_Cat <<- data.frame(Date=SO17[i,]$A,
                                      Year = Event_Year, Month = Event_Month, Day = Event_Day,
                                      Category="Flash",Count=as.numeric(SO17[i,]$Q))
            SO17Events <<- rbind(SO17Events,Single_Cat)
            
            
          }
          # add camera name
          SO17Events <<- data.frame(Camera="SE",SO17Events)

        }        
                
        # combine data frames
        CategoryData <<- data.frame()
        if (Oyear == "2014" | Oyear == "ALL") {
                CategoryData <<- rbind(CategoryData,NO14Events)
                CategoryData <<- rbind(CategoryData,SO14Events)
        }        
        if (Oyear == "2015" | Oyear == "ALL") {
                CategoryData <<- rbind(CategoryData,NO15Events)
                CategoryData <<- rbind(CategoryData,SO15Events)
                CategoryData <<- rbind(CategoryData,NE15Events)
        }        
        if (Oyear == "2016" | Oyear == "ALL") {
                CategoryData <<- rbind(CategoryData,NO16Events)
                CategoryData <<- rbind(CategoryData,NE16Events)
                CategoryData <<- rbind(CategoryData,SO16Events)
        } 
        if (Oyear == "2017" | Oyear == "ALL") {
          CategoryData <<- rbind(CategoryData,NO17Events)
          CategoryData <<- rbind(CategoryData,NE17Events)
          CategoryData <<- rbind(CategoryData,SO17Events)
        }
        CategoryData <<- filter(CategoryData, Count != "NA")
        ## Write out category data for use by other charting programs
        setwd(ReportDir)
        write.csv(CategoryData, file = paste(Oyear,Omonth,"Consolidated-Category-Data.csv",sep=""), row.names=FALSE)
        
}
       