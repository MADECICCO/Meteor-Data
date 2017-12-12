#Chart_Pareto <<- function(Oyear,Omonth,Ocamera="ALL") 
{
        #######################################################################################################
        ## Version 1
        ## This function reads in the Remote Monitoring Data file and prepares Pareto style analysis
        #######################################################################################################
        
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
        
        
        ## validate Oyear
        if (Oyear != "2014" & Oyear != "2015" & Oyear != "2016" & Oyear != "2017" & Oyear != "ALL") {
                stop("Invalid Oyear")
        }
        else {}
        if (Oyear == "2016") {
                daysinyear <<- 366
        }
        else { daysinyear <<- 365
        }
        
        ## array of valid Omonths 
        valid_months <<- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","ALL","YTD")
        full_months <<- c("January","February","March","April","May","June","July",
                          "August","September","October","November","December","ALL","YTD")
        numeric_months <<- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
        ## Validate Omonth #####################################################################################
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
                        Full_Month <<- as.character(full_months[month_index])
                        m <<- Months_len
                }        
        }
        if (!m_valid) {
                stop("Invalid Omonth")
        }
        if (month_index < 13) {
                
        }
        else {
                #month_index <<- 12  ### need to sort out YTD and ALL logic later
                ## commented out as we need to process 13 (ALL) 14 (YTD) differently,later in the script
        }
        ##############################################################################################
        
        
        ## validate Ocamera
        if (Ocamera != "ALL" & Ocamera != "north" & Ocamera != "south" &
            Ocamera != "north west" & Ocamera != "north east" & Ocamera != "south east") {
                stop("Invalid Camera")
        }
        ##############################################################################################
        
 
        if (Oyear == "ALL") {
                ## Prepare data for Chart of everything recorded
                All_Category_Data_P <<- CategoryData
                Omonth <<- "ALL"          ## all data selected so force headings to conform
        }
        else {
                ## Prepare data for charts of the specified year
                All_Category_Data_P <<- filter(CategoryData,Year==as.numeric(Oyear))
                
                if (Omonth == "ALL" | Omonth == "YTD") {
                        ## Use all year's data for Chart of everything in the requested year
                }
                else {
                        ## Prepare data for charts of the specified month
                        All_Category_Data_P <<- filter(All_Category_Data_P,Month==as.numeric(month_index))
                }
                
        }
        All_Category_Data_P <<- arrange(All_Category_Data_P,Category,Count) # tidies up chart slightly !
        
        ## filter for selected camera if requested
        if (Ocamera == "north") {
                All_Category_Data_P <<- filter(All_Category_Data_P,Camera == "NO")
        }
        if (Ocamera == "north west") {
                All_Category_Data_P <<- filter(All_Category_Data_P,Camera == "NW")
        }
        if (Ocamera == "north east") {
                All_Category_Data_P <<- filter(All_Category_Data_P,Camera == "NE")
        }
        if (Ocamera == "south east") {
                All_Category_Data_P <<- filter(All_Category_Data_P,Camera == "SE")
        }
        if (Ocamera == "south") {
                All_Category_Data_P <<- filter(All_Category_Data_P,Camera == "SO")
        }
        
        
        ## calculate category totals and display them (print)
        Air <<- filter(All_Category_Data_P, Category == "Aircraft")
        AirSum <<- sum(Air$Count)
        #print(c("Total Aircraft Events : ",AirSum))
        Bat <<- filter(All_Category_Data_P, Category == "Bat")
        BatSum <<- sum(Bat$Count)
        #print(c("Total Bat Events : ",BatSum))
        Bird <<- filter(All_Category_Data_P, Category == "Bird")
        BirdSum <<- sum(Bird$Count)
        #print(c("Total Bird Events : ",BirdSum))
        Flash <<- filter(All_Category_Data_P, Category == "Flash")
        FlashSum <<- sum(Flash$Count)
        #print(c("Total Flash Events : ",FlashSum))
        Insect <<- filter(All_Category_Data_P, Category == "Insect")
        InsectSum <<- sum(Insect$Count)
        #print(c("Total Insect Events : ",InsectSum))
        Meteor <<- filter(All_Category_Data_P, Category == "Meteor")
        MeteorSum <<- sum(Meteor$Count)
        #print(c("Total Meteor Events : ",MeteorSum))
        Moon <<- filter(All_Category_Data_P, Category == "Moon")
        MoonSum <<- sum(Moon$Count)
        #print(c("Total Moon Events : ",MoonSum))
        Noise <<- filter(All_Category_Data_P, Category == "Noise")
        NoiseSum <<- sum(Noise$Count)
        #print(c("Total Electrical Noise Events : ",NoiseSum))
        Spider <<- filter(All_Category_Data_P, Category == "Spider")
        SpiderSum <<- sum(Spider$Count)
        #print(c("Total Spider Events : ",SpiderSum))
        Twilight <<- filter(All_Category_Data_P, Category == "Twilight")
        TwilightSum <<- sum(Twilight$Count)
        #print(c("Total Twilight Events : ",TwilightSum))
        Weather <<- filter(All_Category_Data_P, Category == "Weather")
        WeatherSum <<- sum(Weather$Count)
        #print(c("Total Weather Events : ",WeatherSum))
        White <<- filter(All_Category_Data_P, Category == "White")
        WhiteSum <<- sum(White$Count)
        #print(c("Total White Noise Events : ",WhiteSum))
        
        # print overall total number of events recorded
        AllSum <<- AirSum + BatSum + BirdSum + FlashSum + InsectSum + MeteorSum + MoonSum +
                NoiseSum + SpiderSum + TwilightSum + WeatherSum + WhiteSum
        print (c("Total number of events recorded : ",AllSum))
        
        Bar_Totals <<- data.frame()
        Single_Cat <<- data.frame(Category="Aircraft",Count=as.numeric(AirSum))
        Bar_Totals <<- rbind(Bar_Totals,Single_Cat)
        Single_Cat <<- data.frame(Category="Bat",Count=as.numeric(BatSum))
        Bar_Totals <<- rbind(Bar_Totals,Single_Cat)
        Single_Cat <<- data.frame(Category="Bird",Count=as.numeric(BirdSum))
        Bar_Totals <<- rbind(Bar_Totals,Single_Cat)
        Single_Cat <<- data.frame(Category="Flash",Count=as.numeric(FlashSum))
        Bar_Totals <<- rbind(Bar_Totals,Single_Cat)
        Single_Cat <<- data.frame(Category="Insect",Count=as.numeric(InsectSum))
        Bar_Totals <<- rbind(Bar_Totals,Single_Cat)
        Single_Cat <<- data.frame(Category="Meteor",Count=as.numeric(MeteorSum))
        Bar_Totals <<- rbind(Bar_Totals,Single_Cat)
        Single_Cat <<- data.frame(Category="Moon",Count=as.numeric(MoonSum))
        Bar_Totals <<- rbind(Bar_Totals,Single_Cat)
        Single_Cat <<- data.frame(Category="Noise",Count=as.numeric(NoiseSum))
        Bar_Totals <<- rbind(Bar_Totals,Single_Cat)
        Single_Cat <<- data.frame(Category="Spider",Count=as.numeric(SpiderSum))
        Bar_Totals <<- rbind(Bar_Totals,Single_Cat)
        Single_Cat <<- data.frame(Category="Twilight",Count=as.numeric(TwilightSum))
        Bar_Totals <<- rbind(Bar_Totals,Single_Cat)
        Single_Cat <<- data.frame(Category="Weather",Count=as.numeric(WeatherSum))
        Bar_Totals <<- rbind(Bar_Totals,Single_Cat)
        Single_Cat <<- data.frame(Category="White",Count=as.numeric(WhiteSum))
        Bar_Totals <<- rbind(Bar_Totals,Single_Cat)
        
        #####################################################################################
        ## Prepare to Plot Pareto Chart
        #####################################################################################
        
        if (Ocamera == "ALL") {
                camera_text <<- "all cameras for "
                if (Omonth == "ALL") {
                        month_text <<- ""
                }
                else {
                        month_text <<- Full_Month
                }
        }
        else {
                camera_text <<- paste(Ocamera, " camera for ",sep="" )
                if (Omonth == "ALL") {
                        month_text <<- ""
                }
                else {
                        month_text <<- Full_Month
                }
        }
        if (Oyear == "ALL") {
          year_text <<- " all years"
        }
        else {
          year_text <<- paste(" ",Oyear,sep="")
        }
        # Select and configure the output device
        select_dev(Outfile, Otype=output_type, wd= paper_width, ht=paper_height, pp=paper_orientation)
        par(mai=c(1.0,1.5,0.5,1.0))
        #tmptab <- table(Bar_Totals$Category)
        #tmpfrm <-as.data.frame(tmptab)
        
        setwd(ReportDir)
        #####################################################################################
        ## Now Plot Pareto Chart
        #####################################################################################
        ## First create a custom colour palette for consistency across charts
        colour_range     <<- c("Aircraft","Bat",     "Bird",    "Flash",   "Insect",  "Meteor",
                              "Moon",    "Noise",   "Spider",  "Twilight","Weather", "White")        
        colour_range.col <<- c("#8dd3c8","#fffdb3","#bebbda","#fb8071","#80b1d2","#0000ff",
                              "#b3df66","#fdcde5","#d9d9d9","#bd80bc","#cceac4","#ffed6f")
        
        colmapping <<- data.frame(Category=colour_range,Col=colour_range.col)
        colScale <<- scale_colour_manual(name = "Category",
                                        breaks=colmapping$Category, 
                                        values = colmapping$Col)
        fillScale <<- scale_fill_manual(name = "Category",
                                       breaks=colmapping$Category, 
                                       values = colmapping$Col)  
        P <<- ggplot(Bar_Totals, 
                    aes(x=reorder(Category,-Count), y=Count, 
                        fill=Category),
                    environment = environment()) + geom_bar(stat = "identity")
        P <<- P + geom_text(data=Bar_Totals, mapping=aes(x=reorder(Category,-Count), y=Count, label=Count), size=3, vjust=-1)
        P <<- P + ylab("No of Events") + xlab("Category") + ggtitle(paste("Events recorded by ",camera_text,month_text,year_text,sep=""))
        P <<- P + theme(plot.title = element_text(size = rel(1.5), colour = "blue"))
        P <<- P + theme(axis.text.x =
                               element_text(size  = 10, colour = "black",
                                            angle = 0,
                                            ##hjust = 1,
                                            vjust = 1))
        P <<- P + theme(axis.text.y =
                               element_text(size  = 10, colour = "black",
                                            angle = 0))
        P <<- P + theme(legend.position= "none")
        P <<- P + scale_fill_manual("Category", values = colour_range.col)
        print(P)
        

}
