#Chart_Daily_Bar <<- function(Oyear,Omonth,Ocamera="ALL") {
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
  
        #########################################################################
        ## Prepare Data for requested chart
        #########################################################################
        if (Oyear == "ALL") {
          ## Prepare data for Chart of everything recorded
          All_Category_Data_B <<- CategoryData
          Omonth <<- "ALL"          ## all data selected so force headings to conform
        }
        else {
          ## Prepare data for charts of the specified year
          All_Category_Data_B <<- filter(CategoryData,Year==as.numeric(Oyear))
          
          if (Omonth == "ALL" | Omonth == "YTD") {
            ## Use all year's data for Chart of everything in the requested year
          }
          else {
            ## Prepare data for charts of the specified month
            All_Category_Data_B <<- filter(All_Category_Data_B,Month==as.numeric(month_index))
          }
          
        }
        #All_Category_Data_B <<- arrange(All_Category_Data_B,Category,Count) # tidies up chart slightly !
        
        ## filter for selected camera if requested
        if (Ocamera == "north") {
          All_Category_Data_B <<- filter(All_Category_Data_B,Camera == "NO")
        }
        if (Ocamera == "north west") {
          All_Category_Data_B <<- filter(All_Category_Data_B,Camera == "NW")
        }
        if (Ocamera == "north east") {
          All_Category_Data_B <<- filter(All_Category_Data_B,Camera == "NE")
        }
        if (Ocamera == "south east") {
          All_Category_Data_B <<- filter(All_Category_Data_B,Camera == "SE")
        }
        if (Ocamera == "south") {
          All_Category_Data_B <<- filter(All_Category_Data_B,Camera == "SO")
        }
        
        ## Force one record for each colour
         DummyDF <<- data.frame(Camera = All_Category_Data_B[1,1],
                                Date = All_Category_Data_B[1,2],
                                Year = All_Category_Data_B[1,3],
                                Month = All_Category_Data_B[1,4],
                                Day = All_Category_Data_B[1,5],
                                Category = as.character(c("White","Noise","Aircraft","Twilight","Weather","Moon",
                                             "Spider","Bird","Bat","Insect","Meteor","Flash")),
                                Count = 0
                                )
        All_Category_Data_B <<- rbind(DummyDF,All_Category_Data_B)
        ## Aggregate data needed for plot
        All_Category_Data_B <<- select(All_Category_Data_B,-Camera)
        #Plot_Data <<- All_Category_Data_B
        Plot_Data <<- group_by(All_Category_Data_B,Day,Category)
        Plot_Data <<- summarise(Plot_Data,Count=sum(Count))
        Plot_Data <<- arrange(Plot_Data,Day,desc(Category))
        
        
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
          year_text <<- "all years"
        }
        else {
          year_text <<- Oyear
        }
        
        # Select and configure the output device
        select_dev(Outfile, Otype=output_type, wd= paper_width, ht=paper_height, pp=paper_orientation)
        par(mai=c(1.0,1.5,0.5,1.0))
        #tmptab <- table(Bar_Totals$Category)
        #tmpfrm <-as.data.frame(tmptab)
        
        setwd(ReportDir)
        #####################################################################################
        ## Now Plot Daily Bar Chart
        #####################################################################################
        ## First create a custom colour palette for consistency across charts
        colour_range     <<- c("Aircraft","Bat",     "Bird",    "Flash",   "Insect",  "Meteor",
                               "Moon",    "Noise",   "Spider",  "Twilight","Weather", "White")        
        colour_range.col <<- c("#8dd3c8","#fffdb3","#bebbda","#fb8071","#80b1d2","#0000ff",
                               "#b3df66","#fdcde5","#d9d9d9","#bd80bc","#cceac4","#ffed6f")
        
        colmapping <<- data.frame(Category=colour_range,Col=colour_range.col)
        colmapping <<- mutate_if(colmapping,is.factor,as.character)
        colScale <<- scale_colour_manual(name = "Category",
                                         breaks=colmapping$Category, 
                                         values = colmapping$Col)
        fillScale <<- scale_fill_manual(name = "Category",
                                        breaks=colmapping$Category, 
                                        values = colmapping$Col)  

        P <<- ggplot(Plot_Data, 
                     aes(x=Day,y=Count, 
                     fill=Category), 
                     environment = environment()) + geom_bar(stat = "identity", position = "stack")
        P <<- P + ylab("No of Events") + xlab("Day of Month") + ggtitle(paste("Events recorded per day by ",camera_text,month_text," ",year_text,sep=""))
        P <<- P + theme(plot.title = element_text(size = rel(1.5), colour = "blue"))
        P <<- P + theme(axis.text.x =
                                element_text(size  = 10, colour = "black",
                                             angle = 0,
                                             ##hjust = 1,
                                             vjust = 1))
        P <<- P + theme(axis.text.y =
                                element_text(size  = 10, colour = "black",
                                             angle = 0))
        P <<- P + scale_fill_manual("Category", values = colour_range.col,drop=FALSE)
        print(P)
        
        
         
}        
