#Ocategory_Bar <<- function(Oyear,Omonth="",Ocategory) {
{
        #######################################################################################################
        ## Version 1
        ## This function reads in the Consolidated Remote Monitoring Data file and prepares histograms for 
        ## the specified category
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

        
        ## Prepare data for charts of the specified year
        if (Oyear =="ALL") {
                All_Category_Data <<- mutate(CategoryData,Plot_Month = Month)
                Omonth <<- "ALL"          ## all data selected so force headings to conform
        }
        else {
                if (Oyear == "ROLL") { # is this value still allowed ?
                        All_Category_Data <<- mutate(CategoryData,Plot_Month = paste(Year,Month,sep=""))
                        Omonth <<- "ALL"          ## still all data selected so force headings to conform
                }        
        
                else {
                        All_Category_Data <<- filter(CategoryData,Year==as.numeric(Oyear))
                        All_Category_Data <<- mutate(All_Category_Data,Plot_Month = Month)
                }
        }        
                
              
        ## Prepare data for charts of the specified camera
        if (Ocategory == "ALL") {}
        else {
                #if specific Ocategory provided
                All_Category_Data <<- filter(All_Category_Data,Category == Ocategory)
        }
        DIMCD <<- dim(All_Category_Data)
        ROWCD <<- DIMCD[1]
        
        for (i in 1: ROWCD) {
                if (nchar(All_Category_Data[i,8]) == 5) {
                        All_Category_Data[i,8] <<- paste(substr(All_Category_Data[i,8],1,4),"0",substr(All_Category_Data[i,8],5,5),sep="")
                }
                
        } 
        
        All_Category_Data <<- arrange(All_Category_Data,Plot_Month,Camera)

        # Select and configure the output device
        select_dev(Outfile, Otype=output_type, wd= paper_width, ht=paper_height, pp=paper_orientation)
        par(mai=c(1.0,1.5,0.5,1.0))
        #tmptab <- table(Bar_Totals$Category)
        #tmpfrm <-as.data.frame(tmptab)
        
        setwd(ReportDir)
        #####################################################################################
        ## Now Plot Category Bar Chart
        #####################################################################################                        
                
        P <<- ggplot(All_Category_Data, aes(Plot_Month,Count, colour=Camera, fill=Camera),environment = environment()) + geom_bar(stat = "identity")
        P <<- P + ylab("No of Events") + xlab("Month") + ggtitle(paste(Ocategory," events recorded by camera",sep=""))
        P <<- P + theme(plot.title = element_text(size = rel(2), colour = "blue"))
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
