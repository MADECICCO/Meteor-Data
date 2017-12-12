#Chart_Scatter_SD_CDEG <<- function(Ocamera="ALL") {
{
        #######################################################################################################
        ## Version 1
        ## This function reads in data for one or more camera and prepares scatter charts
        #######################################################################################################

        
        All13Data <<- data.frame()
        if (Ocamera == "ALL" | Ocamera == "NE") {
                All13Data <<- rbind(All13Data,NEData)
        }
        if (Ocamera == "ALL" | Ocamera == "NO") {
                All13Data <<- rbind(All13Data,NOData)
        }        
        if (Ocamera == "ALL" | Ocamera == "NW") {
                All13Data <<- rbind(All13Data,NWData)
        }
        if (Ocamera == "ALL" | Ocamera == "SE") {
                All13Data <<- rbind(All13Data,SEData)
        }
        if (Ocamera == "ALL" | Ocamera == "SO") {
                All13Data <<- rbind(All13Data,SOData)
        }
        
        ####################################################################################
        ## The following logic added to enable the script to chart data for a single month
        ####################################################################################
        All13Data <<- mutate (All13Data,Date = substr(Clip_Name,2,9))
        All13Data <<- mutate (All13Data,Year = substr(Date,1,4))
        All13Data <<- mutate (All13Data,Month = substr(Date,5,6))
        All13Data <<- mutate (All13Data,Day = substr(Date,7,8))
        All13Data <<- mutate (All13Data,Time = substr(Clip_Name,11,16))
        All13Data <<- mutate (All13Data,Hour = substr(Time,1,2))
        
        Nyear <<- as.integer(Oyear)
        Nmonth <<- as.integer(Omonth)
        if (Nmonth > 12) {
          Nmonth <<- 12
          Omonth <<- "ALL"
        }
        if (Oyear != "ALL" ) {
          All13Data <<- filter(All13Data,Year == Oyear)
        }
        if (SelectMon != "ALL" & SelectMon != "YTD") {
          All13Data <<- filter(All13Data,as.integer(Month) == Nmonth)
        }
        ## end of added logic #############################################################
        
        All13Data <<- arrange(All13Data,as.integer(DDL))
        All13Data$SD <<- as.numeric(All13Data$SD)
        All13Data$cDeg <<- as.numeric(All13Data$cDeg)
        All13Data$DDL <<- as.integer(All13Data$DDL)
        
        ## setup variable text for header
        if (Ocamera == "ALL") {
                Subject <<- "all cameras"
        }
        else {
                Subject <<- paste(Ocamera," camera",sep = "")
        }
        varheader <<- paste("Optimised SD v optimised cDeg for Clanfield ",Subject," ",SelectMon," ",Oyear,sep="")
        
        # Close any open graphical output devices (other than NULL)
        repeat{
          if(dev.cur() == 1) {
            break
          }
          dev.off()
        }
        
        ## Setup colour palette for consistency across charts
        colour_range <<- c("NE","NO","NW","SE","SO")
        colour_range.col <<- c("#339966","#cc6600","#666699","#ff3399","#669933")
        names(colour_range.col)  <<- colour_range
        
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
        ##################################################################################
        ## Plot Scatter Chart of SD v cDeg
        ##################################################################################
        P <<- ggplot(data=All13Data, aes(x=SD, y=cDeg, colour=Camera))##, size=1)) 
        ##P <<- ggplot(data=All13Data, aes(x=SD, y=cDeg, colour=Camera, size=Camera)) 
        P <<- P + geom_point()
        P <<- P + labs(title = varheader)
        P <<- P + theme(plot.title = element_text(size = rel(2), colour = "blue"))
        P <<- P + theme(axis.text.x =
                                element_text(size  = 5, colour = "black",
                                             angle = 0,
                                             ##hjust = 1,
                                             vjust = 1))
        P <<- P + theme(axis.text.y =
                                element_text(size  = 5, colour = "black",
                                             angle = 0))
        ##P <<- P + coord_flip()
        P <<- P + xlim(0,2) + ylim(0,0.05)
       

        print(P)

}        