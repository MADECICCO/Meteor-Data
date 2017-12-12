#Chart_Mag_Spread <<- function(Ocamera="ALL",mmin=-8,mmax=5) {
{
        #######################################################################################################
        ## Version 1
        ## This function reads in the consolidated camera data prepared by Consolidata.R and uses it
        ## to prepare charts showing spread of Magnitude
        #######################################################################################################
        
        mmin <<- -8
        mmax <<- 5
        
        ## Gather input from camera data created by Consolidata script
        All14Data <<- data.frame()
        if (Ocamera == "ALL" | Ocamera == "NE") {
                All14Data <<- rbind(All14Data,NEData)
        }
        if (Ocamera == "ALL" | Ocamera == "NO") {
                All14Data <<- rbind(All14Data,NOData)
        }
        if (Ocamera == "ALL" | Ocamera == "NW") {
                All14Data <<- rbind(All14Data,NWData)
        }
        if (Ocamera == "ALL" | Ocamera == "SE") {
                All14Data <<- rbind(All14Data,SEData)
        }
        if (Ocamera == "ALL" | Ocamera == "SO") {
                All14Data <<- rbind(All14Data,SOData)
        }
        
        ####################################################################################
        ## The following logic added to enable the script to chart data for a single month
        ####################################################################################
        All14Data <<- mutate (All14Data,Date = substr(Clip_Name,2,9))
        All14Data <<- mutate (All14Data,Year = substr(Date,1,4))
        All14Data <<- mutate (All14Data,Month = substr(Date,5,6))
        All14Data <<- mutate (All14Data,Day = substr(Date,7,8))
        All14Data <<- mutate (All14Data,Time = substr(Clip_Name,11,16))
        All14Data <<- mutate (All14Data,Hour = substr(Time,1,2))
        
        Nyear <<- as.integer(Oyear)
        Nmonth <<- as.integer(Omonth)
        if (Nmonth > 12) {
          Nmonth <<- 12
          Omonth <<- "ALL"
        }
        if (Oyear != "ALL" ) {
          All14Data <<- filter(All14Data,Year == Oyear)
        }
        if (SelectMon != "ALL" & SelectMon != "YTD") {
          All14Data <<- filter(All14Data,as.integer(Month) == Nmonth)
        }
        ## end of added logic #############################################################
        
        All14Data[, "Mag"] <<- as.numeric(as.character(All14Data[, "Mag"]))
        #data[,2] <- as.numeric(as.character(data[,2]))
        All14Data <<- arrange(All14Data,as.numeric(Mag))

        ##Drop results beyond data limits in ggplot2
        All14Data <<- filter(All14Data,Mag >= mmin & Mag <= mmax)
        ## setup variable text for header
        if (Ocamera == "ALL") {
                Subject <<- "all cameras"
        }
        else {
                Subject <<- paste(Ocamera," camera",sep = "")
        }
        varheader <<- paste("Plot of calculated Meteor Magnitudes for Clanfield ",Subject," ",SelectMon," ",Oyear,sep="")
        
        ## Setup colour palette for consistency across charts
        colour_range <<- c("NE","NO","NW","SE","SO")
        colour_range.col <<- c("#339966","#cc6600","#666699","#ff3399","#669933")
        names(colour_range.col)  <<- colour_range
                        
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
        # Select and configure the output device
        select_dev(Outfile, Otype=output_type, wd= paper_width, ht=paper_height, pp=paper_orientation)
        par(mai=c(1.0,1.5,0.5,1.0))
        #tmptab <- table(Bar_Totals$Category)
        #tmpfrm <-as.data.frame(tmptab)
        
        setwd(ReportDir)
        #####################################################################################
        ##Plot Magnitude density
        #####################################################################################
        P <<- ggplot(All14Data, aes(Mag, fill=Camera),environment = environment()) + geom_histogram(binwidth=0.1,colour="white")
        P <<- P + labs(title = varheader,x = "Absolute Magnitude (at 100kms)")
        P <<- P + scale_y_discrete(name="Number of Occurences",
        #                           limits=c(0,200),
                                   breaks= seq(0,200,5), 
                                   labels= seq(0,200,5))
        P <<- P + theme(plot.title = element_text(size = rel(1.5), colour = "blue"))
        P <<- P + theme(axis.text.x =
                                element_text(size  = 10, colour = "black",
                                             angle = 0,
                                             ##hjust = 1,
                                             vjust = 1))
        P <<- P + theme(axis.text.y =
                                element_text(size  = 10, colour = "black",
                                             angle = 0))
        #P <<- P + scale_fill_brewer(palette="Dark2") ## the manual palette is the same as this
        P <<- P + scale_fill_manual("Camera", values = colour_range.col)
        print(P)

}        