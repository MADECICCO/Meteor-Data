The automation of UFO Analysis is achieved by using an AutoIT script that repeatedly invokes the UFO Analyser Analyse ALL function 
followed by the Move_Accepted_Clips R script.

The UFOAuto-1-5 Live SRB2.au3 script reads in an ordered list of DDL values and presents each value in turn to UFO Analyser.
It then runs Analyse ALL from the Main TAB using that value of DDL

At the end of the UFO Analyser run, the script switches control to the RStudio Console and runs Move_Accepted_Clips.R
This R script checks the values of SD and cDeg calculated for each clip, and if they are under the required threshold, it
moves that clip to another folder.

Control is then passed back to UFO Analyser which processes the remaining clips with the next value of DDL

This loop continues until all DDL values have been tried, or there are no more clips to process.

Any remaining clips have to be processed by hand

After the remaining clips have been processed, the Move_Accepted_Clips script is run one final time with the "DDL" parameter set to "LAST"
