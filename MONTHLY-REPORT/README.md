 Each month, Monthly Report.rmd should be run under RStudio - it constructs the monthly report for the Clanfield Meteor Station 
in HTML or PDF depending on which option is selected from the pull down menu on the Knit button

The script invokes all the Child rmarkdown scripts from its current folder.

NB. As currently written, the script and copies of all the child scripts should be copied to a new folder each month and updated as required.

The only changes needed in the main Monthly Report script are to the YAML section at the top of the script (Title and dates will need to be updated).

Otherwise, the only manual updates needed should be to the *_Status.rmd files which contain the author's commentary.
