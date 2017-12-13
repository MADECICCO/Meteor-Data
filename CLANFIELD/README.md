These scripts consolidate and analyse data captured by and about a (Sonotaco UFO based) video meteor capture system.

On a regular basis (ideally daily), the UFO captures are assessed and categorised remotely (for example via TeamViewer). Counts are maintained of all events to enable system admin to look for problems / areas for improvement (eg spiders that need to be evicted!). False clips (eg those spiders) can then be deleted to keep a lid on data volumes.
 
The 1.MONTHLY_REMOTE_REPORTING script charts these counts to facilitate this analysis.

Valid clips (those we believe are genuine meteor events) are downloaded from the capture PC(s) regularly (typically at the end of each calendar month), and processed through UFO Analyser to determine trajectories and velocities.

The 2.MONTHLY_ANALYSIS_REPORTING script performs quality checks on the output from UFO Analyser and charts the results to ensure that the data is of the highest quality possible before it is integrated with data from other meteor detection stations and input into UFO Orbit for triangulation to permit determination of the orbital characteristics of the originating meteoroid.

The 3.MONTHLY_UFO_ANALYSIS_REPORTING script reports on the actual data (number of Fireballs, number of meteors recorded per shower etc)

Important Note: Before running scripts 2 and 3 for a new month, the child script Consolidata.R needs to be updated to include the input file for the new month (insert 2 new lines). This is a recognised limitation and ought to be fixed sometime.
