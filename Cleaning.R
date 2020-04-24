#### Data cleaning for "PublicationTracker.csv" and GrantstoTrack ####
## Created by: MLF on 24Apr20
## Last edited by:


## Overview of this script: 
# Use notes/values to differentiate which grants are listed in the pub tracker that have no papers published vs. overlapping publications with other grants
# Also listed in grant # (cooperative grants) - 
# Group cooperative grants as separate units, and double funded programs
# Find and remove duplicates - the grant which has the most publications gets to keep the pub if the same paper is listed in two or more grants
# Double check ones that say "can't find", make sure its in the "notes" column (anna), then remove in R
# Conference proceedings- can be a way to get results out so retaining
# No dissertations/ theses

