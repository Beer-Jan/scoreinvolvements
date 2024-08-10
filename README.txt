The HTML file walks through the statistical portion of this project. It has some examples of analysis. The files can be downloaded to create plots for whatever purpose is desired.

Contents of the repository:

allscoreinvolvements.csv : data from afl website with average percentage score involvements per game
createplots.R            : operations that use the csv files to create score involvements plots
pamdata.csv              : data from afltables that were used to fit the PAM clustering - not to be updated
playerheights.csv        : data from afltables to fill height part of clustering data
playerpositions.csv      : data from afltables that classify all players in 1 of 7 positions, which in turn get converted into one of the four traditional positions in createplots.R
positionmedoids.csv      : medoids that are used to find player positions from afltables stats
recreatemedoids.R        : script that gives the same medoids that are used for classification of positions
sianalysis.html          : file that walks through statistical analysis
updatedataframes.R       : updates the csv's with new data from afl website and afltables
