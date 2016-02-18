# dplyr-introduction.r. For dplyr talk at PBS R workshop. This code is designed
#  to be stepped through one line at a time, and the comments should be read
#  to help explain it. Adapted from ipchSerBallHooksYYR.Snw.
#  Andrew Edwards. 15th February 2016.

# This code will demonstrate some of the features of the R package
#  dplyr applied to a dataset that has been generated, based on
#  the format of a real dataset (I don't have permission to make the
#  true dataset public).

rm(list=ls())
require(dplyr)
require(xtable)
require(gplots)                 # for plotCI
require(boot)
require(PBSmapping)             # for .createIDs


# Some functions for maps to be used later:
# New function by Rowan to minimize white margin around maps
#  expandPlot = function(mar=c(4,3,1.2,0.5), mgp=c(1.6,0.5,0), ...)
#  AME modifying defaults:
expandPlot = function(mar=c(1.8,2,1.0,0.5), mgp=c(1.6,0.5,0), ...)
  {
	par(mar=mar, mgp=mgp, ...)
	invisible()
  }

#------------------------------------------------------
# D1_5a_Basic_map_BC_Rowan.r (last modified 2014-06-13)
#------------------------------------------------------
# Changing slightly from Rowan's:
plotBC = function(xlim=c(-134,-124), ylim=c(48,54.6), 
  zlev=seq(200,1200,200), ...)
{
  data(nepacLL,nepacLLhigh,bcBathymetry)
  coast = if (diff(xlim)<5) nepacLLhigh else nepacLL
  clin = contourLines(bcBathymetry, levels=zlev)
  poly = convCP(clin)
  isob = clipLines(poly$PolySet,xlim=xlim,ylim=ylim)
  pdat = poly$PolyData
  attr(isob,"projection") <- "LL"
  clrFN = colorRampPalette(c("cyan4","blue","navy"))
  clrs = clrFN(length(zlev))
  plotMap(coast,xlim=xlim,ylim=ylim,col="lemonchiffon",border="grey50",
          plt=NULL, ...)
  # addLines(isob, col=clrs, ...)
  invisible(isob)
}

# Stolen from PBSmodelling, where it's a hidden function:
trimWhiteSpace = function(x) {
    return(sub("[[:space:]]+$", "", sub("^[[:space:]]+", "", x)))
  }

source("s_dplyr_funcs.r")     # helper functions that allow string arguments,
                              #  by Sebastian Kranz.
set.seed(1234)
figheight = 6                 # To standardise size of figures
figwidth = 5.7  

# Load in data set.

load("dummyData.RData")               # Loads in data - hook-by-hook
                                      #  and blockLocs0314 - locations of
                                      #  stations (originally called blocks),
                                      #  not by year, all with keep=1

ls()

# dataOrig - a dataframe of SIMULATED longline survey data spanning 2003 to
#  2014.
# Each row represents a hook that caught a species of fish. The columns will be
#  explained as we go along. For now, the imporant information is:
# At each 'block' (which is a location or a station), a set of up to eight
#  skates is put in the water. Each skate consists of up to (approximately)
#  100 hooks. When the longline is pulled back in, any fish caught on a hook
#  is identified and recorded for that hook. Thus, each row corresponds to
#  a hook on which a fish was caught, with columns identifying the year,
#  block (station), skate, hook, species, and more.


dataOrig                       # Not advisable due to size!

head(dataOrig)

summary(dataOrig)

dim(dataOrig)

class(dataOrig)

dataOrig = tbl_df(dataOrig)   # dplyr function:

class(dataOrig)

dataOrig

# From ?tbl_df :
# A data frame tbl wraps a local data frame. The main advantage to
#      using a 'tbl_df' over a regular data frame is the printing: tbl
#      objects only print a few rows and all the columns that fit on one
#      screen, describing the rest of it as text.
# 
# 'tbl_df' implements two important base methods:
#     print Only prints the first 10 rows, and the columns that fit on
#          screen
#     '[' Never simplifies (drops), so always returns data.frame  [?]




# First we want to rename the column 'block' with the more intuitive name
#  'station'. How to do that usually in R?

?




# In dplyr, just
dataOrig = rename(dataOrig, station = block)

dataOrig

# Okay, how would we usually select just the columns with names year, station
#  and species?

?

# And how would we remove some un-needed columns? Often datasets have columns
#  that aren't relevant to our analyses (or at least we first think they aren't).
#  How to remove the columns with names:
#  tripID, hookID, skateID, set, OLDobsHooksPerSkate, direction

?


# dplyr has the function: select()

# To select three columns, do:

dummy = select(dataOrig, year, station, species)

# This means select, from the dataframe dataOrig, the columns with names year,
#  station and species.

dummy

# To select all columns between two column names, just do:

dummy = select(dataOrig, year:species)

dummy

# To remove the un-needed columns:

data = select(dataOrig, -c(tripID, hookID, skateID, set, 
    OLDobsHooksPerSkate, direction))

data

summary(data)

# Can also remove all columns between two column names:

dummy = select(dataOrig, -(year:species))

dummy

# See ?select for more options, such as start_with(), ends_with(), matches()
#  and contains(). For example:

dummy = select(dataOrig, starts_with("s"))

dummy             # Not really of interest in this example. 



# So, having removed the un-needed columns (above) we have:

data



# dplyr function to work on rows: filter()
#  To remember which is which: filteR for Rows, seleCt for Columns.

# Usual R, how to filter the dataframe to just have data for year 2003?

?

# And how about just to have the data for year 2003, station number 2001, skate
#  number 1 and species code 614:

?

# Using filter():

dummy = filter(data, year == 2003)

dummy

dummy = filter(data, year == 2003, station == 2001, skate == 1, species == 614)

dummy

# Can also use | for 'or':

dummy = filter(data, year == 2003 | skate == 1)

dummy

summary(dummy)

# And %in%:

dummy = filter(data, year %in% 2003:2010)

dummy


# dplyr function: arrange()
#  Reorders rows. Takes a dataframe and a set of column names (or more
#  complicated expressions) to order the dataframe by. If more than one column
#  name, then each additional column will break ties in values of preceding
#  columns.

dummy = arrange(data, species)

dummy

dummy = arrange(data, species, hooksPerSkate)

dummy

# And for descending order:

dummy = arrange(data, desc(species))

dummy

# dplyr function: distinct()
#  return the unique values in a table

dummy = distinct(select(dataOrig, species))

dummy

dummy = distinct(select(dataOrig, year, species))

dummy

summary(dummy)


# So far, besides removing unwanted columns we have just manipulated the
#  data frame. Now to do some analyses.

data

# dplyr function: mutate()
# mutate adds a new column to the dataframe, where the new column
#  is a function of existing columns.

# Just select a few columns:
dummy = select(data, year, station, skate, hooksPerSkate, obsHooksPerSkate)

# hooksPerSkate is the number of hooks that were put in the water for each
#  skate, and obsHooksPerSkate is the number of hooks that were actually
#  observed by the person recording the data. We want to ask if there is
#  a difference between these two numbers.
# mutate() can give an extra column that is the difference between the two.

dummy = mutate(dummy, difference = hooksPerSkate - obsHooksPerSkate)

dummy

# Lots of zeros, to just look at the non-zero values of difference:

dummy2 = filter(dummy, difference > 0)

dummy2

# Note that each row still represents a hook.

# With mutate() you can refer to columns that you are creating in the same
#  command.

dummy = select(data, year, station, skate, hooksPerSkate, obsHooksPerSkate)

dummy    # No 'difference' column

dummy2 = mutate(dummy, difference = hooksPerSkate - obsHooksPerSkate,
    diffSquared = difference^2)      # last part refers to 'difference' column

dummy2    



# Okay, in usual R how would we get the total number of Yelloweye Rockfish
#  (species code 442) caught in each year? So for each year, we need to add
#  up the catchCount values for which the species code is 442.

# catchCount is the number of that species caught on the hook (usually 1):
summary(as.factor(data$catchCount))

# First set spCode to be the species code, so we can just change this
#  value here if we want to look at another species (good programming practice).
spCode=442; spName="Yelloweye Rockfish" # spCodeIphc=89

?






# dplyr functions: summarise() and group_by()

# I don't really use group_by on it's own, but it's helpful here to see
#  that is just adds groups to the dataframe:
dummy = group_by(data, year)

dummy

class(dummy)

# So we grouped by year, and there are 11 groups, since we have the years:
unique(data$year)

# Back to our question: how would we get the total number of Yelloweye Rockfish
#  (species code 442) caught in each year?
# First extract just the data for the species we
#  are interested in:

dataSpCode = filter(data, species == spCode)

dataSpCode

# This groups the dataSpCode by year, and then, for each year, gives a new
#  column 'total' that is the sum of the catchCount values in that year.
catchSum = summarise( group_by(dataSpCode, year), total = sum(catchCount) ) 

catchSum

# So, that gives us the number of spCode fish caught in each year.

# We could just do it all in one go on 'data':
catchSum2 = summarise(group_by(dataSpCode, year), species = spCode,
    total = sum(catchCount) ) 

catchSum2

# summarise() works on 'aggregate' functions, which take a vector of values
#  and return a single number. Such as min(), max(), mean() etc. Some extra
#  ones in dplyr are:

# n() - the number of observations in the current group
# n_distinct(x) - number of unique values in x
# first(x), last(x), and nth(x, n).

# For example, we can do the above calculation a different way, since
#  there is only ever one of our species caught on a hook:
summary(dataSpCode$catchCount)    # all ones

catchSum3 = summarise(group_by(dataSpCode, year), species = spCode,
    total = n() ) 
# The n() counts the number of observations (rows) in each group
catchSum3

catchSum2 - catchSum3





# So {\tt data} is a \Sexpr{dim(data)[1]}$\times$\Sexpr{dim(data)[2]} (local) dataOrig frame. Each row is a unique hook, with some that have no catch (species 999) to give a {\tt hooksPerSkate} for that skate. Has already been arranged in order of year and station. See {\tt iphc0314.Snw} for further details.

# May use later:
# \subsection{Check that bait field only changes for 2012}
# data %>% group_by(year, bait) %>% tally()  

# That's good, only 110 (chum salmon) used until 2011, then that and the other two in 2012. Use just the 110 values later.

# \section{Roll up to skate then set level, with $N_{it}$, 2003-2014}

# Get unique set data from this hooks data set. First need to look at the skate level, and then sum the {\tt hookPerSkate}'s for each set. Also look at bait because that can be summarised at skate level. And add up the counts of a particular species for each skate. See Section \ref{sec:notation} for notation, some of which gets used here. [Some of this could maybe have been done in {\tt iphc0314.Snw}, but think I may have kept variable names the same in 20-hook analysis, so keep this all here to avoid any potential confusion.]



# Maybe look at?:
# skateSumm = select(data, year, station, skate, hooksPerSkate,
#    obsHooksPerSkate, effSkate, bait)
# skateUniqHooks = unique(skateSumm)         # Picks unique rows
# Now do with group_by, since then can automatically do the count for the species



# The stock-assessment interest in looking at these data is to obtain a
#  relative index of abundance through time, to then be input for an
#  assessment model. So a time series with one value for each year, where
#  the value is aggregated across the spatial region. (In practice we quantify
#  uncertainty also, but not today).

# But, each skate does not have the same number of hooks. The column effSkate
#  gives the effective skate number, which is, for each set, the effective
#  number of hooks that were put out. A value for a set of effSkate = 8
#  essentially means that that set contained 8 skates each of 100 hooks. 

# So I wanted to summarise, for our species, the number of fish caught for each
#  set, and then divide that by the effSkate, which will standardise the
#  number of fish caught by each set (since with less hooks you'd expect to
#  catch less fish).

# So effSkate should be the same for each set. We can check that on our data,
#  remembering that each line is a row.

dummy = summarise(group_by(data, year, station), unique(effSkate))

dummy

# If effSkate was not the same for each set, then the above command would
#  give an error, since unique(effSkate) would not take a single value.
#  For example:

data
data[1,"effSkate"]

data2 = data
data2[1,"effSkate"] = 7
data2[1,"effSkate"]

# If I do the above command on data2 instead of data, then should get an
#  error:

dummy = summarise(group_by(data2, year, station), unique(effSkate))

# So that's a check that our dataframe is what we expect - we have a row
#  for each hook, but hooks from the same set all have the same effSkate
#  number.

# I first want to roll up the data to the skate level [bait is the bait used
#  for that skate - not always the same!]:

skateUniqHooks = summarise(group_by(data, year, station, skate), 
   hooksPerSkate = unique(hooksPerSkate), 
   obsHooksPerSkate = unique(obsHooksPerSkate),
   bait = unique(bait), effSkate = unique(effSkate), 
   fishPerSkate = sum( (species == spCode) * catchCount))  

# This is a check that will fail if I messed up somewhere:
if( dim(unique(select(data, year, station, skate)))[1] != dim(skateUniqHooks)[1])
                                     { stop("check skateUniqHooks")}

# What is the bait issue?

# This will do groups by year and bait, and then summarise the number of
#  values (rows of skateUniqHooks) for each combination:

summarise(group_by(skateUniqHooks, year, bait), n())

# Can see that every year uses bait 110 (Chum Salmon), except 2010.
#  In 2012 a bait
#  experiment was done: for each set there were four consecutive skates
#  with Chum Salmon, one skate with Pink Salmon and one skate with Walleye
#  Pollock each, and two empty skates. It's possible that the catch rates
#  are effected by the bait, which is why we are still working at the skate
#  level.

# So now we just want to have hook numbers and catch numbers for when
#  Chum Salmon (code 110) was used as bait:

skateUniqHooks = mutate(skateUniqHooks, 
    chumObsHooksPerSkate =  obsHooksPerSkate * (bait == 110),
    chumFishPerSkate =  fishPerSkate * (bait == 110))


# Look at one station from 2012. Use data.frame command to force it to
#  display all the columns, and head just displayes the first 6).
data.frame(head( filter(skateUniqHooks, year == 2012, station == 2002)))


# Now to round up to the set level:

setUniqHooks = summarise( group_by(skateUniqHooks, year, station),
    H_itOut = sum(hooksPerSkate), H_itObs = sum(obsHooksPerSkate), K_itAll = n(),
    K_itChum = sum(bait == 110), effSkate = unique(effSkate), 
    H_itObsChum = sum(chumObsHooksPerSkate), N_itAll = sum(fishPerSkate), 
    N_itChum = sum(chumFishPerSkate))
    # Unique sets, from hooks data. Notation is to match mathematical notation
    #  in the assessment, with subscript i being set and t being year.
    #  H_itOut is number of hooks that were put out (for set i and year t)
    #  H_itObs is the same for those that got observed.
    #  Will give error if
    #  unique(effSkate) not a single value. H_itObsChum is observed hooks
    #  that had chum on. K_itAll is number of skates (all bait), K_itChum is
    #  number of skates with chum. N_itAll is number of spCode fish in the set
    #  using all skates, N_itChum is just based on chum.

if( dim(unique(select(data, year, station)))[1] != dim(setUniqHooks)[1])
                                     { stop("check setUniqHooks")}

# Add column to make effSkateChum only be based on Chum bait (correction factor
#  to account for some skates having different bait):
setUniqHooks = mutate(setUniqHooks, effSkateChum = effSkate * 
    H_itObsChum / H_itObs)

# Use arrange() to put in order (though it should still be in this order).
setUniqHooks = arrange(setUniqHooks, year, station)

# 2012 effSkateChum now relate to number of observed chum-bait hooks: 
data.frame(head(filter(setUniqHooks, year == 2012)))

# For 2012, all K_itChum values seem to be 4 (always 4 skates with chum):
summary((filter(setUniqHooks, year == 2012))$K_itChum)

# Earlier we looked at year 2012, station 2002:
data.frame(head( filter(skateUniqHooks, year == 2012, station == 2002)))
# There are 6 fish of our species caught altogether, but only four
#  are on the skates with chum bait (bait = 110)

# Looking at the set level data, it agrees:
data.frame(head( filter(setUniqHooks, year == 2012, station == 2002)))
# N_itAll = 6, the number of fish caught with all bait; N_itChum = 4, the number
#  of fish caught with Chum.


setUniqHooks

# Now want to create an index value for each year. First simplify to have a
#  set-level dataframe with what we will need:

setVals0314 = select(setUniqHooks, year=year,
    station=station,
    E_it = effSkateChum, N_it = N_itChum)

# Add a column C_it for the catch rate, the number of fish caught divided by
#  the effective skate number (Chum only, as in the above line).
setVals0314 = mutate(setVals0314, C_it = N_it / E_it)

setVals0314


#
# \subsubsection{Filter locations geography, to determine {\tt setVals0314keep}} 
#         % Need all the setVals0314 for the maps

# stationLocs0314 - a dataframe corresponding to each survey station ,
#  giving the longitude and latitude for each station, with
#  an allowable range. The locations do not change each year (so there is no
#  year in here).
#

#stationLocs0314[stationLocs0314$lat < latCutOff, "keep"] = 0    # don't keep some

#setVals0314keep= filter(setVals0314, station %in% 
#    filter(stationLocs0314, keep == 1)$station)  # Keep only stations in geog. region

yearVals0314keep = summarise(group_by(setVals0314keep, year), n_t = n(), 
    setsWithSp = sum(N_it != 0), sum.C_it = sum(C_it))


yearVals0314keep = mutate(yearVals0314keep, I_t = sum.C_it / n_t)
yearVals0314keep
 


stationVals0314 = summarise(group_by(setVals0314, station), totalN_it = sum(N_it))
zeroStations0314 = filter(stationVals0314, totalN_it == 0)$station # Stations always 0
nonZeroStations0314 = filter(stationVals0314, totalN_it > 0)$station  # Stations with catch
alwaysZero0314 = length(zeroStations0314)

# Stations we're keeping (think I calc using the above stuff for the figs anyway)
stationVals0314keep = summarise(group_by(setVals0314keep, station), 
    totalN_it = sum(N_it))
zeroStations0314keep = filter(stationVals0314keep, totalN_it == 0)$station 
                                        # Stations always 0
nonZeroStations0314keep = filter(stationVals0314keep, totalN_it > 0)$station  
                                        # Stations with catch
alwaysZero0314keep = length(zeroStations0314keep)

# \section{BCA bootstrap confidence intervals for all years}

# First, get non-bootstrapped summary:
# Non-bootstrapped values at the year level:
yearValsKeep = summarise(group_by(setValsKeep, year), n_t = n(), 
     setsWithSp = sum(N_it != 0), sum.C_it = sum(C_it))
yearValsKeep = mutate(yearValsKeep, I_t = sum.C_it / n_t, propWithoutSp = 
    (n_t - setsWithSp) / n_t )  
yearValsKeep

# Now for the bootstrapping results. 
# <<booting>>=
boolKeep = list()        # list of boostrap outputs
boolCIkeep = list()
confLow = 0.025        # Confidence levels
confHigh = 0.975
num.boots=10000        # number of bootstrap replicates

meanFun = function(x, I) mean(x[I])
bcaConfKeep = mutate(yearValsKeep, I_tBootMean = NA, I_tBootLow = NA,
    I_tBootHigh = NA, I_tBootCV = NA)
if(confLow != 0.025) { stop("see boolKeep calcs -have not included an option
     for changing the confidence level, as I think you only include 0.95 or 0.90
     etc. and these might not strictly be 2.5-97.5 intervals, as I'm not exactly
     sure how they're calculated") }
years = as.integer(yearValsKeep$year)      # so integers in xtables
for(i in 1:length(years))
  {
  ddKeep = filter(setValsKeep,year == years[i])$C_it
  boolKeep[[i]] = boot(ddKeep, meanFun, R = num.boots)   # list of boot results
  bcaConfKeep[bcaConfKeep$year==years[i], "I_tBootMean"] = 
      mean(boolKeep[[i]]$t)
  boolCIkeep[[i]] = boot.ci(boolKeep[[i]], type="bca")
  bcaConfKeep[bcaConfKeep$year==years[i], "I_tBootLow"] = 
      boolCIkeep[[i]]$bca[4]
  bcaConfKeep[bcaConfKeep$year==years[i], "I_tBootHigh"] = 
      boolCIkeep[[i]]$bca[5]
  bcaConfKeep[bcaConfKeep$year==years[i], "I_tBootCV"] = 
      sd(boolKeep[[i]]$t) / 
      bcaConfKeep[bcaConfKeep$year==years[i], "I_tBootMean"]
  }

# Moving, rather than copying, table and figure to the proper write up as may want to adapt it all there.


# \section{Summary results, plus use some earlier maps prob. See final RBR probably for text..............doing this to line up with {\tt IPHCJOIN6.Snw} for comparing results....need big header............................................}

# Might want Figure \ref{fig:all2003mapA} referenced instead..... check IPHCjoin7, that had something about the extra map. See final RBR write up for text.

# For the 2003$+$ series, Figure \ref{fig:all2003map} shows the locations of the \Sexpr{dim(stationLocs0314)[1] - length(nonZeroStations0314)} stations that never caught \Sexpr{spName} in any year, and the \Sexpr{length(nonZeroStations0314)} stations that caught it at least once.   **That's for all, not just the ones we're keeping.

# <<echo=FALSE, results=hide>>=
postscript("all2003mapA.eps", height = figheight, width = figwidth,
           horizontal=FALSE,  paper="special")  
#expandPlot(mfrow=c(1,1),mar=c(2,2,0.5,0.5))
expandPlot(mfrow=c(1,1),mar=c(1.8,2,1.0,0.5))
plotBC(main = "All 2003+ stations")  #zlev=100)
points(lat~lon, data=stationLocs0314, col="blue", cex=1) # , cex=0.6
points(lat~lon, 
      data=filter(stationLocs0314, station %in% nonZeroStations0314),
      col="blue", pch=20, cex=1.2) 
legend("bottomleft", legend=c(paste("Never caught", shortName), 
       paste("Have caught", shortName)),
       pch=c(1, 20), pt.cex=c(1, 1.2), col=c("blue", "blue"))
dev.off()
# @

# \onefig{all2003mapA}{Locations of all \Sexpr{dim(stationLocs0314)[1]} stations for the IPHC survey for 2003 onwards. There are \Sexpr{dim(stationLocs0314)[1] - length(nonZeroStations0314)} stations that never caught \Sexpr{spName} (blue open circles), and \Sexpr{length(nonZeroStations0314)} stations that did catch it at least once.}

# Do still want just 2003 onwards with the cutoff, so copying the above and
#   editing based on iphcSerA20hooksYYR.Snw:

# <<echo=FALSE, results=hide>>=
postscript("all2003mapAkeep.eps", height = figheight, width = figwidth,
           horizontal=FALSE,  paper="special")  
expandPlot(mfrow=c(1,1),mar=c(1.8,2,1.0,0.5))
plotBC(main = "All 2003-2012 and 2014 stations")  #zlev=100)
points(lat~lon, data=stationLocs0314, col="blue", cex=1) # , cex=0.6
points(lat~lon, 
      data=filter(stationLocs0314, station %in% nonZeroStations0314),
      col="blue", pch=20, cex=1.2) 
points(lat~lon, 
      data=filter(stationLocs0314, keep == 0),
      col="black", pch=4, cex=1.2) 
legend("bottomleft", 
      legend=c(paste("Never caught ", shortName),
                paste("Have caught", shortName), 
                "Omitting from index"),
       pch=c(1, 20, 4), pt.cex=c(1, 1.2, 1.2), col=c("blue", "blue", "black"))
dev.off()

# \onefig{all2003mapAkeep}{Locations of all \Sexpr{dim(stationLocs0314)[1]} stations for the IPHC survey for 2003 onwards. There are \Sexpr{dim(stationLocs0314)[1] - length(nonZeroStations0314)} stations that never caught \Sexpr{spName} (blue open circles), and \Sexpr{length(nonZeroStations0314)} stations that did catch it at least once. Black crosses indicate the \Sexpr{sum(stationLocs0314$keep == 0)} stations being excluded from the analyses.}   % $ 



# % Notation based on {\tt IPHChookAnalysis20.Snw}, which did change slightly from  {\tt IPHCsetTidy.Snw} and {\tt IPHChookTidy.Snw} because then I hadn't distinguish all hooks sent out, observed hooks and chum-bait hooks. Chum-bait calculations on the data are done later.

# The index series from the IPHC surveys consists of the mean catch rate for each year. The mean for a year is the mean of the catch rates of all sets within that year. The catch rate of a set has units of number of \spName~caught per effective skate. The catch rates within a year are bootstrapped, to give bootstrapped means, bias-corrected and adjusted (BCa) bootstrapped 95\% confidence intervals, and bootstrapped coefficients of variation (CV). The bootstrapped means and CVs are used as input for the statistical catch-at-age model. 

# The effective skate number provided by the IPHC is for all skates used, which in 2012 will include skates that were not baited with Chum Salmon (Eric Soderlund, IPHC, pers.~comm.). But we wish to only include the Chum Salmon baited skates, and so we first modify the effective skate number. The effective skate number depends on the number of observed hooks (Eric Soderlund, IPHC, pers.~comm.), rather than the number of hooks that were deployed.

Define:

$H_{it}$ -- number of observed chum-bait hooks in set $i$ in year $t$, % {\tt H\_itObsChum}$=${\tt H\_it} later,

$H_{it}^*$ -- number of observed hooks for all bait types (equals $H_{it}$ for $t < 2012$) % {\tt H\_itObs},

$E_{it}'$ -- effective skate number of set $i$ in year $t$ from IPHC, which is based on all observed hooks (regardless of bait), % {\tt effSkate}

$E_{it}$ -- effective skate number of set $i$ in year $t$ based only on observed chum-bait hooks, % so is {\tt effSkateChum}$=${\tt E\_it} later.

The effective skate number scales linearly with the number of hooks \citep{yocld08}, and so the desired $E_{it}$ is 
\eb
E_{it} = \dfrac{H_{it}}{H_{it}^*} E_{it}'.
\ee

The desired index for the species of interest, in this case \spName, is:

$I_{t}$ -- count index for year $t$ (with units of numbers of fish per effective skate).

Further define:

$n_t$ -- the number of usable sets in year $t$,

$N_{it}$ -- the number of \spName~caught on set $i=1,2,...,n_t$ in year $t$, based on observed chum-bait hooks, % {\tt N\_itChum}$=${\tt N\_it}.

$C_{it}$ -- catch rate (with units of numbers of fish per effective skate) of \spName~for set $i$ in year $t$, based on observed chum-bait hooks. % {\tt C\_it}, such that

Adapting equations on page 3 of \cite{yocld08}, we then have:
\eb
C_{it} = \frac{N_{it}}{E_{it}}.
\label{catchPerSetIPHC}
\ee
The catch rate index for year $t$, $I_t$ (numbers per effective skate), is then the mean catch rate across all sets:    % $I_{t\survey}$
\eb
I_{t} = \frac{1}{n_t} \sum_{i=1}^{n_t} C_{it} = \frac{1}{n_t} \sum_{i=1}^{n_t} \frac{N_{it}}{E_{it}}.
\label{indexIPHC}
\ee
The catch rates $C_{it}$ within each year are boostrapped \Sexpr{num.boots} times to obtain the bootstrap statistics.

\subsection{Results - presumably use the text in {\tt IPHCcombin46.Snw} for a write up. Tables and figures and captions are correct}

# qwe = summarise(group_by(setValsKeep, year), min = min(E_it), mean = mean(E_it), max = max(E_it), lower = quantile(E_it, 0.025), higher=quantile(E_it, 0.975))
effSumm = summarise(group_by(setValsKeep, year), lower = quantile(E_it, 0.025), 
    mean = mean(E_it), higher=quantile(E_it, 0.975))
names(effSumm) = c("Year", "Lower", "Mean", "Higher")
effSummTab = xtable(effSumm, 
    caption=eval(paste("Summary of effective skate numbers, $E\\_{it}$, for each
     year. Lower and Higher are the 2.5\\% and 97.5\\% quantiles, 
     respectively.")),
     lab="tab:effSumm")
# omit to have later in summary
# print(effSummTab, table.placement="tp", caption.placement="top",
#    include.rownames = FALSE, sanitize.text.function=function(x){x})  # was !ht

% Table \ref{tab:effSumm} shows that the values of the effective skate numbers does change over time, but with no clear trend. The lowest value for 2012 is due to only four skates (those with Chum Salmon as bait) being usable. The year 1995 is also low, but has the same mean as 2007 and 2008. ***1996  

% The number of sets each year is lower in 1995 than for 2003$+$ (Table \ref{tab:bcaConfKeep}). Year 2008 has one fewer set because for station number 2113 the hook tally sheet was lost overboard \citep{yfcd11}.  ***1996


# <<bcaConfKeepTable, results=tex, echo=FALSE>>=
xxxxbcaKeep = bcaConfKeep[ , c("year", "n_t", "propWithoutSp", "I_t", 
    "I_tBootMean", "I_tBootLow", "I_tBootHigh", "I_tBootCV")]
                                   # duplicated to rearrange & change names
names(xxxxbcaKeep) = c("Year", "Sets, $n_t$", spAbbr, 
         "Sample $\\bar{I}_t$", "B'ed $I_t$",
         "B'ed $I_t$ lower",
         "B'ed $I_t$ higher", 
         "B'ed $I_t$ CV")
# can't say 2.5% and 97.5% since that might not be correct, just say 95%
xxxxbcaKeepTab = xtable(xxxxbcaKeep, 
    caption=eval(paste("Catch rates by year based on the stations being kept
     in Figure 
     \\ref{fig:stations959603Keep2}. B'ed means bootstrapped 
     value. `\\spAbbr' is the proportion of sets that did not catch \\spName     that year. Lower and higher are the 
     lower and upper bounds of the 95\\% bias-corrected and adjusted (BCa)
     confidence intervals. **IGNORE ME**")),
     lab="tab:bcaConfKeep")
# Omit to have later in summary:
# print(xxxxbcaKeepTab, table.placement="tp", caption.placement="top",
#      include.rownames = FALSE, sanitize.text.function=function(x){x})  # was !ht

# <<bootPlot, echo=FALSE, results=hide>>=
postscript("bcaConfKeep959603.eps", height = figheight, width = figwidth,
           horizontal=FALSE,  paper="special")  
plotCI(years, bcaConfKeep$I_tBootMean, li=bcaConfKeep$I_tBootLow, 
       ui=bcaConfKeep$I_tBootHigh, col="black", barcol="blue", lwd=1, 
       ylim=c(0, max(bcaConfKeep$I_tBootHigh)), 
       xlab="Year", ylab="Catch rate index (numbers per effective skate)")
points(years, bcaConfKeep$I_t, col="red", pch=20, cex=0.7)
# For all hooks (which prob want on same plot at some point) it was:
# plotCI(years, bcaConfExcl0$I_tBootMean, li=bcaConfExcl0$I_tBootLow, 
#       ui=bcaConfExcl0$I_tBootHigh, col="black", barcol="blue", lwd=1, 
#       ylim=c(0, max(bcaConfExcl0$I_tBootHigh)), 
#       xlab="Year", ylab="Catch rate index (numbers per effective skate)")
# points(years, bcaConfExcl0$I_t, col="red", pch=20, cex=0.7)
dev.off()
# @

# % **May want something like this automated:
# % The proportion of sets each year with zero catch of \spName~(Table \ref{tab:bcaConfKeep}) ranges from \Sexpr{round(min(filter(bcaConfKeep, years > 2002.5)$propWithoutSp), digits=2)}-\Sexpr{round(max(filter(bcaConfKeep, years > 2002.5)$propWithoutSp), digits=2)} for the 2003$+$ series, with the highest value being for 2012, which has the lowest effective skate number because of the bait experiment. A higher proportion of zeros (\Sexpr{min(round(filter(bcaConfKeep, years < 2002.5)$propWithoutSp, digits=2))}-\Sexpr{max(round(filter(bcaConfKeep, years < 2002.5)$propWithoutSp, digits=2))}) occurs for earlier data. ***Need to say/think more about this?? Add something about 1996 if necessary.  %$

# The bootstrapped results are shown in (Table \ref{tab:bcaConfKeep}) and plotted in Figure \ref{fig:bcaConfKeep959603}.

# % \onefig{bcaConfKeep959603}{Catch rate index (number of individual \Sexpr{spName} caught per skate). For a given year, the catch rate for each set is calculated from (\ref{catchPerSetIPHC}). These catch rates are then resampled for \Sexpr{num.boots} bootstrap values, from which a bootstrapped mean (open blue circles) and 95\% bias-corrected and adjusted confidence intervals (blue bars) are calculated. Red closed circles are sample means (not bootstrapped).}

# % c) How to deal with all the zero catches? Could use an approach based on his MEE paper (see my written notes). Would more properly account for the zeros. He couldn't say whether or not the CV's would end up being reduced. Bootstrapping seems okay, especially as always at the same site. For write-up I can say:

# A summary of effective skate numbers for each year is shown in Table \ref{tab:effSumm}. 

# <<effTable2, results=tex, echo=FALSE>>=
print(effSummTab, table.placement="tp", caption.placement="top",
      include.rownames = FALSE, sanitize.text.function=function(x){x})  # was !ht


# The resulting bootstrapped catch rate index is shown in Figure \ref{fig:bcaConfKeep959603}, with values in Table \ref{tab:bcaConfKeep}

# The bootstrapped results are shown in (Table \ref{tab:bcaConfKeep}) and plotted in Figure \ref{fig:bcaConfKeep959603}.

# <<effTable2, results=tex, echo=FALSE>>=
print(xxxxbcaKeepTab, table.placement="tp", caption.placement="top",
      include.rownames = FALSE, sanitize.text.function=function(x){x})  # was !ht
# @ 

# save.image(file="iphcSerBallHooksYYR.RData")



# Now to look at a particular species:
# Comment in one of these lines (shortName is for figure legends):
# spCode=401; spName="Redbanded Rockfish"; shortName = "Redbanded"; spCodeIphc=90; spAbbr = RBR
spCode=442; spName="Yelloweye Rockfish"; shortName = "Yelloweye"; spCodeIphc=89; spAbbr = "No YYR" 
# spCode = 614; spName = "Pacific Halibut"; shortName = "Halibut"; spCodeIphc=1; spAbbr = "HAL" 
# spCode = 044; spName = "North Pacific Spiny Dogfish"; shortName = "Dogfish"; spCodeIphc=54; spAbbr = "DOG"    # Get error in bca.ci, expected adjustment 'w' is infinite - for HAL too.

latCutOff = 50.6; lon1=-130; lon2=-128.25 # a latitude cut-off value to 
#  divide the area into northern and southern areas. lon1 and lon2 are to help
#  draw a line on a map.

