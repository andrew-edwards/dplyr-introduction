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

# dataOrig - a dataframe of SIMULATED survey data spanning 2003 to 2014. Each row
#  represents a hook that caught a species of fish. The columns will be
#  explained as we go along.

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


# dplyr has the function: select

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



# dplyr function to work on rows: filter
#  To remember which is which: filteR for Rows, seleCt for Columns.

# Usual R, how to filter the dataframe to just have data for year 2003?

?

# And how about just to have the data for year 2003, station number 2001, skate
#  number 1 and species code 614:

?

# Using filter:

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

# Note that:

dummy = filter(data, year == 2003, station %in% c(2001, 2020), skate %in% c(1,2), species %in% c(614, 442))

dummy

# I think filter retains the row order of the original dataframe.

# Note that this gives the same dataframe, but with rows reordered, because we
#  filtered the rows in a different order:

dummy2 = filter(data, year == 2003, skate %in% c(1,2), species %in% c(614, 442), station %in% c(2001, 2020))

dummy2



# So {\tt data} is a \Sexpr{dim(data)[1]}$\times$\Sexpr{dim(data)[2]} (local) dataOrig frame. Each row is a unique hook, with some that have no catch (species 999) to give a {\tt hooksPerSkate} for that skate. Has already been arranged in order of year and station. See {\tt iphc0314.Snw} for further details.

# \subsection{Check that bait field only changes for 2012}

data %>% group_by(year, bait) %>% tally()  

# That's good, only 110 (chum salmon) used until 2011, then that and the other two in 2012. Use just the 110 values later.

# \section{Roll up to skate then set level, with $N_{it}$, 2003-2014}

# Get unique set data from this hooks data set. First need to look at the skate level, and then sum the {\tt hookPerSkate}'s for each set. Also look at bait because that can be summarised at skate level. And add up the counts of a particular species for each skate. See Section \ref{sec:notation} for notation, some of which gets used here. [Some of this could maybe have been done in {\tt iphc0314.Snw}, but think I may have kept variable names the same in 20-hook analysis, so keep this all here to avoid any potential confusion.]


# Now to look at a particular species:
# Comment in one of these lines (shortName is for figure legends):
# spCode=401; spName="Redbanded Rockfish"; shortName = "Redbanded"; spCodeIphc=90; spAbbr = RBR
spCode=442; spName="Yelloweye Rockfish"; shortName = "Yelloweye"; spCodeIphc=89; spAbbr = "No YYR" 
# spCode = 614; spName = "Pacific Halibut"; shortName = "Halibut"; spCodeIphc=1; spAbbr = "HAL" 
# spCode = 044; spName = "North Pacific Spiny Dogfish"; shortName = "Dogfish"; spCodeIphc=54; spAbbr = "DOG"    # Get error in bca.ci, expected adjustment 'w' is infinite - for HAL too.

latCutOff = 50.6; lon1=-130; lon2=-128.25 # a latitude cut-off value to 
#  divide the area into northern and southern areas. lon1 and lon2 are to help
#  draw a line on a map.


# Maybe look at?:
# skateSumm = select(data, year, station, skate, hooksPerSkate,
#    obsHooksPerSkate, effSkate, bait)
# skateUniqHooks = unique(skateSumm)         # Picks unique rows
# Now do with group_by, since then can automatically do the count for the species
skateUniqHooks = summarise(group_by(data, year, station, skate), 
   hooksPerSkate = unique(hooksPerSkate), 
   obsHooksPerSkate = unique(obsHooksPerSkate),
   bait = unique(bait), effSkate = unique(effSkate), 
   fishPerSkate = sum( (species == spCode) * catchCount))  

if( dim(unique(select(data, year, station, skate)))[1] != dim(skateUniqHooks)[1])
                                     { stop("check skateUniqHooks")}
skateUniqHooks = mutate(skateUniqHooks, 
    chumObsHooksPerSkate =  obsHooksPerSkate * (bait == 110),
    chumFishPerSkate =  fishPerSkate * (bait == 110))
                             # Add on field for no. hooks with chum, and how 
                             #  how many fish of spCode those hooks caught 
                             # (Just 2012 for now, will maybe be 2014+ - no)
data.frame(head(filter(skateUniqHooks, year == 2012, station == 2002)))  
if( diff(range(select(filter(skateUniqHooks, year != 2012), obsHooksPerSkate)
      - select(filter(skateUniqHooks, year != 2012), chumObsHooksPerSkate) ))!=0)
                              {stop("check skateUniqHooks outside year 2012")}
setUniqHooks = summarise( group_by(skateUniqHooks, year, station),
    H_itOut = sum(hooksPerSkate), H_itObs = sum(obsHooksPerSkate), K_itAll = n(),
    K_itChum = sum(bait == 110), effSkate = unique(effSkate), 
    H_itObsChum = sum(chumObsHooksPerSkate), N_itAll = sum(fishPerSkate), 
    N_itChum = sum(chumFishPerSkate))
    # Unique sets, from hooks data, H_itOut is as I originally had and is the
    #  hooks that go out (which I need for the numbering for first 20),H_itObs is
    #  then just for the observed hooks, using new values. Will give error if
    #  unique(effSkate) not a single value. H_itObsChum is observed hooks
    #  that had chum on. K_itAll is number of skates (all bait), K_itChum is
    #  number of skates with chum. N_itAll is number of spCode fish in set
    #  using all skates, N_itChum is just based on chum. 
if( dim(unique(select(data, year, station)))[1] != dim(setUniqHooks)[1])
                                     { stop("check setUniqHooks")}
# Add column to make effSkateChum only be based on Chum bait:
setUniqHooks = mutate(setUniqHooks, effSkateChum = effSkate * 
    H_itObsChum / H_itObs)
setUniqHooks = arrange(setUniqHooks, year, station) # in case didn't stay in order
# Early effSkateChum won't change:
# data.frame(head(setUniqHooks))
# 2012 effSkateChum now relate to number of observed chum-bait hooks (next section)
data.frame(head(filter(setUniqHooks, year == 2012)))
# For 2012, all K_itChum values seem to be 4:
summary((filter(setUniqHooks, year == 2012))$K_itChum)

# Can see above that for 2012, station 2002, the three Redbanded were caught on non-Chum skates. 

# \subsection{Check that {\tt effSkateChum} is somewhat consistent with number of observed chum-bait hooks}
 
# Estimate the effective skate number based on just the number of observed chum-bait hooks, then look at examples that differ from given values by $>1$ or $>0.5$.

setUniqHooksSamp = mutate(setUniqHooks, estEffSkateChum = H_itObsChum/100)
as.data.frame(filter(setUniqHooksSamp, abs(estEffSkateChum - effSkateChum) > 1))
as.data.frame(filter(setUniqHooksSamp, abs(estEffSkateChum - effSkateChum) >0.5))

# Only a few examples (others should have been sorted out in manual tidying up before), I'd thought there might be more. None look too bad, station 2101 suggests a typo, but leave for now as not hugely in error.

# And for 2012, it shouldn't be much more than 4:

summary(filter(setUniqHooks, year == 2012)$effSkateChum)  

# Looks okay.

# \subsection{So we have {\tt setUniqHooks} okay, with counts for \Sexpr{spName}; now define notation}\label{sec:notation}

setUniqHooks

# \subsection{Create index value for each year}

# Simplify to give a set-level dataframe with the correct terminology, containing just what is needed now:

setVals0314 = select(setUniqHooks, year=year, station=station, # H_it = H_itObsChum, 
    E_it = effSkateChum, N_it = N_itChum)
setVals0314 = mutate(setVals0314, C_it = N_it / E_it)
setVals0314


# \subsubsection{Filter locations geography, to determine {\tt setVals0314keep}} 
         % Need all the setVals0314 for the maps

# stationLocs0314 - a dataframe corresponding to each survey station 
#  (called a  station), giving the longitude and latitude for each station, with
#  an allowable range. The locations do not change each year (so there is no
#  year in here).


stationLocs0314[stationLocs0314$lat < latCutOff, "keep"] = 0    # don't keep some

setVals0314keep= filter(setVals0314, station %in% 
    filter(stationLocs0314, keep == 1)$station)  # Keep only stations in geog. region
# data = filter(dataOrig, station %in% 
#    filter(stationLocs0314, keep == 1)$station)  # Keep only stations in geog. region

yearVals0314keep = summarise(group_by(setVals0314keep, year), n_t = n(), 
    setsWithSp = sum(N_it != 0), sum.C_it = sum(C_it))

#% For Redbanded:
#% So after doing everything at the hook-by-hook level, making corrections, accounting for bait, etc., compared to the same calculations at the set-by-set level (a few weeks ago):

#% {\tt setsWithSp} is the same, except 69 and 63 now was 70 and 71 for 2011 and 2012.

#% {\tt sum.C\_it} is exactly the same for 2003, 2005, 2007-2010, $<1$ different for 2004, 2006, and for 2011 was 162.44 (just $>1$ less and for 2012 was 141.20, so 20 less. The final value is clearly important to have correct -- set-level calculation didn't account for the bait experiment, and couldn't do because the data was already rolled up at the set level.

#<<index>>=
yearVals0314keep = mutate(yearVals0314keep, I_t = sum.C_it / n_t)
yearVals0314keep
 


# For RBR: So the lowest year is 2007, whereas original calculation from the set-level data had 2012 as lowest. Note that can't use the {\tt setsWithSp} values here for 2012 to compare to other years because less skates with Chum so less to choose from -- would have to calculate something like that at the skate level, to look at proportion of empty skates.


# \section{Now combine for sets keeping for 1995, 1996 and 2003+}
setValsKeep = rbind(setVals1995keep, setVals1996keep, setVals0314keep)

# % This was from IPHCjoin6.Snw.   Want it for the bootstrapping. And I think for the Keep for 03-12

# \section{How many stations never catch a \Sexpr{spName} for 2003 onwards?}

# % Modifying from the 20-hook version, which presumably came from earlier full hook version. Add ..keep to things here.

# Now to investigate stations that never catch a \Sexpr{spName} for the stations being considered. Do for 2003$+$ ({\tt zeroStations0314}) etc, and easy to just filter on the 1995 and 1996 data. [At one point may have had {\tt zeroStations} containing all, but that doesn't make sense since names aren't consistent between years, so maybe things just got moved around].
# % , but be aware that station names are different in 1995, 1996 and for 2003$+$. Check that none overlap:% Should do as map, not doing figure as gets messed up with funny station names for 1995 (and presumably 1996) data.

#<<zeroStations0314>>=
# THESE WERE ALREADY COMMENTED....
# yyy1 = intersect(filter(setValsKeep, year == 1995)$station, 
#    filter(setValsKeep, year == 1996)$station) 
#yyy2 = intersect(filter(setValsKeep, year == 1995)$station, 
#    filter(setValsKeep, year == 2003)$station) 
# yyy3 = intersect(filter(setValsKeep, year == 1996)$station, 
#    filter(setValsKeep, year == 2003)$station) # stations are same for 2003 onwards
# if(length(yyy1) + length(yyy2) + length(yyy3) > 0) stop("check station names")
# stationValsKeep = summarise(group_by(setValsKeep, station), totalN_it = sum(N_it))
# postscript("stationValsKeep.eps", height = figheight, width = figwidth,
#           horizontal=FALSE,  paper="special")  
# plot(filter(stationValsKeep, totalN_it > 0), col="blue", xlab="Station", 
#     ylab="Numbers caught in each station from 2003 onwards")
# points(filter(stationValsKeep, totalN_it == 0), col="red", pch=20)
# dev.off()
# END OF WHAT WAS ALREADY COMMENTED
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

