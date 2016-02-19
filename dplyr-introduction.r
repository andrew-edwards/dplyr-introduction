# dplyr-introduction.r. For dplyr talk at PBS R workshop. This code is designed
#  to be stepped through one line at a time (probably with ctrl-Enter,
#  depending on how you are running R), and the user should read the
#  comments to help explain it. Towards the end it may get a little complicated
#  as I didn't go through all that in the workshop, but I didn't have time
#  to make a simpler example. But it should still give you an idea of what
#  dplyr can be used for. This all works fine with R version 3.2.3. 
#  Andrew Edwards. [Adapted from ipchSerBallHooksYYR.Snw.]
#  15th February 2016.

# This code will demonstrate some of the features of the R package
#  dplyr applied to a dataset that has been generated, based on
#  the format of a real dataset (so I don't have permission to make the
#  true dataset public).

rm(list=ls())

require(dplyr)
require(PBSmapping)             # for maps. 

# *** START OF SET UP ***
#  Just some function for the maps. Source these, just don't worry
#  about the details.

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

# source("s_dplyr_funcs.r")     # helper functions that allow string arguments,
                              #  by Sebastian Kranz. (Don't think actually use
                              #  here, but they can be useful).

# *** END OF SET UP ***

# Load in data set.

load("dummyData.RData")               # Loads in dataOrig: hook-by-hook data,
                                      #  and blockLocs0314: locations of
                                      #  stations 

ls()

# Plot a map of the stations - locations of the survey:
expandPlot(mfrow=c(1,1),mar=c(1.8,2,1.0,0.5))
plotBC(main = "All 2003-2014 stations")  #zlev=100)
points(lat~lon, data=stationLocs0314, col="blue", cex=1) # , cex=0.6

# dataOrig - a dataframe of SIMULATED longline survey data spanning 2003 to
#  2014.
# Each row represents a hook that caught a species of fish. The columns will be
#  explained as we go along. For now, the imporant information is:
# At each station (location) a set of up to eight
#  skates is put in the water. Each skate consists of up to (approximately)
#  100 hooks. When the longline is pulled back in, any fish caught on a hook
#  is identified and recorded for that hook. Thus, each row corresponds to
#  a hook on which a fish was caught, with columns identifying the year,
#  block (station), skate, hook, species, and more.


# Now to look at the hook-by-hook data set:

# I did this in the workshop, but only uncomment it if you know how to
#  interrupt your R (ctrl-C maybe):
# dataOrig                       # Not advisable due to size!

head(dataOrig)

summary(dataOrig)

dim(dataOrig)

class(dataOrig)

# dplyr function: tbl_df()
dataOrig = tbl_df(dataOrig)   

class(dataOrig)               # Note the extra attributes now.

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
#     '[' Never simplifies (drops), so always returns data.frame
#  [not sure what that last lines means]

# So dataOrig is still a dataframe, it just has extra attributes.

# First we want to rename the column 'block' with the more intuitive name
#  'station'. How to do that usually in R?

# In the workshop we tried something like:
#  colnames(dataOrig[] == "block") = "station"
# but it didn't work.

# dplyr function: rename()
dataOrig = rename(dataOrig, station = block)

dataOrig

# Okay, in R how would we usually select just the columns with
#  names year, station and species?

dummy = dataOrig[,c("year", "station", "species")]
                                        # Fairly easy, but need punctuation

# And how would we remove some un-needed columns? Often datasets have columns
#  that aren't relevant to our analyses (or at least we first think they aren't).
#  How to remove the columns with names:
#  tripID, hookID, skateID, set, OLDobsHooksPerSkate, direction

# Goofy? We kind of tried in the workshop.


# dplyr function: select()

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

dummy = subset(dataOrig, year ==2003)

# And how about just to have the data for year 2003, station number 2001, skate
#  number 1 and species code 614:

dummy = subset(dataOrig, year ==2003 & station == 2001 & skate == 1 & species == 614)

# See ?subset  - apparently not recommended for analysing data! Thanks Matt.

# Using filter():

dummy = filter(data, year == 2003)

dummy

dummy = filter(data, year == 2003, station == 2001, skate == 1, species == "614")

dummy

# Can also use | for 'or':

dummy = filter(data, year == 2003 | skate == 1)

dummy

summary(dummy)

# And %in%:

dummy = filter(data, year %in% 2003:2010)  # and also: one_of

dummy

# ****Big warning for filter****
#  If you do not make your dataframe a tbl_df then the filter command
#   from base R will be used, which will create an error because the
#   options are not the same as the dplyr version. I think. I just
#   tried to create a simple demonstration:
xx = data.frame(value = c(1,2,3,4))
filter( xx, value == 2)
#   but that seems to work okay, but I'm sure this issue has foxed me in the
#   past, because I had forgotten to use tbl_df(). Note that, above, because
#   dataOrig was already a tbl_df, then data was as one, because data was
#   formed by:
#    data = select(dataOrig, -c(tripID, hookID, skateID, set,
#         OLDobsHooksPerSkate, direction))


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

dummy = distinct(select(data, species))

dummy

dummy = distinct(select(data, year, species))

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


# We want to get the total number of Yelloweye Rockfish caught in each year.

# catchCount is the number of a species caught on the hook (usually just 1):
summary(as.factor(data$catchCount))

# First set spCode to be the species code, so we can just comment in ONE of
#  the following lines here, and change it to look at another species
#  (good programming practice - we just change one line to change our species).
spCode=442; spName="Yelloweye Rockfish"; shortName = "Yelloweye" 
# spCode=401; spName="Redbanded Rockfish"; shortName = "Redbanded"
# spCode = 614; spName = "Pacific Halibut"; shortName = "Halibut"; 
# spCode = 044; spName = "North Pacific Spiny Dogfish"; shortName = "Dogfish"


# Okay, so in usual R how would we get the total number of Yelloweye Rockfish
#  (species code 442) caught in each year? So for each year, we need to add
#  up the catchCount values for which the species code is 442.

# We tried something like:
# dummy = sapply(unique(   , ...) blah blah ) 
# dummy = aggregate(catchCount | years, data=data, subset(


# dplyr functions: summarise() and group_by()

# I don't really use group_by on it's own, but it's helpful here to see
#  that is just adds groups to the dataframe:
dummy = group_by(data, year)

dummy

class(dummy)

# So we grouped by year, and there are 11 groups, since we have 11 years:
length(unique(data$year))

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
catchSum2 = summarise(group_by(data, year), species = spCode,
    total = sum(catchCount) ) 

catchSum2

# summarise() works on 'aggregate' functions, which take a vector of values
#  and return a single number. Such as min(), max(), mean() etc. Some extra
#  ones in dplyr are:

# n() - the number of observations in the current group
# n_distinct(x) - number of unique values in x
# first(x), last(x), and nth(x, n).



# We now want to roll up the data to the skate and then set level, and then
#  obtain a catch rate per set, and then an average catch rate per set for
#  each year.

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

data2 = data           # Create a second data set, and change one of the values
data2[1,"effSkate"] = 7
select(data2, year, station, skate, hook, effSkate)
                       # So effSkate is now not the same for all hooks from skate 1
                       #  at station 2001 in year 2003.

# So if I do the above command on data2 instead of data, then should get an
#  error:
dummy = summarise(group_by(data2, year, station), unique(effSkate))

# So that's a check that our dataframe is what we expect - we have a row
#  for each hook, but hooks from the same set all have the same effSkate
#  number.

# I first want to roll up the data to the skate level [bait is the bait used
#  for that skate]:

skateUniqHooks = summarise(group_by(data, year, station, skate), 
   hooksPerSkate = unique(hooksPerSkate), 
   obsHooksPerSkate = unique(obsHooksPerSkate),
   bait = unique(bait), effSkate = unique(effSkate), 
   fishPerSkate = sum( (species == spCode) * catchCount))
# So each row is a skate. 
# The last line calculates fishPerSkate, the number of species 'spCode'
#  caught on that skate (in that year at that station). Remember skates are
#  numbered from 1 to 8 within each set. 

# This is a check that will fail if I messed up somewhere:
if( dim(unique(select(data, year, station, skate)))[1] != dim(skateUniqHooks)[1])
                                     { stop("check skateUniqHooks")}


# What is the bait issue?

# This will do groups by year and bait, and then summarise the number of
#  values (rows of skateUniqHooks) for each combination:

summarise(group_by(skateUniqHooks, year, bait), n())

# Can see that every year uses bait 110 (Chum Salmon), except 2010.
#  In 2012 a bait experiment was done:
#  For each set there were four consecutive skates
#   with Chum Salmon, one skate with Pink Salmon and one skate with Walleye
#   Pollock each, and two empty skates. It's possible that the catch rates
#   are effected by the bait, which is why we are still working at the skate
#   level.

# Note that the above command can also be done using piping (as Matt showed)
#  and also shows the tally() function.
skateUniqHooks %>% group_by(year, bait) %>% tally()  

# So now we just want to have hook numbers and catch numbers for when
#  Chum Salmon (code 110) was used as bait:

skateUniqHooks = mutate(skateUniqHooks, 
    chumObsHooksPerSkate =  obsHooksPerSkate * (bait == 110),
    chumFishPerSkate =  fishPerSkate * (bait == 110))


# Let's look at one station from 2012. Use data.frame command to force it to
#  display all the columns, and head just displayes the first 6).
data.frame(head( filter(skateUniqHooks, year == 2012, station == 2002)))
# Can see that skate 1 had bait species 109, skates 3-6 had chum (110),
#  skate 8 had bait species 19, and there were no skates 2 or 7 [though more
#  accurately (I think), no skates 2 or 7 that caught spCode - such skates
#  I think will have been dropped in our data. For the actual assessment I
#  had to use a separate dataframe of the stations to account for this].

# Now to round up to the set level:

setUniqHooks = summarise( group_by(skateUniqHooks, year, station),
    H_itOut = sum(hooksPerSkate), H_itObs = sum(obsHooksPerSkate), K_itAll = n(),
    K_itChum = sum(bait == 110), effSkate = unique(effSkate), 
    H_itObsChum = sum(chumObsHooksPerSkate), N_itAll = sum(fishPerSkate), 
    N_itChum = sum(chumFishPerSkate))
    # Unique sets, from hooks data. Notation is now to match the mathematical
    #   notation in our stock assessments, with subscript i being set and
    #   t being year.
    #  H_itOut is number of hooks that were put out (for set i and year t).
    #  H_itObs is the same for those that got observed.
    #  
    #  H_itObsChum is observed hooks that had chum on.
    #  K_itAll is number of skates (all bait).
    #  K_itChum is number of skates with chum.
    #  N_itAll is number of spCode fish in the set
    #   using all skates, N_itChum is just based on chum.

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

# For 2012, all K_itChum values are 4 (because the survey design was to
#  always have 4 skates with chum):
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


# Now want to get an index for each year:
yearVals0314 = summarise(group_by(setVals0314, year), n_t = n(), 
    setsWithSp = sum(N_it != 0), sum.C_it = sum(C_it))
# n_t is the number of sets in each year
# setsWithSp is the number of sets each year that did have the species.
# And the index I_t is the mean catch rate across all sets (so the total
#  of the set-level catch rates divided by the number of sets, for each year).
yearVals0314 = mutate(yearVals0314, I_t = sum.C_it / n_t)

yearVals0314

windows()      # I don't want to delete the earlier map
plot(yearVals0314$year, yearVals0314$I_t, xlab="Year",
     ylab="Catch rate index (numbers per effective skate)",
     ylim=c(0, max(yearVals0314$I_t)), type="o")

dev.off()

# Now back to the map, show which stations never caught the species.
stationVals0314 = summarise(group_by(setVals0314, station),
    totalN_it = sum(N_it))
zeroStations0314 = filter(stationVals0314, totalN_it == 0)$station
                                        # Stations that always have zero catch
nonZeroStations0314 = filter(stationVals0314, totalN_it > 0)$station
                                        # Stations with catch
length(zeroStations0314)

# stationLocs0314 - a dataframe corresponding to each survey station ,
#  giving the longitude and latitude for each station, with
#  an allowable range. The locations do not change each year (so there is no
#  year in here).
#
head(stationLocs0314)
# We already know that station numbers are the same as in the catch dataset we looked at.

# Show on the earlier map the stations that never caught the species
points(lat~lon, 
      data=filter(stationLocs0314, block %in% nonZeroStations0314),
      col="blue", pch=20, cex=1.2) 
legend("bottomleft", legend=c(paste("Never caught", shortName), 
       paste("Have caught", shortName)),
       pch=c(1, 20), pt.cex=c(1, 1.2), col=c("blue", "blue"))

