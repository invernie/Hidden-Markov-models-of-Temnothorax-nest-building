##########################################################################################################################################
# Stone density data #
##########################################################################################################################################

library(readr)

# use this to make the combined data file
#S.dt <- read_csv("pathToFile/Deposition_data.csv")
#S.dt$ROI <- as.factor(S.dt$ROI)
#View(S.dt)
#dt <- S.dt

# use this to make the by-colony matrices to pass to calcROIrate()
dt <- read_csv("pathToFile/all_building_data.csv")
dt$ROI <- as.factor(dt$ROI)

D54 <- dt[which(dt$event=='deposition' & dt$Colony=='R54'),]                                # deposition events (stone carried from outside the ROI)
P54 <- dt[which(dt$event=='pick-up' & dt$behaviour_type!='within_ROI' & dt$Colony=='R54'),] # pick-up events (stone carried off outside the ROI)
DP54 <- dt[which(dt$behaviour_type=='within_ROI' & dt$Colony=='R54'),]                      # deposition and pick-up events (stone picked-up and deposited within the ROI)
DT54 <- rbind(D54,DP54)                                                                     # all deposition events
PT54 <- rbind(P54,DP54)                                                                     # all pick-up events

D34 <- dt[which(dt$event=='deposition' & dt$Colony=='R34'),]
P34 <- dt[which(dt$event=='pick-up' & dt$behaviour_type!='within_ROI' & dt$Colony=='R34'),]
DP34 <- dt[which(dt$behaviour_type=='within_ROI' & dt$Colony=='R34'),]
DT34 <- rbind(D34,DP34)
PT34 <- rbind(P34,DP34)

D29 <- dt[which(dt$event=='deposition' & dt$Colony=='R29'),]
P29 <- dt[which(dt$event=='pick-up' & dt$behaviour_type!='within_ROI' & dt$Colony=='R29'),]
DP29 <- dt[which(dt$behaviour_type=='within_ROI' & dt$Colony=='R29'),]
DT29 <- rbind(D29,DP29)
PT29 <- rbind(P29,DP29)

D05 <- dt[which(dt$event=='deposition' & dt$Colony=='R5'),]
P05 <- dt[which(dt$event=='pick-up' & dt$behaviour_type!='within_ROI' & dt$Colony=='R5'),]
DP05 <- dt[which(dt$behaviour_type=='within_ROI' & dt$Colony=='R5'),]
DT05 <- rbind(D05,DP05)
PT05 <- rbind(P05,DP05)

Col54 <- rbind(D54,P54,DP54)
Col34 <- rbind(D34,P34,DP34)
Col05 <- rbind(D05,P05,DP05)
Col29 <- rbind(D29,P29,DP29)


###########################################################################################################################################
# Brood data #
##########################################################################################################################################

## Create brood data frame from excel workbook and calculate distance centre-of-ROI to centre-of-cluster using centroid coordinates.

library(readxl)

brPath <- "pathToFile/brood_measurements - colonies_R34-R54-R29-R5 - anonymous measurers.xlsx"

calcDistance <- function(Br.dt, x.r, y.r) {
   
   x.b <- as.numeric(Br.dt["X_centroid_1.5-ant"])
   y.b <- as.numeric(Br.dt["Y_centroid_1.5-ant"])
   
   d <- sqrt( (x.r - x.b)^2 + (y.r - y.b)^2 )
   
   return(d)
   
}

Br05 <- read_excel(brPath, sheet = "R5")
Br29 <- read_excel(brPath, sheet = "R29")
Br34 <- read_excel(brPath, sheet = "R34")
Br54 <- read_excel(brPath, sheet = "R54")


# calculate distance of brood centroid from each ROI at each timepoint
coordPath <- "pathToFile/ROI_coordinates.xlsx"
coord.R05 <- read_excel(coordPath, sheet = "R5")
coord.R29 <- read_excel(coordPath, sheet = "R29")
coord.R34 <- read_excel(coordPath, sheet = "R34")
coord.R54 <- read_excel(coordPath, sheet = "R54")

pxmm.R05 <- 11.54 #pixels per mm
pxmm.R29 <- 11.8
pxmm.R34 <- 11.60445
pxmm.R54 <- 11.36215

x5.1 <- as.numeric(coord.R05[coord.R05$ROI == "1", "x"])
y5.1 <- as.numeric(coord.R05[coord.R05$ROI == "1", "y"])
x5.2 <- as.numeric(coord.R05[coord.R05$ROI == "2", "x"])
y5.2 <- as.numeric(coord.R05[coord.R05$ROI == "2", "y"])
x5.3 <- as.numeric(coord.R05[coord.R05$ROI == "3", "x"])
y5.3 <- as.numeric(coord.R05[coord.R05$ROI == "3", "y"])
Br05$d_from_ROI_1 <- apply(Br05, 1, calcDistance, x.r = x5.1, y.r = y5.1)/pxmm.R05
Br05$d_from_ROI_2 <- apply(Br05, 1, calcDistance, x.r = x5.2, y.r = y5.2)/pxmm.R05
Br05$d_from_ROI_3 <- apply(Br05, 1, calcDistance, x.r = x5.3, y.r = y5.3)/pxmm.R05

x29.1 <- as.numeric(coord.R29[coord.R29$ROI == "1", "x"])
y29.1 <- as.numeric(coord.R29[coord.R29$ROI == "1", "y"])
x29.2 <- as.numeric(coord.R29[coord.R29$ROI == "2", "x"])
y29.2 <- as.numeric(coord.R29[coord.R29$ROI == "2", "y"])
x29.3 <- as.numeric(coord.R29[coord.R29$ROI == "3", "x"])
y29.3 <- as.numeric(coord.R29[coord.R29$ROI == "3", "y"])
Br29$d_from_ROI_1 <- apply(Br29, 1, calcDistance, x.r = x29.1, y.r = y29.1)/pxmm.R29
Br29$d_from_ROI_2 <- apply(Br29, 1, calcDistance, x.r = x29.2, y.r = y29.2)/pxmm.R29
Br29$d_from_ROI_3 <- apply(Br29, 1, calcDistance, x.r = x29.3, y.r = y29.3)/pxmm.R29

x34.1 <- as.numeric(coord.R34[coord.R34$ROI == "1", "x"])
y34.1 <- as.numeric(coord.R34[coord.R34$ROI == "1", "y"])
x34.2 <- as.numeric(coord.R34[coord.R34$ROI == "2", "x"])
y34.2 <- as.numeric(coord.R34[coord.R34$ROI == "2", "y"])
x34.3 <- as.numeric(coord.R34[coord.R34$ROI == "3", "x"])
y34.3 <- as.numeric(coord.R34[coord.R34$ROI == "3", "y"])
Br34$d_from_ROI_1 <- apply(Br34, 1, calcDistance, x.r = x34.1, y.r = y34.1)/pxmm.R34
Br34$d_from_ROI_2 <- apply(Br34, 1, calcDistance, x.r = x34.2, y.r = y34.2)/pxmm.R34
Br34$d_from_ROI_3 <- apply(Br34, 1, calcDistance, x.r = x34.3, y.r = y34.3)/pxmm.R34

x54.1 <- as.numeric(coord.R54[coord.R54$ROI == "1", "x"])
y54.1 <- as.numeric(coord.R54[coord.R54$ROI == "1", "y"])
x54.2 <- as.numeric(coord.R54[coord.R54$ROI == "2", "x"])
y54.2 <- as.numeric(coord.R34[coord.R54$ROI == "2", "y"])
x54.3 <- as.numeric(coord.R54[coord.R54$ROI == "3", "x"])
y54.3 <- as.numeric(coord.R54[coord.R54$ROI == "3", "y"])
Br54$d_from_ROI_1 <- apply(Br54, 1, calcDistance, x.r = x54.1, y.r = y54.1)/pxmm.R54
Br54$d_from_ROI_2 <- apply(Br54, 1, calcDistance, x.r = x54.2, y.r = y54.2)/pxmm.R54
Br54$d_from_ROI_3 <- apply(Br54, 1, calcDistance, x.r = x54.3, y.r = y54.3)/pxmm.R54

t0.R54 <- min(Col54$time_s)
t0.R34 <- min(Col34$time_s)
t0.R29 <- min(Col29$time_s)
t0.R05 <- min(Col05$time_s)
Br54$`normalised time` <- Br54$time_s-t0.R54
Br34$`normalised time` <- Br34$time_s-t0.R34
Br29$`normalised time` <- Br29$time_s-t0.R29
Br05$`normalised time` <- Br05$time_s-t0.R05

Br.dt <- rbind(Br05,Br29,Br34,Br54)
Br.dt$measurer <- as.factor(Br.dt$measurer)

View(Br.dt)


############
# distance #
############
par(mfrow = c(2,2))
plot(y = Br05$d_from_ROI_1, x = Br05$`normalised time`, type = 'l', xlab = "time from start of activity (s)", ylab = "distance of area 1\nfrom center of cluster (mm)")
plot(y = Br29$d_from_ROI_1, x = Br29$`normalised time`, type = 'l', lty = 2)
plot(y = Br34$d_from_ROI_1, x = Br34$`normalised time`, type = 'l', lty = 3)
plot(y = Br54$d_from_ROI_1, x = Br54$`normalised time`, type = 'l', lty = 4)


########################################################################################################
#                                 Merge stone density data with brood data                             #
########################################################################################################

library(dplyr)

combined.dt <- S.dt
for (obsn in 1:nrow(combined.dt)) {
  
   obst <- as.numeric(combined.dt[ obsn , "time_s" ])
   obscol <- as.character(combined.dt[ obsn, "Colony"])
   matchrow <- which( (Br.dt$time_s - obst)^2 == min( (Br.dt[ which( Br.dt$colony == obscol) , "time_s"] - obst )^2 ) & Br.dt$colony == obscol, arr.ind = T)
   if (length(matchrow) > 1) { # sometimes, the time of the event is half-way between the earlier and the later time point used for brood data collection. In these cases, always use the earlier.
     matchrow = matchrow [1]
   }
   combined.dt[ obsn , "cluster_area_1.5-ant" ] <- Br.dt[ matchrow , "cluster_area_mm" ]
   combined.dt[ obsn , "standardised_cluster_area_1.5-ant" ] <- Br.dt[ matchrow , "standardised_cluster_area_1.5-ant" ]
   combined.dt[ obsn , "n_ants_in_cluster_1.5-ant" ] <- Br.dt[ matchrow , "n_ants_in_cluster_1.5-ant" ]
   combined.dt[ obsn , "standardised_n_ants_in_cluster_1.5-ant" ] <- Br.dt[ matchrow , "standardised_n_ants_in_cluster_1.5-ant" ]
   combined.dt[ obsn , "X_centroid_1.5-ant" ] <- Br.dt[ matchrow , "X_centroid_1.5-ant" ]
   combined.dt[ obsn , "Y_centroid_1.5-ant" ] <- Br.dt[ matchrow , "Y_centroid_1.5-ant" ]
   combined.dt[ obsn , "cluster_density_1.5-ant" ] <- Br.dt[ matchrow , "cluster_density_1.5-ant" ]
   combined.dt[ obsn , "measurer" ] <- Br.dt[ matchrow , "measurer" ]
 }


# normalise the time so that the start point is the first building event (rather than the start of the video)
t0.R34 <- min(combined.dt[combined.dt$Colony == "R34","time_s"])
t0.R54 <- min(combined.dt[combined.dt$Colony == "R54","time_s"])
t0.R05 <- min(combined.dt[combined.dt$Colony == "R5","time_s"])
t0.R29 <- min(combined.dt[combined.dt$Colony == "R29","time_s"])

combined.dt[ which(combined.dt[,"Colony"] == "R34") , "normalised time"] = combined.dt[ which(combined.dt[,"Colony"] == "R34") , "time_s"] - t0.R34
combined.dt[ which(combined.dt[,"Colony"] == "R54") , "normalised time"] = combined.dt[ which(combined.dt[,"Colony"] == "R54") , "time_s"] - t0.R54
combined.dt[ which(combined.dt[,"Colony"] == "R5") , "normalised time"] = combined.dt[ which(combined.dt[,"Colony"] == "R5") , "time_s"] - t0.R05
combined.dt[ which(combined.dt[,"Colony"] == "R29") , "normalised time"] = combined.dt[ which(combined.dt[,"Colony"] == "R29") , "time_s"] - t0.R29


View(combined.dt)
write.csv(combined.dt, file = "pathToFile/all_building_data.csv", row.names = FALSE )


#################
# stone density #
#################

# after creating the combined data file, for data visualisation
combined.dt <- dt

dt05 = combined.dt[combined.dt$Colony == "R5", ] 
dt29 = combined.dt[combined.dt$Colony == "R29", ] 
dt34 = combined.dt[combined.dt$Colony == "R34", ] 
dt54 = combined.dt[combined.dt$Colony == "R54", ] 

# Visualise how stone density changes over time
par(mfrow = c(2,2))
plot(y = dt05$stone_density, x = dt05$`normalised time`, ylab = "stone density", xlab = "time from start of activity (s)", main = "R05")
plot(y = dt29$stone_density, x = dt29$`normalised time`, col = 'red', ylab = "stone density", xlab = "time from start of activity (s)", main = "R29")
plot(y = dt34$stone_density, x = dt34$`normalised time`, col = 'green', ylab = "stone density", xlab = "time from start of activity (s)", main = "R34")
plot(y = dt54$stone_density, x = dt54$`normalised time`, col = 'blue', ylab = "stone density", xlab = "time from start of activity (s)", main = "R54")

par(mfrow = c(1,1))

############
# distance #
############

par(mfrow = c(2,2))

plot(y = dt05[dt05$ROI == "1", "d_ROI_to_centroid_mm"], x = dt05[dt05$ROI == "1", "normalised time"], ylim = c(min(dt05$d_ROI_to_centroid_mm), max(dt05$d_ROI_to_centroid_mm)), xlim = c(min(dt05$`normalised time`), max(dt05$`normalised time`)), ylab = "distance", xlab = "time from start of activity (s)", main = "R05")
points(y = dt05[dt05$ROI == "2", "d_ROI_to_centroid_mm"], x = dt05[dt05$ROI == "2", "normalised time"], col = "red")
points(y = dt05[dt05$ROI == "3", "d_ROI_to_centroid_mm"], x = dt05[dt05$ROI == "3", "normalised time"], col = "blue")

plot(y = dt29[dt29$ROI == "1", "d_ROI_to_centroid_mm"], x = dt29[dt29$ROI == "1", "normalised time"], ylim = c(min(dt29$d_ROI_to_centroid_mm), max(dt29$d_ROI_to_centroid_mm)), xlim = c(min(dt29$`normalised time`), max(dt29$`normalised time`)), ylab = "distance", xlab = "time from start of activity (s)", main = "R29")
points(y = dt29[dt29$ROI == "2", "d_ROI_to_centroid_mm"], x = dt29[dt29$ROI == "2", "normalised time"], col = "red")
points(y = dt29[dt29$ROI == "3", "d_ROI_to_centroid_mm"], x = dt29[dt29$ROI == "3", "normalised time"], col = "blue")

plot(y = dt34[dt34$ROI == "1", "d_ROI_to_centroid_mm"], x = dt34[dt34$ROI == "1", "normalised time"], ylim = c(min(dt34$d_ROI_to_centroid_mm), max(dt34$d_ROI_to_centroid_mm)), xlim = c(min(dt34$`normalised time`), max(dt34$`normalised time`)), ylab = "distance", xlab = "time from start of activity (s)", main = "R34")
points(y = dt34[dt34$ROI == "2", "d_ROI_to_centroid_mm"], x = dt34[dt34$ROI == "2", "normalised time"], col = "red")
points(y = dt34[dt34$ROI == "3", "d_ROI_to_centroid_mm"], x = dt34[dt34$ROI == "3", "normalised time"], col = "blue")

plot(y = dt54[dt54$ROI == "1", "d_ROI_to_centroid_mm"], x = dt54[dt54$ROI == "1", "normalised time"], ylim = c(min(dt54$d_ROI_to_centroid_mm), max(dt54$d_ROI_to_centroid_mm)), xlim = c(min(dt54$`normalised time`), max(dt54$`normalised time`)), ylab = "distance", xlab = "time from start of activity (s)", main = "R54")
points(y = dt54[dt54$ROI == "2", "d_ROI_to_centroid_mm"], x = dt54[dt54$ROI == "2", "normalised time"], col = "red")
points(y = dt54[dt54$ROI == "3", "d_ROI_to_centroid_mm"], x = dt54[dt54$ROI == "3", "normalised time"], col = "blue")

####################################
# Looking at rate change over time #
####################################
################
# rates by ROI #
################

# 15 min interval
n = 15      # per minute rate
intv = n*60

calcRate <- function(partial_series = 0, tmax = 0, tmin=0, intv, D.dt, P.dt, Br.dt, ROI = NA) {
   # function arguments:
   #    partial_series = whether we are using the full time series (0) or a subset (1)
   #    tmax = end time point of time series subset (s). uses normalised time. always required.
   #    tmin = start time point of time series subset (s). uses normalised time.
   #    intv = length of interval (s) over which each rate value is calculated
   #    D.dt = deposition data set
   #    P.dt = pick-up data set
   #    Br.dt = brood measurements data
   #    ROI = ROI subset
   #
   #browser()
   if (is.na(ROI) == F) {
      ROI = as.character(ROI)
      D.dt <- D.dt[D.dt$ROI == ROI,]
      P.dt <- P.dt[P.dt$ROI == ROI,]
   }
   full.dt <- rbind(D.dt, P.dt)
   full.dt <- full.dt[order(full.dt$`normalised time`),]
   
   
   if (max(full.dt$`normalised time`)<tmin) {
      
      stop("There are no data points in this time phase")
      
   }
   
   if (partial_series == 0) {
      tmax <- max(full.dt$`normalised time`)
   } else {
      if (max(full.dt$`normalised time`) < tmax) {
         tmax <- max(full.dt$`normalised time`)
      }
   }
   
   
   tpoints <- seq(tmin, tmax, intv)
   ntpoints <- length(tpoints)-1
   Drate <- rep(0,ntpoints)
   Prate <- rep(0,ntpoints)
   meanS <- rep(0,ntpoints)             # mean stone density in time interval
   meanBrArea <- rep(0,ntpoints)        # mean standardised brood cluster area in time interval
   meanNAnts <- rep(0,ntpoints)         # mean standardised n ants in brood cluster in time interval
   meanBrDens <- rep(0,ntpoints)        # mean brood cluster density in time interval
   meanDist <- rep(0,ntpoints)          # mean standardised distance from centre of ROI to centre of brood cluster
   
   x = 1
   last_S = 0
   
   while (x<length(tpoints)) {
      
      t0 = tpoints[x]
      t1 = tpoints[x+1]
      
      brstart <- which.min((Br.dt$`normalised time` - t0)^2) # find the closest brood measurement data point 
      nbrpoints <- round(intv/(15*60))
      brend <- brstart + nbrpoints
      
      Drate[x] = nrow(D.dt[D.dt$`normalised time`>=t0 & D.dt$`normalised time`<t1,])/n # n events per min
      Prate[x] = nrow(P.dt[P.dt$`normalised time`>=t0 & P.dt$`normalised time`<t1,])/n
      event.dt = full.dt[full.dt$`normalised time`>=t0 & full.dt$`normalised time`<t1,]
      m <- nrow(event.dt)
      
      meanS[x] = ifelse( m > 0 , sum(event.dt[, "stone_density"])/m, last_S ) # average stone density
      meanBrArea[x] = ( sum(as.numeric(unlist(Br.dt[ seq(brstart,brend) , "standardised_cluster_area_1.5-ant" ]))))/(nbrpoints+1)
      meanNAnts[x] = ( sum(as.numeric(unlist(Br.dt[ seq(brstart,brend) , "standardised_n_ants_in_cluster_1.5-ant" ]))))/(nbrpoints+1)
      meanBrDens[x] = ( sum(as.numeric(unlist(Br.dt[ seq(brstart,brend) , "cluster_density_1.5-ant" ]))))/(nbrpoints+1)
      
      if (is.na(ROI) == F) {
         colName <- paste0("d_from_ROI_", ROI)
         meanDist[x] = (sum(as.numeric(unlist(Br.dt[seq(brstart,brend), grepl(colName, names(Br.dt))]))))/(nbrpoints+1) # find the correct column based on ROI
      } else {
         dist.v <- unlist(Br.dt[seq(brstart,brend), c("d_from_ROI_1", "d_from_ROI_2", "d_from_ROI_3")]) # mean distance of all ROIs
         meanDist[x] = mean(dist.v)
      }
      if (meanDist[x]==0) browser()
      
      last_S = ifelse(m>0, as.numeric(event.dt[m, "stone_density"]), last_S) # save the last stone density value in this interval to use in case there are no events in the next interval (i.e., stone density remains unchanged)
      
      x = x+1
      
   }
   
   out.rates <- data.frame(D = Drate, P = Prate, stone_dens = meanS, cluster_area = meanBrArea, n_ants = meanNAnts, cluster_dens = meanBrDens, distance = meanDist) 
   
   return( out.rates )
   
}

rates.R54.1 <- calcRate(intv = intv, D.dt = DT54, P.dt = PT54, Br.dt = Br54, ROI = 1)
rates.R54.1$cumulative <- rates.R54.1$D+rates.R54.1$P
rates.R54.1$timepoints <- seq(1,nrow(rates.R54.1))*n
rates.R54.1$ID <- "R54.1"
rates.R54.1$colony <- "R54"
rates.R54.2 <- calcRate(intv = intv, D.dt = DT54, P.dt = PT54, Br.dt = Br54, ROI = 2)
rates.R54.2$cumulative <- rates.R54.2$D+rates.R54.2$P
rates.R54.2$timepoints <- seq(1,nrow(rates.R54.2))*n
rates.R54.2$ID <- "R54.2"
rates.R54.2$colony <- "R54"
rates.R54.3 <- calcRate(intv = intv, D.dt = DT54, P.dt = PT54, Br.dt = Br54, ROI = 3)
rates.R54.3$cumulative <- rates.R54.3$D+rates.R54.3$P
rates.R54.3$timepoints <- seq(1,nrow(rates.R54.3))*n
rates.R54.3$ID <- "R54.3"
rates.R54.3$colony <- "R54"
ratesR54 <- rbind(rates.R54.1, rates.R54.2, rates.R54.3)
ratesR54$colonyID <- as.factor(4)

rates.R34.1 <- calcRate(intv = intv, D.dt = DT34, P.dt = PT34, Br.dt = Br34, ROI = 1)
rates.R34.1$cumulative <- rates.R34.1$D+rates.R34.1$P
rates.R34.1$timepoints <- seq(1,nrow(rates.R34.1))*n
rates.R34.1$ID <- "R34.1"
rates.R34.1$colony <- "R34"
rates.R34.2 <- calcRate(intv = intv, D.dt = DT34, P.dt = PT34, Br.dt = Br34, ROI = 2)
rates.R34.2$cumulative <- rates.R34.2$D+rates.R34.2$P
rates.R34.2$timepoints <- seq(1,nrow(rates.R34.2))*n
rates.R34.2$ID <- "R34.2"
rates.R34.2$colony <- "R34"
rates.R34.3 <- calcRate(intv = intv, D.dt = DT34, P.dt = PT34, Br.dt = Br34, ROI = 3)
rates.R34.3$cumulative <- rates.R34.3$D+rates.R34.3$P
rates.R34.3$timepoints <- seq(1,nrow(rates.R34.3))*n
rates.R34.3$ID <- "R34.3"
rates.R34.3$colony <- "R34"
ratesR34 <- rbind(rates.R34.1, rates.R34.2, rates.R34.3)
ratesR34$colonyID <- as.factor(3)

rates.R29.1 <- calcRate(intv = intv, D.dt = DT29, P.dt = PT29, Br.dt = Br29, ROI = 1)
rates.R29.1$cumulative <- rates.R29.1$D+rates.R29.1$P
rates.R29.1$timepoints <- seq(1,nrow(rates.R29.1))*n
rates.R29.1$ID <- "R29.1"
rates.R29.1$colony <- "R29"
rates.R29.2 <- calcRate(intv = intv, D.dt = DT29, P.dt = PT29, Br.dt = Br29, ROI = 2)
rates.R29.2$cumulative <- rates.R29.2$D+rates.R29.2$P
rates.R29.2$timepoints <- seq(1,nrow(rates.R29.2))*n
rates.R29.2$ID <- "R29.2"
rates.R29.2$colony <- "R29"
rates.R29.3 <- calcRate(intv = intv, D.dt = DT29, P.dt = PT29, Br.dt = Br29, ROI = 3)
rates.R29.3$cumulative <- rates.R29.3$D+rates.R29.3$P
rates.R29.3$timepoints <- seq(1,nrow(rates.R29.3))*n
rates.R29.3$ID <- "R29.3"
rates.R29.3$colony <- "R29"
ratesR29 <- rbind(rates.R29.1, rates.R29.2, rates.R29.3)
ratesR29$colonyID <- as.factor(2)

rates.R05.1 <- calcRate(intv = intv, D.dt = DT05, P.dt = PT05, Br.dt = Br05, ROI = 1)
rates.R05.1$cumulative <- rates.R05.1$D+rates.R05.1$P
rates.R05.1$timepoints <- seq(1,nrow(rates.R05.1))*n
rates.R05.1$ID <- "R05.1"
rates.R05.1$colony <- "R05"
rates.R05.2 <- calcRate(intv = intv, D.dt = DT05, P.dt = PT05, Br.dt = Br05, ROI = 2)
rates.R05.2$cumulative <- rates.R05.2$D+rates.R05.2$P
rates.R05.2$timepoints <- seq(1,nrow(rates.R05.2))*n
rates.R05.2$ID <- "R05.2"
rates.R05.2$colony <- "R05"
rates.R05.3 <- calcRate(intv = intv, D.dt = DT05, P.dt = PT05, Br.dt = Br05, ROI = 3)
rates.R05.3$cumulative <- rates.R05.3$D+rates.R05.3$P
rates.R05.3$timepoints <- seq(1,nrow(rates.R05.3))*n
rates.R05.3$ID <- "R05.3"
rates.R05.3$colony <- "R05"
ratesR05 <- rbind(rates.R05.1, rates.R05.2, rates.R05.3)
ratesR05$colonyID <- as.factor(1)

ROIrates.df <- rbind(ratesR54, ratesR34, ratesR29, ratesR05)
ROIrates.df <- ROIrates.df[order(ROIrates.df$timepoints),]
View(ROIrates.df)

write.csv(ROIrates.df, file = "pathToFile/rates_by_ROI.csv", row.names = FALSE )

## Check the pattern of activity from the start of building

#by colony
par(mfrow=c(1,1))

plot(y = rates.R05.1$D, x = rates.R05.1$timepoints, main = "Deposition rate over time \n R5", xlab = 'time since first deposition', ylab = "n events per min", type = 'l', lty = 1, ylim = c(0,max(max(rates.R05.1$D), max(rates.R05.2$D), max(rates.R05.3$D))))
lines(rates.R05.2$D, x = rates.R05.2$timepoints, 'l', lty = 2)
lines(rates.R05.3$D, x = rates.R05.3$timepoints, 'l', lty = 3)
legend('topright', legend = c('ROI 1','ROI 2', 'ROI 3'), lty = c(1,2,3),bty = 'n')

plot(y = rates.R29.1$D, x = rates.R29.1$timepoints, main = "Deposition rate over time \n R29", xlab = 'time since first deposition', ylab = "n events per min", type = 'l', lty = 1, ylim = c(0,max(max(rates.R29.1$D), max(rates.R29.2$D), max(rates.R29.3$D))))
lines(rates.R29.2$D, x = rates.R29.2$timepoints, 'l', lty = 2)
lines(rates.R29.3$D, x = rates.R29.3$timepoints, 'l', lty = 3)
legend('topright', legend = c('ROI 1','ROI 2', 'ROI 3'), lty = c(1,2,3),bty = 'n')

plot(y = rates.R34.1$D, x = rates.R34.1$timepoints, main = "Deposition rate over time \n R34", xlab = 'time since first deposition', ylab = "n events per min", type = 'l', lty = 1, ylim = c(0,max(max(rates.R34.1$D), max(rates.R34.2$D), max(rates.R34.3$D))))
lines(rates.R34.2$D, x = rates.R34.2$timepoints, 'l', lty = 2)
lines(rates.R34.3$D, x = rates.R34.3$timepoints, 'l', lty = 3)
legend('topright', legend = c('ROI 1','ROI 2', 'ROI 3'), lty = c(1,2,3),bty = 'n')

plot(y = rates.R54.1$D, x = rates.R54.1$timepoints, main = "Deposition rate over time \n R54", xlab = 'time since first deposition', ylab = "n events per min", type = 'l', lty = 1, ylim = c(0,max(max(rates.R54.1$D), max(rates.R54.2$D), max(rates.R54.3$D))))
lines(rates.R54.2$D, x = rates.R54.2$timepoints, 'l', lty = 2)
lines(rates.R54.3$D, x = rates.R54.3$timepoints, 'l', lty = 3)
legend('topright', legend = c('ROI 1','ROI 2', 'ROI 3'), lty = c(1,2,3),bty = 'n')

tmax.all <- max(tmax.R05, tmax.R29, tmax.R34, tmax.R54)


# change in distance to brood over time
brood.p <- ggplot(rates, aes(x = timepoints, y = distance)) +
   geom_line(aes(linetype = ID)) +
   labs(y = "average distance of building areas to brood centroid (mm)\n ", x = " \ntime since start of activity (min)", linetype = "colony") +
   theme_bw()

# change in distance to brood over time by ROI
rates_ROI <- read_csv("PathToFile/rates_by_ROI.csv")

ROI54.brood <- ggplot(subset(rates_ROI, colony == "R54"), aes(x = timepoints, y = distance)) +
   geom_line(aes(linetype = ID)) +
   labs(y = "distance of building area to brood centroid (mm)", x = "time since start of activity (min)", linetype = "building area") +
   theme_bw()

ROI34.brood <- ggplot(subset(rates_ROI, colony == "R34"), aes(x = timepoints, y = distance)) +
   geom_line(aes(linetype = ID)) +
   labs(y = "distance of building area to brood centroid (mm)", x = "time since start of activity (min)", linetype = "building area") +
   theme_bw()

ROI29.brood <- ggplot(subset(rates_ROI, colony == "R29"), aes(x = timepoints, y = distance)) +
   geom_line(aes(linetype = ID)) +
   labs(y = "distance of building area to brood centroid (mm)", x = "time since start of activity (min)", linetype = "building area") +
   theme_bw()

ROI05.brood <- ggplot(subset(rates_ROI, colony == "R05"), aes(x = timepoints, y = distance)) +
   geom_line(aes(linetype = ID)) +
   labs(y = "distance of building area to brood centroid (mm)", x = "time since start of activity (min)", linetype = "building area") +
   theme_bw()

par(mfrow = c(2,2))
plot(y = rates[rates$ID == "R05", "D"], x = rates[rates$ID == "R05", "stone_dens"], xlim = c(0,smax.all), ylim = c(0,ymax.all), main = "R05", xlab = "stone density (stones/mm^2)", ylab = "deposition events per minute")
plot(y = rates[rates$ID == "R29", "D"], x = rates[rates$ID == "R29", "stone_dens"], xlim = c(0,smax.all), ylim = c(0,ymax.all), main = "R29", xlab = "stone density (stones/mm^2)", ylab = "deposition events per minute")
plot(y = rates[rates$ID == "R34", "D"], x = rates[rates$ID == "R34", "stone_dens"], xlim = c(0,smax.all), ylim = c(0,ymax.all), main = "R34", xlab = "stone density (stones/mm^2)", ylab = "deposition events per minute")
plot(y = rates[rates$ID == "R54", "D"], x = rates[rates$ID == "R54", "stone_dens"], xlim = c(0,smax.all), ylim = c(0,ymax.all), main = "R54", xlab = "stone density (stones/mm^2)", ylab = "deposition events per minute")

plot(y = rates[rates$ID == "R05", "D"], x = rates[rates$ID == "R05", "n_ants"], xlim = c(nmin.all,nmax.all), ylim = c(0,ymax.all), main = "R05", xlab = "n ants in cluster (standardised)", ylab = "deposition events per minute")
plot(y = rates[rates$ID == "R29", "D"], x = rates[rates$ID == "R29", "n_ants"], xlim = c(nmin.all,nmax.all), ylim = c(0,ymax.all), main = "R29", xlab = "n ants in cluster (standardised)", ylab = "deposition events per minute")
plot(y = rates[rates$ID == "R34", "D"], x = rates[rates$ID == "R34", "n_ants"], xlim = c(nmin.all,nmax.all), ylim = c(0,ymax.all), main = "R34", xlab = "n ants in cluster (standardised)", ylab = "deposition events per minute")
plot(y = rates[rates$ID == "R54", "D"], x = rates[rates$ID == "R54", "n_ants"], xlim = c(nmin.all,nmax.all), ylim = c(0,ymax.all), main = "R54", xlab = "n ants in cluster (standardised)", ylab = "deposition events per minute")

plot(y = rates[rates$ID == "R05", "D"], x = rates[rates$ID == "R05", "distance"], ylim = c(0,ymax.all), main = "R05", xlab = "distance between ROI centre and centre of cluster (mm)", ylab = "deposition events per minute")
plot(y = rates[rates$ID == "R29", "D"], x = rates[rates$ID == "R29", "distance"], ylim = c(0,ymax.all), main = "R29", xlab = "distance between ROI centre and centre of cluster (mm)", ylab = "deposition events per minute")
plot(y = rates[rates$ID == "R34", "D"], x = rates[rates$ID == "R34", "distance"], ylim = c(0,ymax.all), main = "R34", xlab = "distance between ROI centre and centre of cluster (mm)", ylab = "deposition events per minute")
plot(y = rates[rates$ID == "R54", "D"], x = rates[rates$ID == "R54", "distance"], ylim = c(0,ymax.all), main = "R54", xlab = "distance between ROI centre and centre of cluster (mm)", ylab = "deposition events per minute")


###################
# rates over TOD #
##################
library(lubridate)
library(hms)

R54.expStartTime <- as.period("0d 08H 16M 0S")
R34.expStartTime <- as.period("0d 08H 30M 0S")
R29.expStartTime <- as.period("0d 06H 40M 0S")
R05.expStartTime <- as.period("0d 08H 15M 0S")

R54.startTime.seconds <- period_to_seconds(R54.expStartTime) + min(Col54$time) # time of start of experiment + time from start of experiment to first building event
R34.startTime.seconds <- period_to_seconds(R34.expStartTime) + min(Col34$time)
R29.startTime.seconds <- period_to_seconds(R29.expStartTime) + min(Col29$time)
R05.startTime.seconds <- period_to_seconds(R05.expStartTime) + min(Col05$time)

secToTime <- function(s) {
   hh <- floor(s/3600)
   mm <- floor((s%%3600)/60)
   ss <- s - hh*3600 - mm*60
   return(sprintf('%02d:%02d:%02d', hh, mm, ss))
}

rates.R54 <- subset(rates, ID == "R54")
rates.R54$TOD <- secToTime(as.double(rates.R54$timepoints*60 + R54.startTime.seconds))

rates.R34 <- subset(rates, ID == "R34")
rates.R34$TOD <- secToTime(as.double(rates.R34$timepoints*60 + R34.startTime.seconds))

rates.R29 <- subset(rates, ID == "R29")
rates.R29$TOD <- secToTime(as.double(rates.R29$timepoints*60 + R29.startTime.seconds))

rates.R05 <- subset(rates, ID == "R05")
rates.R05$TOD <- secToTime(as.double(rates.R05$timepoints*60 + R05.startTime.seconds))

rates_with_TOD <- rbind(rates.R54, rates.R34, rates.R29, rates.R05)
rates_with_TOD$TOD <- as.POSIXct(rates_with_TOD$TOD, format = "%H:%M:%S", tz = "EST")
rates_with_TOD$TOD_time_only <- hms(hours = lubridate::hour(rates_with_TOD$TOD), minutes =  minute(rates_with_TOD$TOD), seconds =  second(rates_with_TOD$TOD))
View(rates_with_TOD)

lt <- c(2,4,3,1)

TOD.plot <- ggplot(rates_with_TOD, aes(x = TOD, y = D)) +
   geom_line(aes(linetype = as.factor(lt [numericID]))) +
   coord_cartesian(ylim = c(0,4), expand = FALSE) +
   labs(y = "deposition rate", x = "time of day", linetype = "colony") +
   scale_linetype(name = "colony", labels = as.character(c(4,3,2,1))) +
   theme_bw() +
   theme(text = element_text(size = 18))

TOD.plot
