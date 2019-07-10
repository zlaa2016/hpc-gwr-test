
library(dplyr)
library(tidyr)
library(spgwr)
library(broom)
library(data.table)
#library(margins)
library(Rmpi)
library(tictoc)

require(tictoc)

tic.clear()
tic.clearlog()

#mpi.spawn.Rslaves(nslaves=1)

tic("Reading subset_1_v1.csv")
data <- read.csv("~/Desktop/CS 60/subset_1_v1.csv")
toc()

tic("glm")
model = glm(sig_change ~ far_app + factor(ZipCode) + setback + use_code_cat0 + use_code_cat1 +
              use_code_cat2+use_code_cat3+age+
              implandratio+lotarea+builtsqftmain+age_39,data = data,family = binomial(link="probit"))
toc()

tic("coordinates")
coordinates <- cbind(data$center_lon,data$center_lat)
toc()

tic("GWRbandwidth")
GWRbandwidth <- gwr.sel(sig_change ~ far_app + factor(year) + setback + use_code_cat0 + use_code_cat1 +
                          use_code_cat2+use_code_cat3+age+implandratio+lotarea+builtsqftmain+age_39, 
                        data=data, coords=coordinates,adapt=FALSE)
toc()

tic("model")
gwr.model = gwr(sig_change ~ far_app + setback + factor(year) + use_code_cat0 + use_code_cat1 +
                  use_code_cat2+use_code_cat3+age+implandratio+lotarea+builtsqftmain+age_39, 
                data=data, coords=coordinates, bandwidth = GWRbandwidth)
toc()


tic("model loop")
for (i in 1:18){
  year_data <- data[ which(data$year==(1999+i)), ]
  print(i)
  gwr.model.new <- gwr(sig_change ~ far_app + setback + use_code_cat0 + use_code_cat1 +
                         use_code_cat2+use_code_cat3+age+implandratio+lotarea+builtsqftmain+age_39, 
                       data=year_data, coords=cbind(year_data$center_lon,year_data$center_lat), bandwidth = GWRbandwidth)
  #gwr.model.stored<- append(gwr.model.stored,gwr.model.new)
}

toc()

proc.time()

#mpi.spawn.Rslaves(nslaves=1)
#ptm<-proc.time() 
#mpi.iparReplicate(400, mean(rnorm(1000000)))
#print(proc.time() - ptm)
