library(dplyr)
library(INLA)

#import mobility data for the month of March 2020
mob<-readxl::read_excel("mobility_mat17OCT.xlsx") %>%
  select(-col) 
colnames(mob)=c(1:nrow(mob))

#convert file to matrix 
mob_mat<- as.matrix(mob)

#set within municipality trips to 0 
diag(mob_mat) <- 0

#set trip threshold to 175
thresh <- 175 #550 #225

#convert matrix to a binary matrix where 1=number of trips taken exceeds threshold and 0=number of trips below threshold
mat_bin<-mob_mat>thresh 
mat_bin<-mat_bin*1

#remove header names
dimnames(mat_bin) <- NULL

#import order of municipalities based on matrix
muni_mat<-read.csv("code_muni_names_pedro.csv") %>%
  rename(idarea2=idarea)

#import code_muni_sp.csv which serves as the reference list 
muni_sp<-read.csv("code_muni_names_sp.csv") %>%
  rename(idarea1=idarea) %>%
  arrange(idarea1)

#create id dictionary that links id from code_muni_sp.csv to id/location in matrix
dict<-left_join(muni_sp,muni_mat[,c("code_muni","idarea2")],by="code_muni")

#create function that extracts the idarea of each matrix cell based on the definitive list of muni names
idarea_extract<- function(mat_bin) {
id_list <- vector(mode = "list", length = 1)
#determine id of reference list 
  for (i in dict$idarea1) {
    #find corresponding id of mobility matrix
    id_mat_orig<-dict[i,]$idarea2
    #locate row id in binary matrix 
    id_mat_row=as.logical(mat_bin[id_mat_orig,])
    #extract id of cells that are TRUE
    id_mat_dest<-which(id_mat_row, arr.ind=TRUE)
    #find corresponding id from definitive list
    id_dest_df<- dict %>% 
      filter(idarea2 %in% id_mat_dest)
    id_dest<-dplyr::pull(id_dest_df, idarea1)
    #determine number of edges
    edges=length(id_mat_dest) 
    #create list containing id of origin, number of edges, and ids of destination municipalities
    id_list<-append(id_list,
                    list(c(i,edges,id_dest)))
  }
#first entry must be the number of nodes in the graph
id_list[1]<-645
return(id_list)
}

#extract list for mobility matrix
id_list_forinla<-idarea_extract(mat_bin)

#save to text file 
save_file<-lapply(id_list_forinla, write, "id_list_forinla.txt", append=TRUE, ncolumns=1000)

#convert list to vector only so it becomes an ascii file which is used as inlagraph input
id_forinla<-unlist(id_list_forinla, use.names=FALSE)

#convert to inla graph
g = INLA::inla.read.graph(id_forinla)
str(g)

#plot inla.graph
inla.spy(g)

#alternative way to plot graph
image(inla.graph2matrix(g),xlab="",ylab="") 

#save matrix as an inla graph
saveRDS(g,"mobility_spatial_matrix_spstate_175.rds")
