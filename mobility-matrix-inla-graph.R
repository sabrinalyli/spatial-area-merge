library(dplyr)
library(INLA)


#' The threshold above which we consider to areas to be adjacent.
thresh <- 175 #550 #225

#' NOTE download files from dropbox folder called "spatial-matrix". The
#' \code{OUTPUT_FILE} is where we write the final result to.

MOBILITY_XLSX <- "mobility_mat17OCT.xlsx"
PEDRO_NAMES_CSV <- "code_muni_names_pedro.csv"
MUNI_NAMES_CSV <- "code_muni_names_sp.csv"

OUTPUT_FILE <- "id_list_forinla.txt"

stopifnot(file.exists(MOBILITY_XLSX))
stopifnot(file.exists(PEDRO_NAMES_CSV))
stopifnot(file.exists(MUNI_NAMES_CSV))

#import mobility data for the month of March 2020
mob<-readxl::read_excel(MOBILITY_XLSX) %>%
  select(-col) 
colnames(mob)=c(1:nrow(mob))

#convert file to matrix 
mob_mat<- as.matrix(mob)

#set within municipality trips to 0 
diag(mob_mat) <- 0

#' convert matrix to a binary matrix where 1=number of trips taken exceeds
#' threshold and 0=number of trips below threshold. The multiplication by 1 is
#' there to force R to represent the logical values in the matrix as numbers.
mat_bin<-mob_mat>thresh 
mat_bin<-mat_bin*1

#remove header names
dimnames(mat_bin) <- NULL

#import order of municipalities based on matrix
muni_mat<-read.csv(PEDRO_NAMES_CSV) %>%
  rename(idarea2=idarea)

#import code_muni_sp.csv which serves as the reference list
muni_sp<-read.csv(MUNI_NAMES_CSV) %>%
  rename(idarea1=idarea)

#create id dictionary that links id from code_muni_sp.csv to id/location in matrix
dict<-left_join(muni_sp,muni_mat[,c("code_muni","idarea2")],by="code_muni")

#' create function that extracts the idarea of each matrix cell based on the
#' definitive list of muni names
idarea_extract<- function(mat_bin) {
id_list <- vector(mode = "list", length = 1)

#' first entry must be the number of nodes in the graph, to avoid hardcoding
#' this we read the number of rows in the matrix
id_list[1]<- nrow(mat_bin)

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
return(id_list)
}

#extract list for mobility matrix
id_list_forinla<-idarea_extract(mat_bin)

#save to text file 
save_file<-lapply(id_list_forinla, write, OUTPUT_FILE, append=TRUE, ncolumns=1000)

#convert list to vector only so it becomes an ascii file which is used as inlagraph input
id_forinla<-unlist(id_list_forinla, use.names=FALSE)

#convert to inla graph
g = INLA::inla.read.graph(id_forinla)
str(g)

#plot inla.graph
inla.spy(g)

#alternative way to plot graph
image(inla.graph2matrix(g),xlab="",ylab="") 

#' To be confident that the graph has been written to file correctly we can read
#' it in from file and compare it to the result we already have. Because
#' equality is not implemented for the \code{inla.graph} class we have to
#' translate them to a matrix first.
g2 <- inla.read.graph(OUTPUT_FILE)
stopifnot(all(inla.graph2matrix(g) == inla.graph2matrix(g2)))

#' We also want to know that the matrix is symmetric because this is a property
#' of any adjacency matrix.
stopifnot(all(inla.graph2matrix(g2) == t(inla.graph2matrix(g2))))

#save matrix as an inla graph
saveRDS(g,"mobility_spatial_matrix_spstate_175.rds")
