library(sf)
library(dplyr)
library(ggspatial)
library(ggplot2)
library(magrittr)

options(scipen=999)


# ## we only need an identifying variable and the geometry of the state to proceed
# brazil_states <- geobr::read_state(code_state = "all", simplified=TRUE) %>%
#     select(code_state,geom) %>%
#     mutate(code_state = as.character(code_state))
# 
# set.seed(0)
# 
# ## We add populations to each state so we have a variable to aggregate by.
# num_states <- nrow(brazil_states)
# brazil_states$pop_total <- sample(1:num_states,
#                                   size = num_states,
#                                   replace = FALSE)
# 
# pop_threshold <- 75
  
# code_muni_select<-c("3550308")
  
  # c("3541000","3551009","3531100","3522109",
  #                   "3513504","3537602","3551009",
  #                   "3548500","3518701","3548708",
  #                   "3520301", "3520426","3509908",
  #                   "3536208","3524600","3509254",
  #                   "3505401","3537206","3542602","3514809",
  #                   "3521200", "3555406","3513603",
  #                   "3549607","3503158","3549607",
  #                   "3503505","3513603","3550001",
  #                   "3532306","3535606","3545001",
  #                   "3506607","3530607","3552502")

#"3550308",

# census tracts of Sao Paulo metro area
metro_sp_munis <- geobr::read_metro_area(year=2018)
metro_sp_munis <- subset(metro_sp_munis, name_metro == 'RM São Paulo')$code_muni

#upload dataframe
df<- readRDS('census_tracts_covariates_spstate_4aug.rds') %>%
    select(code_tract,code_muni, pop_total) %>%
    filter(!(code_muni %in% metro_sp_munis))%>%
    select(-code_muni) %>%
    mutate(code_tract=as.character(code_tract)) %>%
    mutate(pop_total=tidyr::replace_na(pop_total,0))
st_geometry(df)<-NULL

census_tract_rds <- "census_tract_df.rds"
census_tract <- if (file.exists(census_tract_rds)) {
  ## read the data in from disk
  readRDS(census_tract_rds)
} else {
  ## download the census tracts using the \code{geobr} package.
  geobr::read_census_tract(year=2010,
                           code_tract="SP",
                           simplified=FALSE)
}

#upload census tract shapefile with full geometry
census_tract<-geobr::read_census_tract(year=2010,code_tract="SP",simplified=FALSE)
# muni<-geobr::read_municipality(year=2018,code_muni=35,simplified=FALSE)
# st_write(muni, "muni_shape.shp")

#ensure format is correct for merging later
census_tract$code_tract<-as.character(census_tract$code_tract)
df$code_tract<-as.character(df$code_tract)
#re-merge census tract shapefile 
df<-left_join(df,census_tract[,c("code_tract")],by="code_tract")
df<-st_as_sf(df)
# #write to shapefile
# st_write(df, "census_tracts_shape.shp")

# df_sp<-as_Spatial(df)
# list.nb <- gTouches(df_sp, byid = TRUE, returnDense = FALSE)
# #nearest neighbour search
# neighbours<-nngeo::st_nn(df,df,k = 1,progress=FALSE)
# neighbours<-st_join(df,df,join=st_nn,k=1,progress=FALSE)

merge_smallest_by_centroid <- function(brazil_states) {
    ## Seperate the smallest state from the rest so we know what we want to
    ## merge
    pop_sizes <- brazil_states$pop_total
    smallest_pop_mask <- pop_sizes == min(pop_sizes)
    if (sum(smallest_pop_mask) > 1) {
        warning("Multiple states with the same population size")
        area_sizes <- st_area(brazil_states)
        ## we the smallest size *conditioned* upon having equal smallest
        ## population so we need to avoid those with a larger population.
        area_sizes[not(smallest_pop_mask)] <- Inf
        smallest_area_mask <- area_sizes == min(area_sizes)
        smallest_pop_mask <- smallest_pop_mask & smallest_area_mask
        if (sum(smallest_pop_mask) > 1) {
            stop("Could not resolve population size tie by area...")
        }
    }
    smallest_state <- brazil_states[smallest_pop_mask,]
    non_smallest_states <- brazil_states[not(smallest_pop_mask),]
    ## Seperate the nearest neighbour to the smallest state so we have something
    ## to merge with
    ss_centroid <- st_centroid(smallest_state)
    non_ss_centroids <- st_centroid(non_smallest_states)
    centroid_distances <- st_distance(ss_centroid, non_ss_centroids)
    nearest_centroid_distance <- min(centroid_distances)
    non_nn_states <- non_smallest_states[centroid_distances != nearest_centroid_distance,]
    nn_state <- non_smallest_states[centroid_distances == nearest_centroid_distance,]
    ## We need to fix up the attributes manually because they are mangled by the
    ## merge when we put everything back together
    merged_state <- st_union(select(smallest_state, -pop_total, -code_tract),  #just select geom 
                             select(nn_state, -pop_total, -code_tract))
    merged_state$pop_total <- smallest_state$pop_total + nn_state$pop_total       #add attributes back such as pop_total and code_state
    merged_state$code_tract <- paste(smallest_state$code_tract,nn_state$code_tract, sep = ":")
    rbind(non_nn_states, merged_state)
}


iterate_merging <- function(brazil_states, pop_threshold, max_iters = 5500) {
  iter_count <- 0
  smallest_pop <- min(brazil_states$pop_total)
  iter_upper_bound <- min(sum(brazil_states$pop_total < pop_threshold), max_iters)
  if (iter_upper_bound == max_iters) {
    stop("max_iters given to iterate_merging appears too low!")
  }
  prog_bar <- progress_bar$new(format = "merging [:bar] :percent eta :eta after :elapsed",
                               total = iter_upper_bound,
                               clear = FALSE,
                               width = 80)
  ## Loop until the smallest population is at least as big as the population
  ## threshold so that there are no remaining small areas.
  message("Running the merge")
  prog_bar$tick(0)
  while (smallest_pop < pop_threshold & iter_count < max_iters) {
    brazil_states <- merge_smallest_by_centroid(brazil_states)
    smallest_pop <- min(brazil_states$pop_total)
    iter_count <- iter_count + 1 # <--- avoid infinite loop!
    prog_bar$tick()
  }
  
  if (iter_count < max_iters) {
    return(brazil_states)
  } else {
    stop("reached maximum iterations without solution")
  }
}

aggregated_sp_census_state <- iterate_merging(df, 75)
saveRDS(aggregated_sp_census_state,"aggregated_sp_census_23aug.rds")


# fig_1 <- ggplot() +
#     geom_sf(data = df,
#             mapping = aes(fill = code_tract,
#                           colour = pop_total == min(pop_total)),
#             size = 2)
# print(fig_1)
# 
# aggregated_brazil_states <- iterate_merging(brazil_states, 24)
# fig_2 <- ggplot() +
#     geom_sf(data = aggregated_brazil_states,
#             mapping = aes(fill = code_tract,
#                           colour = pop_total == min(pop_total)),
#             size = 2)
# print(fig_2)
