library(sf)
library(dplyr)
library(ggspatial)
library(ggplot2)
library(magrittr)
library(progress)
library(tidyr)
library(stringr)
library(data.table)

options(scipen=999)

#====merge census tracts with small pop, then re-aggregate covariates====

###sample test data###
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

###more sample test data###  
#code_muni_select<-c("3548500")
  
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

#read age csv and merge with df
age<-read.csv("age_data/age-data.csv") %>%
  mutate(code_tract=as.character(code_tract))

#upload processed covariate data from code census2010_spstate_preprocessing.R
df<- readRDS('/Volumes/SLLIWD/covid-social_inequalities/covariates_spstate/covariates_sp_21sept.rds') %>%
  dplyr::select(code_tract,code_muni, pop_total,pop_per_household,
         households_total,household_density,income_total,
         edu_primary_lower,unemployed,informal, area_km,distmin_hosp,starts_with("idade")) %>%
  #filter((code_muni %in% code_muni_select))%>% #filter(!(code_muni %in%   metro_sp_munis))%>%
  #select(-code_muni) %>%
  mutate(code_tract=as.character(code_tract)) %>%
  #if pop_total=NA change it to 0 
  mutate(pop_total=tidyr::replace_na(pop_total,0))  #if pop_total=NA change it to 0 

#remove geometry 
st_geometry(df)<-NULL

#upload census tract shapefile with full geometry
census_tract_rds <- "census_tract_df.rds"
census_tract <- if (file.exists(census_tract_rds)) {
  ## read the data in from disk
  readRDS(census_tract_rds)
 }else {
  ## download the census tracts using the \code{geobr} package.
  geobr::read_census_tract(year=2010,
                           code_tract="SP",
                           simplified=FALSE)
 }
  
#census tracts of Sao Paulo metro area
metro_sp_munis <- geobr::read_metro_area(year=2018)
metro_sp_munis <- subset(metro_sp_munis, name_metro == 'RM São Paulo')$code_muni 

#extract only covariate data for gmsp
gmsp_df<-subset(df,code_muni %in% metro_sp_munis) %>%
  dplyr::select(-code_muni)

#census_tract<-geobr::read_census_tract(year=2010,code_tract="SP",simplified=FALSE)
# muni<-geobr::read_municipality(year=2018,code_muni=35,simplified=FALSE)
# st_write(muni, "muni_shape.shp")

#re-merge with census tract
#for gmsp
df<-left_join(gmsp_df,census_tract[,'code_tract'],by="code_tract") %>%
  glimpse()

#for entire state
##ensure class is correct for merging 
# census_tract$code_tract<-as.character(census_tract$code_tract)
# df$code_tract<-as.character(df$code_tract)
##re-merge census tract with dataframe
#df<-left_join(df,census_tract[,c("code_tract")],by="code_tract")

#convert to sf object
df<-st_as_sf(df)

#merge census tracts based on the shortest distance between centroids 
#after merging census tracts, covariates are reaggregated
merge_smallest_by_centroid <- function(df) {
    ## Seperate the smallest state from the rest so we know what we want to
    ## merge
    pop_sizes <- df$pop_total
    smallest_pop_mask <- pop_sizes == min(pop_sizes)
    if (sum(smallest_pop_mask) > 1) {
      warning("Multiple states with the same population size")
      area_sizes <- st_area(df)
      ## we the smallest size *conditioned* upon having equal smallest
      ## population so we need to avoid those with a larger population.
      area_sizes[not(smallest_pop_mask)] <- Inf
      smallest_area_mask <- area_sizes == min(area_sizes)
      smallest_pop_mask <- smallest_pop_mask & smallest_area_mask
      if (sum(smallest_pop_mask) > 1) {
        stop("Could not resolve population size tie by area...")
      }
    }
    smallest_state <- df[smallest_pop_mask,]
    non_smallest_states <- df[not(smallest_pop_mask),]
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
    merged_state <- st_union(dplyr::select(smallest_state),  #just select geom
                             dplyr::select(nn_state))
    #add attributes back such as pop_total and code_state
    merged_state$pop_total <- smallest_state$pop_total + nn_state$pop_total  

    #income 
    merged_state$income_total<-sum(smallest_state$income_total,nn_state$income_total,na.rm=TRUE)
    # merged_state$income<-sum(smallest_state$income,nn_state$income,na.rm=TRUE)
    
    #merged_state$income_percapita<-sum(merged_state$income_total)/merged_state$pop_total
    # merged_state$income_percapita<-sum(merged_state$income)/merged_state$pop_total
    
    #household density
    merged_state$household_density<- sum(smallest_state$households_total,
            nn_state$households_total,na.rm=TRUE)/sum(smallest_state$area_km,nn_state$area_km,na.rm=TRUE)
    
    # merged_state$household_density<- sum(smallest_state$households_private,
    #     nn_state$households_private,na.rm=TRUE)/sum(smallest_state$area_km,nn_state$area_km,na.rm=TRUE)
    
    #pop_per_household
    merged_state$pop_per_household<- mean(c(smallest_state$pop_per_household,nn_state$pop_per_household),na.rm=TRUE)

    #informal workers
    merged_state$informal <- mean(c(smallest_state$informal,nn_state$informal))
    
    #unemployment
    merged_state$unemployed <- mean(c(smallest_state$unemployed,nn_state$unemployed))
    
    # #education
    merged_state$edu_primary_lower <- mean(c(smallest_state$edu_primary_lower,nn_state$edu_primary_lower))

    #mode age
    # getmode <- function(v) {
    #   uniqv <- unique(v$mode_age)
    #   uniqv<-uniqv[!is.na(uniqv)]
    #   uniqv[which.max(tabulate(match(v, uniqv)))]
    # }
    # 
    # mode_age_all<-rbind(data.frame(mode_age=smallest_state$mode_age),data.frame(mode_age=nn_state$mode_age))
    # merged_state$mode_age<-getmode(mode_age_all)
    
    #age groups
    merged_state$idade_0a9<- mean(c(smallest_state$idade_0a9,nn_state$idade_0a9))
    merged_state$idade_10a14<- mean(c(smallest_state$idade_10a14,nn_state$idade_10a14))
    merged_state$idade_15a19<- mean(c(smallest_state$idade_15a19,nn_state$idade_15a19))
    merged_state$idade_20a29<- mean(c(smallest_state$idade_20a29,nn_state$idade_20a29))
    merged_state$idade_30a39<- mean(c(smallest_state$idade_30a39,nn_state$idade_30a39))
    merged_state$idade_40a49<- mean(c(smallest_state$idade_40a49,nn_state$idade_40a49))
    merged_state$idade_50a59<- mean(c(smallest_state$idade_50a59,nn_state$idade_50a59))
    merged_state$idade_60a69<- mean(c(smallest_state$idade_60a69,nn_state$idade_60a69))
    merged_state$idade_70<- mean(c(smallest_state$idade_70,nn_state$idade_70))

    #min. distance to nearest health facility
    merged_state$distmin_hosp <- mean(c(smallest_state$distmin_hosp,nn_state$distmin_hosp))
    
    #area - repeat for merging
    merged_state$area_km <- sum(smallest_state$area_km,nn_state$area_km)
    
    #households_total - repeat this covariate for merging
    merged_state$households_total <- sum(smallest_state$households_total,nn_state$households_total)
    
    #households_private - repeat this covariate for merging
    #merged_state$households_private <- sum(smallest_state$households_private,nn_state$households_private)
    
    merged_state$code_tract <- paste(smallest_state$code_tract,nn_state$code_tract, sep = ":")
    
    rbind(non_nn_states, merged_state)
  }
  
max_iters<-subset(df,pop_total<143)
#iterate the merge until all census tracts have a pop_total >=75
#for gmsp it's 68
#but for gmsp weekly estimates it's 143
iterate_merging <- function(df, pop_threshold, max_iters =2250) {  
    #iterate_merging <- function(df, pop_threshold, max_iters =5500) {  
      iter_count <- 0
      smallest_pop <- min(df$pop_total)
      iter_upper_bound <- min(sum(df$pop_total < pop_threshold), max_iters)
      if (iter_upper_bound == max_iters) {
        stop("max_iters given to iterate_merging appears too low!")
      }
      prog_bar <- progress_bar$new(format = 
                                     "merging [:bar] :percent eta :eta after :elapsed",
                                   total = iter_upper_bound,
                                   clear = FALSE,
                                   width = 80)
      ## Loop until the smallest population is at least as big as the population
      ## threshold so that there are no remaining small areas.
      message("Running the merge")
      prog_bar$tick(0)
      while (smallest_pop < pop_threshold & iter_count < max_iters) {
        df <- merge_smallest_by_centroid(df)
        smallest_pop <- min(df$pop_total)
        iter_count <- iter_count + 1 # <--- avoid infinite loop!
        prog_bar$tick()
      }
      
      if (iter_count < max_iters) {
        return(df)
      } else {
        stop("reached maximum iterations without solution")
      }
    }

aggregated_gmsp_census <- iterate_merging(df, 143)
#aggregated_sp_census_state<-iterate_merging(df,75)
saveRDS(aggregated_gmsp_census,"aggregated_census_gmsp/aggregated_gmsp_census_06Oct.rds")
write_sf(aggregated_gmsp_census,"aggregated_gmsp.shp")
#saveRDS(aggregated_sp_census_state,"aggregated_sp_census_state_3sept.rds")
aggregated_sp_census_state <- iterate_merging(df, 75)
saveRDS(aggregated_sp_census_state,"aggregated_sp_census_state_25aug.rds")
  
#====combined merged census tracts with weekly case data====

#load data
# aggregated_gmsp_census<-readRDS("/Volumes/SLLIWD/covid-social_inequalities/aggregated_census_gmsp/") %>%
#   mutate(idarea=1:nrow(.))
#   
aggregated_sp_census_state<-readRDS("aggregated_census_gmsp/tract_merge_state/aggregated_sp_census_state_3sept.rds") %>%
    mutate(idarea=1:nrow(.))
  
#remove geometry for processing
st_geometry(aggregated_gmsp_census)<-NULL

# st_geometry(aggregated_sp_census_state)<-NULL

#imported weekly case data
weekly_sum<-readRDS("/Volumes/SLLIWD/covid-social_inequalities/case_data/weekly_sum_complete_gmsp_cases_5Oct.rds") %>%
  #dplyr::select(-pop_total) %>% #weekly_sum_complete_spstate_4sept.rds"
  mutate(code_tract=as.character(code_tract)) %>%
  filter(code_tract %in% gmsp_df$code_tract)

# weekly_sum<-readRDS("weekly_sum_complete_spstate.rds") %>%
#   select(-pop_total,-geom) %>%
#   mutate(code_tract=as.character(code_tract))

#create a column to indicate whether it's a merged or non-merged census tract then add it to the census df
# merged_area_bool<-data.frame(merged_area_bool=str_detect(aggregated_gmsp_census$code_tract, ":"))
# aggregated_gmsp_census<-cbind(merged_area_bool,aggregated_gmsp_census)

merged_area_bool<-data.frame(merged_area_bool=str_detect(aggregated_sp_census_state$code_tract, ":"))
aggregated_sp_state<-cbind(merged_area_bool,aggregated_sp_census_state)

#determine how many columns are needed for function aggregate_weekly_cases
# within(aggregated_gmsp_census, FOO<-data.frame(do.call('rbind', strsplit(as.character(code_tract), ':', fixed=TRUE))))
# within(aggregated_sp_census_state, FOO<-data.frame(do.call('rbind', strsplit(as.character(code_tract), ':', fixed=TRUE))))

#create a new dataframe combining merged census tracts, reaggregated covariates, and weekly case data 
aggregate_cases_by_tract<- function(aggregated_sp_state, weekly_sum){
    newdf<-aggregated_sp_state[aggregated_sp_state$merged_area_bool =="TRUE",]
    #if tract is merged, then run function aggregate_weekly_cases to aggregate weekly case data
    new_merged_weekly_sum<-aggregate_weekly_cases(aggregated_sp_state,weekly_sum)
    #if tract is not merged, then just join weekly case data with covariates 
    ifelse (aggregated_sp_state$merged_area_bool=="FALSE", 
            a<-(left_join(aggregated_sp_state[!(aggregated_sp_state$merged_area_bool %in% newdf$merged_area_bool),],
                          weekly_sum,by="code_tract")),
            b<-(left_join(newdf,new_merged_weekly_sum, by="code_tract"))
    )
    return(rbind(a,b))
  }
  
#this function aggregated the number of weekly cases for each merged census tract
aggregate_weekly_cases<- function(aggregated_sp_state,weekly_sum) {
    df<-  aggregated_sp_state %>% # %>%  newdf
      mutate(code_tract_original=code_tract) %>%
      separate(code_tract, c("ct1","ct2","ct3","ct4",
                             "ct5","ct6","ct7","ct8",
                             "ct9","ct10","ct11","ct12",
                             "ct13","ct14","ct15")) %>%
      rename(code_tract=code_tract_original)
    newdf_with_summed_cases <-data.frame(code_tract=NA, weeklysum=NA, week_notific=NA,status=NA)
    for (i in 1:nrow(df)){
      cases_per_merged_tract<-filter(weekly_sum, code_tract %in% 
                                       c(df[i,]$ct1,df[i,]$ct2,df[i,]$ct3,
                                         df[i,]$ct4,df[i,]$ct5,df[i,]$ct6,
                                         df[i,]$ct7,df[i,]$ct8,df[i,]$ct9,
                                         df[i,]$ct10,df[i,]$ct11,df[i,]$ct12,
                                         df[i,]$ct13,df[i,]$ct14,df[i,]$ct15))
      #aggregate cases by week
      df_2 <-cases_per_merged_tract %>% 
      group_by(week_notific,status) %>% 
      summarise(.,weeklysum = sum(weeklysum,na.rm=TRUE)) 
      newdf_with_summed_cases<-rbind(newdf_with_summed_cases,
                                     data.frame("code_tract"=df$code_tract[i],
                                                "weeklysum"=df_2$weeklysum,
                                                "week_notific"=df_2$week_notific,
                                                "status"=df_2$status))
    }
    newdf_with_summed_cases$code_tract<-as.character(newdf_with_summed_cases$code_tract)
    newdf_with_summed_cases<-newdf_with_summed_cases[!is.na(newdf_with_summed_cases$week_notific), ]
    return(newdf_with_summed_cases)
}
##create sample dataset to test functions
# newdf$id<-1:nrow(newdf)
# newdf<-select(newdf,-id)
# # newdf$id<-1:nrow(newdf)
# # newdf<-select(newdf,-id)
# newdf_sample<-rbind(newdf[1849,],newdf[2142,],newdf[3627,],aggregated_sp_state[1,],
#                       aggregated_sp_state[2,])
#                       aggregated_sp_state[2,]) 
# requires_merging_cases<-new_merged_weekly_sum(newdf_sample,weekly_sum)
# test<-aggregate_cases_by_tract(newdf_sample,weekly_sum)

#run function on data and save file
aggregate_final<-aggregate_cases_by_tract(aggregated_gmsp_census,weekly_sum)
saveRDS(aggregate_final,"/Volumes/SLLIWD/covid-social_inequalities/case_data/aggregate_gmsp_final_merge_06Oct.rds")


length(which(agg$monthlysum == 0))
length(which(aggregate_final$weeklysum != 0))


aggregate_final<-aggregate_cases_by_tract(aggregated_sp_state,weekly_sum)
saveRDS(aggregate_final,"aggregate_final.rds")

#import data again to merge geometry
tracts_sf<-readRDS("/Volumes/SLLIWD/covid-social_inequalities/aggregated_census_gmsp/aggregated_gmsp_census_06Oct.rds") %>%
  mutate(idarea=1:nrow(.))

tracts_sf<-st_as_sf(tracts_sf)
census_tract<-geobr::read_census_tract(year=2010,
                                       code_tract="SP",
                                       simplified=FALSE)
#merge geometry
aggregate_final<-aggregate_final %>%
  dplyr::select(-idarea)

tracts_cases<-left_join(aggregate_final,tracts_sf[,c("code_tract","idarea")],by="code_tract") 
saveRDS(tracts_cases, file = "/Volumes/SLLIWD/covid-social_inequalities/case_data/aggregate_gmsp_final_cases_07Oct.rds") 


# fig_1 <- ggplot() +
#     geom_sf(data = df,
#             mapping = aes(fill = code_tract,
#                           colour = pop_total == min(pop_total)),
#             size = 2)
# print(fig_1)so
# print(fig_1)
# 
# aggregated_brazil_states <- iterate_merging(brazil_states, 24)
# fig_2 <- ggplot() +
#     geom_sf(data = aggregated_brazil_states,
#             mapping = aes(fill = code_tract,
#                           colour = pop_total == min(pop_total)),
#             size = 2)
# print(fig_2)