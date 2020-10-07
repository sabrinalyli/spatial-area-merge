#this script determines the threshold count we are going to use to ensure that census tracts in the gmsp has at least one case per area

census_tract <- geobr::read_census_tract(year=2010, code_tract = 'SP', simplified = T)
metro_sp_munis <- geobr::read_metro_area(year=2018)
metro_sp_munis <- subset(metro_sp_munis, name_metro == 'RM São Paulo')$code_muni

total_pop<-readRDS('/Volumes/SLLIWD/covid-social_inequalities/covariates_spstate/covariates_sp_21sept.rds') %>%
  dplyr::select(pop_total,code_state,code_muni) %>%
  filter(code_muni %in% metro_sp_munis) %>%
  group_by(code_state) %>%
  summarise(total_pop = sum(pop_total)) %>%
  glimpse()

total_cases<-readRDS('case_data/weekly_sum_complete_gmsp_cases_5Oct.rds') %>%
  left_join(census_tract[,c("code_tract","code_muni","code_state")],by="code_tract") %>%
  dplyr::select(weeklysum,code_state,code_muni) %>%
  filter(code_muni %in% metro_sp_munis) %>%
  group_by(code_state) %>%
  summarise(total_cases = sum(weeklysum,na.rm=TRUE)) %>%
  glimpse()

threshold<-total_pop$total_pop/total_cases$total_cases
print(threshold)
  
  
