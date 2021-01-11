#########################################
# load libraries
library(tidyr)
library(RPostgreSQL)
library(plyr)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggsci)
library(gridExtra)
library(cowplot)
library(here)
library(sjPlot)
library(coin)
library(emmeans)
library(stargazer)
library(nnet)
#########################################
# boolean values for saving, username and password for accessing AACT database

savePlot = TRUE
saveData = TRUE
userAACT="USERNAME"
passwordAACT="PASSWORD"

#########################################
# create search parameters
startDate = as.Date("2009-01-01")
endDate = as.Date("2019-12-31")
countriesList = c("United States")
`%nin%` = Negate(`%in%`)
placeboString = c('placebo','standard of care','usual care')
placeboStringOnly = c('placebo')
standardCareString = c('standard of care','usual care')

# terms to search within conditions field
diseaseTerms = tolower(c('Diabetes Mellitus', 'Noninsulin-Dependent', 'Diabetes Mellitus',
                 'Ketosis-Resistant', 'Diabetes Mellitus', 'Ketosis Resistant',
                 'Ketosis-Resistant Diabetes Mellitus', 'Diabetes Mellitus', 'Non Insulin Dependent',
                 'Diabetes Mellitus', 'Non-Insulin-Dependent', 'Non-Insulin-Dependent Diabetes Mellitus',
                 'Diabetes Mellitus, Stable', 'Stable Diabetes Mellitus', 'Diabetes Mellitus, Type II',
                 'NIDDM', 'Diabetes Mellitus, Noninsulin Dependent', 'Diabetes Mellitus',
                 'Maturity-Onset', 'Diabetes Mellitus', 'Maturity Onset',
                 'Maturity-Onset Diabetes Mellitus', 'Maturity Onset Diabetes Mellitus',
                 'MODY', 'Diabetes Mellitus, Slow-Onset', 'Diabetes Mellitus, Slow Onset',
                 'Slow-Onset Diabetes Mellitus', 'Type 2 Diabetes Mellitus',
                 'Noninsulin-Dependent Diabetes Mellitus',
                 'Noninsulin Dependent Diabetes Mellitus',
                 'Maturity-Onset Diabetes', 'Diabetes, Maturity-Onset',
                 'Maturity Onset Diabetes', 'Type 2 Diabetes', 'Diabetes, Type 2',
                 'Diabetes Mellitus, Adult-Onset', 'Adult-Onset Diabetes Mellitus',
                 'Diabetes Mellitus, Adult Onset'))

# terms to exclude
termsSearchCondTitleExclude = c('blahblahblah')


#########################################

# connect to database
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, dbname="aact",host="aact-db.ctti-clinicaltrials.org",user=userAACT,password=passwordAACT,port=5432)

# begin loading, filtering, selecting tables

study_tbl_conditions = tbl(src=con, 'conditions')

# exact string matching original way
condsCond <- study_tbl_conditions %>% select(nct_id,downcase_name)  %>% filter(downcase_name %in% diseaseTerms) %>% collect
condsCond = condsCond %>% group_by(nct_id) %>% summarize(condsPaste = paste(downcase_name,collapse=", ")) %>% collect()

# match any substring, less specific 
#condsCond <- study_tbl_conditions %>% select(nct_id,downcase_name) %>% collect()
#condsCond <- condsCond %>% group_by(nct_id) %>% summarize(condsPaste = paste(downcase_name,collapse=", ")) %>% collect()
#condsCond <- condsCond %>% filter(str_detect(tolower(condsPaste),pattern = paste(diseaseTerms,collapse="|")) & !str_detect(tolower(condsPaste),pattern = paste(termsSearchCondTitleExclude,collapse="|"))) 

#study_tbl_conditions_other = tbl(src=con, 'browse_conditions')

#condsCond_other <- study_tbl_conditions_other %>% select(nct_id,mesh_term) %>% collect
#condsCond_other = condsCond_other %>% group_by(nct_id) %>% summarize(condsPaste = paste(mesh_term,collapse=", ")) %>% collect()


study_tbl = tbl(src=con,'studies')
#filter_dates <- study_tbl %>% select(official_title,study_first_posted_date,verification_date,start_date,start_month_year,nct_id,phase,last_known_status,study_type,enrollment,overall_status) %>% filter(study_type == 'Interventional')  %>% collect()
filter_dates <- study_tbl %>% select(official_title,study_first_posted_date,verification_date,start_date,start_month_year,nct_id,phase,last_known_status,study_type,enrollment,overall_status) %>% filter((start_date >= startDate) & (study_first_posted_date >= startDate) & (study_first_posted_date <= endDate)  & (study_type == 'Interventional'))  %>% collect()
filter_dates <- filter_dates %>%filter(nct_id %in% condsCond$nct_id) %>% mutate(phase = replace(phase, phase == "N/A", "Not Applicable"))

# update overall status with last known status 
filter_dates <- filter_dates %>% mutate(overall_status = case_when(!is.na(last_known_status) ~ last_known_status,
                                                                   TRUE ~ overall_status))
# search within title
trials_excluded_title <- filter_dates %>% filter(str_detect(tolower(official_title),pattern = paste(termsSearchCondTitleExclude,collapse="|"))) 

filter_dates <- filter_dates %>% filter(!str_detect(tolower(official_title),pattern = paste(termsSearchCondTitleExclude,collapse="|"))) 

# search within brief summary 
brief_summaries_tbl = tbl(src=con,"brief_summaries")
brief_summaries = brief_summaries_tbl %>% select(nct_id,description) %>% collect()
brief_summaries <- brief_summaries %>% filter(nct_id %in% filter_dates$nct_id)

trials_excluded_summary <- brief_summaries %>% filter(str_detect(tolower(description),pattern = paste(termsSearchCondTitleExclude,collapse="|"))) 

filter_dates <- filter_dates %>% filter(nct_id %nin% trials_excluded_summary$nct_id)

interventions_tbl = tbl(src=con,'interventions')
interventions = interventions_tbl %>% select(nct_id,name,description) %>% collect()
interventions <- interventions %>% group_by(nct_id) %>% summarize(name_comb =paste(name,collapse=", "),descrip_comb=paste(description,collapse=", "))

design_groups_tbl = tbl(src=con,'design_groups')
design_groups <- design_groups_tbl %>% select(nct_id,group_type,title,description) %>% collect()
design_groups_counted <- design_groups %>% group_by(nct_id) %>% tally()
design_groups_counted <- rename(design_groups_counted,number_of_arms = n)

design_groups <- design_groups %>% group_by(nct_id) %>% summarize(group_type_comb = paste(group_type,collapse=", "))

designTrial = design_groups %>% mutate(designGroup = case_when(str_detect(tolower(group_type_comb), pattern = paste('placebo comparator')) ~ 'Placebo Comparator',
                                                               str_detect(tolower(group_type_comb), pattern = paste('active comparator')) ~ 'Active Comparator',
                                                               str_detect(tolower(group_type_comb), pattern = paste('sham')) ~ 'Sham Comparator',
                                                               str_detect(tolower(group_type_comb), pattern = paste('treatment comparison')) ~ 'Treatment Comparison',
                                                               str_detect(tolower(group_type_comb), pattern = paste('active')) ~ 'Active Comparator',
                                                               str_detect(tolower(group_type_comb), pattern = paste('case')) ~ 'Case',
                                                               str_detect(tolower(group_type_comb), pattern = paste('control')) ~ 'Control',
                                                               str_detect(tolower(group_type_comb), pattern = paste('other')) ~ 'Other',
                                                               str_detect(tolower(group_type_comb), pattern = paste('no intervention')) ~ 'No intervention',
                                                               str_detect(tolower(group_type_comb), pattern = paste('null')) ~ 'Null',
                                                               str_detect(tolower(group_type_comb), pattern = paste('experimental')) ~ 'Experimental Only'))

designTrialSummaryCheck <- designTrial %>% group_by(designGroup) %>% tally()

designTrialCollapsed = design_groups %>% mutate(designGroup = case_when(str_detect(tolower(group_type_comb), pattern = paste('placebo comparator')) ~ 'Control Arm Present',
                                                                        str_detect(tolower(group_type_comb), pattern = paste('active comparator')) ~ 'Control Arm Present',
                                                                        str_detect(tolower(group_type_comb), pattern = paste('sham')) ~ 'Control Arm Present',
                                                                        str_detect(tolower(group_type_comb), pattern = paste('active')) ~ 'Control Arm Present',
                                                                        str_detect(tolower(group_type_comb), pattern = paste('control')) ~ 'Control Arm Present',
                                                                        str_detect(tolower(group_type_comb), pattern = paste('no intervention')) ~ 'Control Arm Present',
                                                                        str_detect(tolower(group_type_comb), pattern = paste('null')) ~ 'No Control Arm Present',
                                                                        str_detect(tolower(group_type_comb), pattern = paste('treatment comparison')) ~ 'No Control Arm Present',
                                                                        str_detect(tolower(group_type_comb), pattern = paste('case')) ~ 'No Control Arm Present',
                                                                        str_detect(tolower(group_type_comb), pattern = paste('other')) ~ 'No Control Arm Present',
                                                                        str_detect(tolower(group_type_comb), pattern = paste('experimental')) ~ 'Experimental Only'))

designTrialCollapsedSummaryCheck <- designTrialCollapsed %>% group_by(designGroup) %>% tally()


designTrialCollapsed$design_groups_counted = design_groups_counted$number_of_arms


# will fix any mislabeled experimental arm only trials later with searh
designTrialCollapsed = designTrialCollapsed %>% mutate(multi_arm = case_when(design_groups_counted==1 ~ 'Single-Arm Trial',
                                                                             (design_groups_counted>1 & designGroup == 'Control Arm Present') ~ 'Control Arm Present',
                                                                             (design_groups_counted>1 & designGroup =='No Control Arm Present') ~ 'No Control Arm Present',
                                                                             (design_groups_counted>1 & designGroup =='Experimental Only') ~ 'No Control Arm Present'))

designTrialCollapsedArmSummaryCheck <- designTrialCollapsed %>% group_by(multi_arm) %>% tally()


designTrialExamineExperimentalOnly <- designTrialCollapsed %>% filter(design_groups_counted>1 & designGroup == 'Experimental Only')

baseline_counts_tbl = tbl(src=con,'baseline_counts')
baseline_counts <- baseline_counts_tbl %>% select(nct_id,count) %>% collect()

design_tbl = tbl(src=con,'designs')
design = design_tbl %>% select(nct_id,intervention_model,allocation,masking) %>% collect()

location_tbl = tbl(src=con,'countries')

# check if country is the only one in a list 
locations = location_tbl %>% select(nct_id,name)  %>% collect()
locations <- locations %>%  group_by(nct_id) %>% summarize(countriesPaste = paste(name,collapse=", ")) %>% mutate(usaLoc = case_when(countriesPaste == countriesList ~ 'USA only',
                                                                                                                                     str_detect(countriesPaste, pattern = paste(countriesList)) ~ 'USA site present',
                                                                                                                                     TRUE ~ 'no site in USA'))


sponsor_tbl = tbl(src=con,'sponsors')
sponsor <- sponsor_tbl %>%  select(nct_id,agency_class,lead_or_collaborator,name)%>% collect()


sponsor = sponsor %>% group_by(nct_id) %>% mutate(funding = case_when(any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Industry',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='nih')) ~ 'NIH',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='u.s. fed')) ~ 'U.S. Fed',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Industry',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='nih')) ~ 'NIH',
                                                                      any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='u.s. fed')) ~ 'U.S. Fed',
                                                                      TRUE ~ 'Other'))

sponsor = distinct(sponsor,nct_id,.keep_all=TRUE) %>% select(nct_id,funding)

sponsorCombined <- sponsor_tbl %>%  select(nct_id,agency_class,lead_or_collaborator,name)%>% collect()
sponsorCombined = sponsorCombined %>% group_by(nct_id) %>% mutate(fundingComb = case_when(any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Industry',
                                                                                          any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='nih')) ~ 'Public',
                                                                                          any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='u.s. fed')) ~ 'Public',
                                                                                          any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Industry',
                                                                                          any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='nih')) ~ 'Public',
                                                                                          any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='u.s. fed')) ~ 'Public',
                                                                                          TRUE ~ 'Other'))

sponsorCombined = sponsorCombined %>% group_by(nct_id) %>% mutate(univHosp = case_when(any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Other',
                                                                                       any(str_detect(tolower(lead_or_collaborator), pattern = paste('collaborator')) & str_detect(tolower(agency_class),pattern='industry')) ~ 'Other',
                                                                                       any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(name),pattern='university')) ~ 'University',
                                                                                       any(str_detect(tolower(lead_or_collaborator), pattern = paste('lead')) & str_detect(tolower(name),pattern='hospital')) ~ 'Hospital',
                                                                                       TRUE ~ 'Other'))



sponsorCombined = distinct(sponsorCombined,nct_id,.keep_all=TRUE) %>% select(nct_id,fundingComb,univHosp)

calculatedValues_tbl = tbl(src=con,'calculated_values')
calculatedValues <- calculatedValues_tbl  %>% select(nct_id,were_results_reported,minimum_age_num,minimum_age_unit) %>% collect()
calculatedValues <- calculatedValues %>% filter((minimum_age_num >= 18) & (minimum_age_unit == 'Years'))

facilities_tbl = tbl(src=con,'facilities')
facilities <- facilities_tbl  %>% select(nct_id,status,name) %>%collect()
facilities_tabulated <- facilities %>% group_by(nct_id) %>% tally()
facilities_tabulated <- rename(facilities_tabulated,facilitiesCount = n)
facilities_tabulated <- facilities_tabulated %>% mutate(multisite = ifelse(facilitiesCount>1,TRUE,FALSE))

study_ref_tbl = tbl(src=con,'study_references')
study_ref <- study_ref_tbl %>% select(nct_id,pmid,reference_type,citation) %>% collect()
study_ref_tabulated <- study_ref %>% filter(reference_type == "results_reference") %>% group_by(nct_id) %>% tally()
study_ref_tabulated <- rename(study_ref_tabulated,pubCount = n)

# this is a join that includes all categories, but only ones that match the description 
joinedTable <- join_all(list(locations,interventions,design_groups_counted,design,designTrialCollapsed,filter_dates,facilities_tabulated,sponsor,sponsorCombined,calculatedValues),by='nct_id',type="full")
joinedTable <- joinedTable %>% filter((nct_id %in% filter_dates$nct_id) & (nct_id %in% calculatedValues$nct_id))

# get rid of any NA start dates
#joinedTable <- joinedTable[complete.cases(joinedTable$start_date),]

# this adds pub counts, and NAs for those that dont have pubs
joinedTable <- left_join(joinedTable,study_ref_tabulated,by='nct_id')

joinedTable <- joinedTable %>% mutate(pubCountBool = case_when(!is.na(pubCount) ~ 'TRUE',
                                                               TRUE ~ 'FALSE'))

joinedTable <- joinedTable %>% mutate(yearStart=year(joinedTable$study_first_posted_date))

# first pass of quality control, if a trial doesnt have a number of arms, and no placebo listed, then no control arm present/
# trials that have other matching criteria are added below 
joinedTable$multi_arm[is.na(joinedTable$number_of_arms) & (joinedTable$intervention_model == 'Parallel Assignment' | joinedTable$intervention_model == 'Crossover Assignment' | joinedTable$intervention_model == 'Factorial Group Assignment' | joinedTable$intervention_model == 'Sequential Assignment') & str_detect(tolower(joinedTable$name_comb), pattern = paste(placeboString,collapse="|"),negate=TRUE)] = 'No Control Arm Present'
noArmsListed = joinedTable[is.na(joinedTable$number_of_arms) & (joinedTable$intervention_model == 'Parallel Assignment' | joinedTable$intervention_model == 'Crossover Assignment' | joinedTable$intervention_model == 'Factorial Group Assignment' | joinedTable$intervention_model == 'Sequential Assignment') & str_detect(tolower(joinedTable$name_comb), pattern = paste(placeboString,collapse="|"),negate=TRUE),]

# fix NAs for single gorup assignment, by definition now no control arm present 
joinedTable$multi_arm[is.na(joinedTable$number_of_arms) & joinedTable$intervention_model == 'Single Group Assignment'] = 'Single-Arm Trial' 

# fix multiple experimental arms that have placebo listed under interventions 
joinedTable$multi_arm[joinedTable$number_of_arms > 1 & joinedTable$designGroup == 'Experimental Only' & str_detect(tolower(joinedTable$name_comb), pattern = paste(placeboString,collapse="|"))] = 'Control Arm Present'

joinedTable$multi_arm[joinedTable$active_placebo == 'No Intervention Comparator' |joinedTable$active_placebo == 'Placebo Comparator' | joinedTable$active_placebo == 'Active & Placebo Present' | joinedTable$active_placebo == 'Active Comparator']= 'Control Arm Present'


joinedTable$multi_arm[is.na(joinedTable$number_of_arms) & (joinedTable$intervention_model == 'Parallel Assignment' | joinedTable$intervention_model == 'Crossover Assignment' | joinedTable$intervention_model == 'Factorial Group Assignment' | joinedTable$intervention_model == 'Sequential Assignment') & str_detect(tolower(joinedTable$name_comb), pattern = paste(placeboString,collapse="|"))] = 'Control Arm Present'


# count number of missing columns
joinedTable<- joinedTable %>% mutate(numMissing = rowSums(is.na(.)))

# double check that no trials are double counted
doubleCounts <- joinedTable %>% group_by(nct_id) %>% summarise(count=n())
unique(doubleCounts$count)

# add in industry vs. non industry
joinedTable <- joinedTable %>% mutate(industryNonIndustry = case_when(str_detect(tolower(funding), pattern = paste('industry')) ~ 'Industry Sponsor',
                                                                      TRUE ~ 'Non-Industry Sponsor'))

# add in information about placebo, active comparator, both 
joinedTable <- joinedTable %>% mutate(active_placebo = case_when((str_detect(tolower(group_type_comb), pattern = paste('placebo comparator'))) & (str_detect(tolower(group_type_comb), pattern = paste('active comparator')))~ 'Active & Placebo Present',
                                                                 (str_detect(tolower(group_type_comb), pattern = paste('sham comparator'))) & (str_detect(tolower(group_type_comb), pattern = paste('active comparator')))~ 'Active & Placebo Present',
                                                                 str_detect(tolower(name_comb),  pattern = paste(placeboStringOnly,collapse="|")) ~ 'Placebo Comparator',
                                                                 str_detect(tolower(name_comb),  pattern = paste(standardCareString,collapse="|")) ~ 'Placebo Comparator',
                                                                 str_detect(tolower(group_type_comb), pattern = paste('active comparator')) ~ 'Active Comparator',
                                                                 str_detect(tolower(group_type_comb), pattern = paste('placebo comparator')) ~ 'Placebo Comparator',
                                                                 str_detect(tolower(group_type_comb),pattern=paste('sham comparator'))~ 'Placebo Comparator',
                                                                 str_detect(tolower(group_type_comb),pattern=paste('no intervention'))~ 'No Intervention Comparator'))

joinedTableFix <- joinedTable %>% filter((multi_arm != 'Control Arm Present') & (!is.na(active_placebo))) %>% mutate(multi_arm = case_when((str_detect(tolower(name_comb),pattern = paste(placeboStringOnly,collapse="|"))) |(str_detect(tolower(descrip_comb),pattern = paste(placeboStringOnly,collapse="|"))) ~'Control Arm Present',
                                                                                                                                           str_detect(tolower(designGroup),pattern='control arm present') ~ 'Control Arm Present'))


joinedTable$multi_arm[(joinedTable$multi_arm != 'Control Arm Present') & (!is.na(joinedTable$active_placebo))] = joinedTableFix$multi_arm


joinedTable$multi_arm[(joinedTable$multi_arm != 'Control Arm Present') & (!is.na(joinedTable$active_placebo))] = joinedTableFix$active_placebo

# do all the manual curation

#joinedTable$active_placebo[joinedTable$nct_id == 'NCT00846469'] = 'Active Comparator'
#joinedTable$multi_arm[joinedTable$nct_id == 'NCT00846469'] = 'Control Arm Present'

# create new group for control group present vs absent (lumping single arm in)
joinedTable <- joinedTable %>% mutate(control_status = case_when(multi_arm=='Control Arm Present' ~ 'Control Arm Present',
                                                                 ((multi_arm=='Single-Arm Trial') | (multi_arm=='No Control Arm Present')) ~ 'No Control Arm Present'))

# create column for condensed phase
joinedTable <- joinedTable %>% mutate(phase_condensed = case_when(phase=='Phase 4' ~ 'Phase 4',
                                                                 ((phase == 'Phase 3') | (phase == 'Phase 2/Phase 3')) ~ 'Phase 3',
                                                                 ((phase == 'Phase 2') | (phase == 'Phase 1/Phase 2')) ~ 'Phase 2',
                                                                 ((phase == 'Phase 1') | (phase == 'Early Phase 1')) ~ 'Phase 1',
                                                                 phase == 'Not Applicable' ~ 'Not Applicable'))

# create column for condensed status
joinedTable <- joinedTable %>% mutate(status_condensed = case_when(((overall_status == 'Not yet recruiting') | (overall_status == 'Active, not recruiting') | (overall_status == 'Enrolling by invitation') | (overall_status == 'Recruiting')) ~ 'In process',
                                                                   ((overall_status == 'Withdrawn') | (overall_status == 'Terminated') | (overall_status == 'Suspended')) ~ 'Discontinued',
                                                                  overall_status == 'Completed' ~ 'Completed',
                                                                  TRUE ~ 'Unknown'))



# create column for phase

# done processing, now do checks, totals, and calculations

joinedTableCheck <- joinedTable %>% filter((multi_arm != 'Control Arm Present') & (!is.na(active_placebo)))

joinedTableActivePlacebo <- joinedTable %>% filter(control_status=='Control Arm Present') %>% group_by(active_placebo) %>% tally()
joinedTableWhichDesign <- joinedTable %>% filter(multi_arm=='Control Arm Present') %>% group_by(intervention_model) %>% tally()
joinedTableWhichDesignTrials <- joinedTable %>% filter(multi_arm=='Control Arm Present') %>% group_by(intervention_model)

joinedTableActivePlaceboCheck <- joinedTable %>% filter(multi_arm!='Control Arm Present') %>% group_by(active_placebo) %>% tally()
joinedTableWhichDesignCheck <- joinedTable %>% filter(multi_arm!='Control Arm Present') %>% group_by(intervention_model) %>% tally()

joinedTableActivePlaceboCheckNoCtrl <- joinedTable %>% filter(multi_arm=='No Control Arm Present') %>% group_by(active_placebo) %>% tally()
joinedTableWhichDesignCheckNoCtrl <- joinedTable %>% filter(multi_arm=='No Control Arm Present') %>% group_by(intervention_model) %>% tally()
joinedTableWhichDesignCheckNoCtrlTrials <- joinedTable %>% filter(multi_arm=='No Control Arm Present') %>% group_by(intervention_model)

joinedTableDoubleCheck <- joinedTable %>% filter((multi_arm != 'Control Arm Present') & ((active_placebo == 'Active & Placebo Present') | (active_placebo == 'Active Comparator') | (active_placebo == 'Placebo Comparator') ))
joinedTableTripleCheck <- joinedTable %>% filter((multi_arm == 'Control Arm Present') & (is.na(active_placebo)))

# check single arm
joinedTableSingleCheck <- joinedTable %>% filter((multi_arm == 'Single-Arm Trial') & ((intervention_model == "Parallel Assignment") | (intervention_model == 'Crossover Assignment')))

# group by year and multi-arm group
joinedTableCount <- joinedTable %>% group_by(yearStart,control_status) %>%
  summarize(n=n()) %>%
  mutate(freq = n/sum(n))
joinedTableCount <- rename(joinedTableCount,yearlyCount = n)

# calculate statistics
joinedTableTotals <- joinedTable %>% group_by(control_status) %>% tally()

controlN =  joinedTableTotals$n[joinedTableTotals$control_status == 'Control Arm Present']
noControlN = joinedTableTotals$n[joinedTableTotals$control_status == 'No Control Arm Present']

joinedTableSummarize <- joinedTable %>% group_by(control_status) %>% summarize(median=median(number_of_arms,na.rm=TRUE),iqr = IQR(number_of_arms,na.rm=TRUE))

joinedTableSummarizeCountry <- joinedTable %>% group_by(control_status,usaLoc) %>% tally()
joinedTableSummarizeCountry <- joinedTableSummarizeCountry %>% mutate(totalN = case_when(control_status=='Control Arm Present' ~ controlN ,
                                                                      control_status=='No Control Arm Present' ~ noControlN))

joinedTableSummarizeType <- joinedTable %>% group_by(control_status,study_type) %>% tally()
joinedTableSummarizePhase <- joinedTable %>% group_by(control_status,phase_condensed) %>% tally()
joinedTableSummarizePhaseMore <- joinedTable %>% group_by(control_status,phase) %>% tally()
joinedTableSummarizeAgency <- joinedTable %>% group_by(control_status,fundingComb) %>% tally()
joinedTableSummarizeReported <- joinedTable %>% group_by(control_status,were_results_reported) %>% tally()
joinedTableSummarizeSite<- joinedTable %>% group_by(control_status,multisite) %>% tally()
joinedTableSummarizeStatus<- joinedTable %>% group_by(control_status,last_known_status) %>% tally()
joinedTableSummarizeOverallStatus <- joinedTable %>% group_by(control_status,status_condensed) %>% tally()
joinedTableSummarizePubCount <- joinedTable %>% group_by(control_status,pubCountBool) %>% tally()
joinedTableMedianNumbers <- joinedTable %>% filter(enrollment>0) %>% group_by(control_status) %>% summarize(median=median(enrollment,na.rm=TRUE),iqr = IQR(enrollment,na.rm=TRUE))
joinedTableUnivHosp <- joinedTable %>% filter((univHosp %in% c('University','Hospital')) & fundingComb == 'Other') %>% group_by(control_status,univHosp) %>% tally()

joinedTableAlloc <- joinedTable %>% filter(allocation=='Non-Randomized' | allocation=='Randomized')
joinedTableActivePlaceboAlloc <- joinedTableAlloc%>% group_by(allocation,active_placebo) %>% tally()

joinedTableSummarizeCountryAlloc <- joinedTableAlloc %>% group_by(allocation,usaLoc) %>% tally()

joinedTableSummarizeTypeAlloc <- joinedTableAlloc %>% group_by(allocation,study_type) %>% tally()
joinedTableSummarizePhaseAlloc <- joinedTableAlloc %>% group_by(allocation,phase_condensed) %>% tally()
joinedTableSummarizePhaseMoreAlloc <- joinedTableAlloc %>% group_by(allocation,phase) %>% tally()
joinedTableSummarizeAgencyAlloc <- joinedTableAlloc %>% group_by(allocation,fundingComb) %>% tally()
joinedTableSummarizeReportedAlloc <- joinedTableAlloc %>% group_by(allocation,were_results_reported) %>% tally()
joinedTableSummarizeSiteAlloc<- joinedTableAlloc %>% group_by(allocation,multisite) %>% tally()
joinedTableSummarizeStatusAlloc<- joinedTableAlloc %>% group_by(allocation,last_known_status) %>% tally()
joinedTableSummarizeOverallStatusAlloc <- joinedTableAlloc %>% group_by(allocation,status_condensed) %>% tally()
joinedTableSummarizePubCountAlloc <- joinedTableAlloc %>% group_by(allocation,pubCountBool) %>% tally()
joinedTableMedianNumbersAlloc <- joinedTableAlloc %>% filter(enrollment>0) %>% group_by(allocation) %>% summarize(median=median(enrollment,na.rm=TRUE),iqr = IQR(enrollment,na.rm=TRUE))
joinedTableUnivHospAlloc <- joinedTableAlloc %>% filter((univHosp %in% c('University','Hospital')) & fundingComb == 'Other') %>% group_by(allocation,univHosp) %>% tally()

joinedTableMask <- joinedTable %>% filter(masking=='None (Open Label)' | masking =='Single' | masking =='Double' | masking =='Triple' |masking =='Quadruple') %>% mutate(masking = recode(masking,"None (Open Label)" = "Not Blinded","Single"="Partially Blinded","Double"="Partially Blinded","Triple"="Partially Blinded","Quadruple"="Fully Blinded"))
joinedTableActivePlaceboMask <- joinedTableMask %>% group_by(masking,active_placebo) %>% tally()

joinedTableSummarizeCountryMask <- joinedTableMask %>% group_by(masking,usaLoc) %>% tally()

joinedTableSummarizeTypeMask <- joinedTableMask %>% group_by(masking,study_type) %>% tally()
joinedTableSummarizePhaseMask <- joinedTableMask %>% group_by(masking,phase_condensed) %>% tally()
joinedTableSummarizePhaseMoreMask <- joinedTableMask %>% group_by(masking,phase) %>% tally()
joinedTableSummarizeAgencyMask <- joinedTableMask %>% group_by(masking,fundingComb) %>% tally()
joinedTableSummarizeReportedMask <- joinedTableMask %>% group_by(masking,were_results_reported) %>% tally()
joinedTableSummarizeSiteMask<- joinedTableMask %>% group_by(masking,multisite) %>% tally()
joinedTableSummarizeStatusMask<- joinedTableMask %>% group_by(masking,last_known_status) %>% tally()
joinedTableSummarizeOverallStatusMask <- joinedTableMask %>% group_by(masking,status_condensed) %>% tally()
joinedTableSummarizePubCountMask <- joinedTableMask %>% group_by(masking,pubCountBool) %>% tally()
joinedTableMedianNumbersMask <- joinedTableMask %>% filter(enrollment>0) %>% group_by(masking) %>% summarize(median=median(enrollment,na.rm=TRUE),iqr = IQR(enrollment,na.rm=TRUE))
joinedTableUnivHospMask <- joinedTableMask %>% filter((univHosp %in% c('University','Hospital')) & fundingComb == 'Other') %>% group_by(masking,univHosp) %>% tally()


#########################################
# statistical testing

# by control status 

# group by year and multi-arm group
joinedTableCountCat <- joinedTable %>% mutate(yearStart = as.factor(yearStart)) %>% group_by(yearStart,control_status) %>%
  summarize(n=n()) %>%
  mutate(freq = n/sum(n))
joinedTableCountCat <- rename(joinedTableCountCat,yearlyCount = n)

joinedTableCat <- joinedTable %>% mutate(yearStart = as.factor(yearStart)) %>%
  mutate(control_status = recode(control_status,"No Control Arm Present" = 0,"Control Arm Present"=1))

stat_model_cat <- glm(control_status~yearStart,data=joinedTableCat,family=binomial(link="logit"))

summary(stat_model_cat)
confint(stat_model_cat)
emmeansModel <- emmeans(stat_model_cat,'yearStart',type='response')
pairs(emmeansModel,reverse=TRUE)
confint(emmeansModel)


joinedTableSampleSizeTest <- joinedTable %>% filter(enrollment>0) %>% select(control_status,yearStart,enrollment,multisite,status_condensed,usaLoc,fundingComb,phase,phase_condensed)
joinedTableSampleSizeTest$control_status <- as.factor(mapvalues(joinedTableSampleSizeTest$control_status,from=c('Control Arm Present','No Control Arm Present'),to=c(1,0)))
joinedTableSampleSizeTest$yearStart <- as.integer(mapvalues(joinedTableSampleSizeTest$yearStart,from=c(min(joinedTableSampleSizeTest$yearStart):max(joinedTableSampleSizeTest$yearStart)),to=c(0:(length(unique(joinedTableSampleSizeTest$yearStart))-1))))
which(! complete.cases(joinedTableSampleSizeTest))

medianSampleSize <- median_test(enrollment~control_status,data = joinedTableSampleSizeTest)

yearlyCount = joinedTableCount$yearlyCount
lengthYC= length(yearlyCount)

stat_model <- glm(control_status~yearStart,family=binomial(link="logit"),data=joinedTableSampleSizeTest)
summary(stat_model)
confint(stat_model)
tab_model(stat_model)

stat_model_group <- glm(control_status~yearStart+multisite+status_condensed+usaLoc+phase_condensed+fundingComb,family=binomial(link="logit"),data=joinedTableSampleSizeTest)
summary(stat_model_group)
tab_model(stat_model_group)
anova(stat_model_group,test="Chisq")
confint(stat_model_group)

tableControlStatusFreq <- joinedTableActivePlacebo %>% mutate(per = round(prop.table(n)*100,1))


tableCountry = table(joinedTable$usaLoc,joinedTable$control_status,useNA = 'ifany')
tableCountryFreq <- joinedTableSummarizeCountry %>% group_by(control_status) %>% mutate(per = round(prop.table(n)*100,1))
tableCountryStats <- sapply(1:nrow(tableCountry),function(z) prop.test(tableCountry[z,, drop = TRUE], n = colSums(tableCountry)))
chisq.test(tableCountry)

tableControlArm = table(joinedTable$active_placebo,joinedTable$control_status,useNA='ifany')
tableControlArmStats <- sapply(1:nrow(tableControlArm),function(z) prop.test(tableControlArm[z,, drop = TRUE], n = colSums(tableControlArm)))



tableStatus = table(joinedTable$status_condensed,joinedTable$control_status,useNA = 'ifany')
tableStatusFreq <- joinedTableSummarizeOverallStatus %>% group_by(control_status) %>% mutate(per = round(prop.table(n)*100,1))
tableStatusStats <- sapply(1:nrow(tableStatus),function(z) prop.test(tableStatus[z,, drop = TRUE], n = colSums(tableStatus)))
chisq.test(tableStatus)

tableSite = table(joinedTable$multisite,joinedTable$control_status,useNA = 'ifany')
tableSiteFreq <- joinedTableSummarizeSite %>% group_by(control_status) %>% mutate(per = round(prop.table(n)*100,1))
tableSiteStats <- sapply(1:nrow(tableSite),function(z) prop.test(tableSite[z,, drop = TRUE], n = colSums(tableSite)))
chisq.test(tableSite)


tableFunder = table(joinedTable$fundingComb,joinedTable$control_status,useNA = 'ifany')
tableFunderFreq <- joinedTableSummarizeAgency %>% group_by(control_status) %>% mutate(per = round(prop.table(n)*100,1))
tableFunderStats <- sapply(1:nrow(tableFunder),function(z) prop.test(tableFunder[z,, drop = TRUE], n = colSums(tableFunder)))
chisq.test(tableFunder)


tablePhase = table(joinedTable$phase_condensed,joinedTable$control_status,useNA = 'ifany')
tablePhaseFreq <- joinedTableSummarizePhase %>% group_by(control_status) %>% mutate(per = round(prop.table(n)*100,1))
tablePhaseStats <- sapply(1:nrow(tablePhase),function(z) prop.test(tablePhase[z,, drop = TRUE], n = colSums(tablePhase)))
chisq.test(tablePhase)


tablePub = table(joinedTable$pubCountBool,joinedTable$control_status,useNA = 'ifany')
tablePubFreq <- joinedTableSummarizePubCount %>% group_by(control_status) %>% mutate(per = round(prop.table(n)*100,1))
tablePubStats <- sapply(1:nrow(tablePub),function(z) prop.test(tablePub[z,, drop = TRUE], n = colSums(tablePub)))
chisq.test(tablePub)


tableResults = table(joinedTable$were_results_reported,joinedTable$control_status,useNA = 'ifany')
tableResultsFreq <- joinedTableSummarizeReported %>% group_by(control_status) %>% mutate(per = round(prop.table(n)*100,1))
tableResultsStats <- sapply(1:nrow(tableResults),function(z) prop.test(tableResults[z,, drop = TRUE], n = colSums(tableResults)))
chisq.test(tableResults)


tableYearlyCount = table(joinedTable$yearStart,joinedTable$control_status,useNA='ifany')
tableYearlyCountStats <- sapply(1:nrow(tableYearlyCount),function(z) prop.test(tableYearlyCount[z,, drop = TRUE], n = colSums(tableYearlyCount)))
chisq.test(tableYearlyCount)


# by randomization

joinedTableAlloc <- joinedTable %>% filter(allocation=='Non-Randomized' | allocation=='Randomized')

joinedTableCountAlloc <- joinedTableAlloc %>% group_by(yearStart,allocation) %>%
  summarize(n=n()) %>%
  mutate(freq = n/sum(n))
joinedTableCountAlloc <- rename(joinedTableCountAlloc,yearlyCount = n)

# group by year and multi-arm group
joinedTableCountCatAlloc <- joinedTableAlloc %>% mutate(yearStart = as.factor(yearStart)) %>% group_by(yearStart,allocation) %>%
  summarize(n=n()) %>%
  mutate(freq = n/sum(n))
joinedTableCountCatAlloc <- rename(joinedTableCountCatAlloc,yearlyCount = n)

joinedTableCatAlloc <- joinedTableAlloc %>% mutate(yearStart = as.factor(yearStart)) %>%
  mutate(allocation = recode(allocation,"Non-Randomized" = 0,"Randomized"=1))

stat_model_catAlloc <- glm(allocation~yearStart,data=joinedTableCatAlloc,family=binomial(link="logit"))

summary(stat_model_catAlloc)
confint(stat_model_catAlloc)
emmeansModelAlloc <- emmeans(stat_model_catAlloc,'yearStart',type='response')
pairs(emmeansModelAlloc,reverse=TRUE)
confint(emmeansModelAlloc)


joinedTableSampleSizeTestAlloc <- joinedTableAlloc %>% filter(enrollment>0) %>% select(allocation,control_status,yearStart,enrollment,multisite,status_condensed,usaLoc,fundingComb,phase,phase_condensed)
joinedTableSampleSizeTestAlloc$allocation <- as.factor(mapvalues(joinedTableSampleSizeTestAlloc$allocation,from=c('Randomized','Non-Randomized'),to=c(1,0)))
joinedTableSampleSizeTestAlloc$yearStart <- as.integer(mapvalues(joinedTableSampleSizeTestAlloc$yearStart,from=c(min(joinedTableSampleSizeTestAlloc$yearStart):max(joinedTableSampleSizeTestAlloc$yearStart)),to=c(0:(length(unique(joinedTableSampleSizeTestAlloc$yearStart))-1))))
which(! complete.cases(joinedTableSampleSizeTest))

medianSampleSizeAlloc <- median_test(enrollment~allocation,data = joinedTableSampleSizeTestAlloc)

yearlyCountAlloc = joinedTableCountAlloc$yearlyCount
lengthYCAlloc= length(yearlyCountAlloc)

stat_modelAlloc <- glm(allocation~yearStart,family=binomial(link="logit"),data=joinedTableSampleSizeTestAlloc)
summary(stat_modelAlloc)
confint(stat_modelAlloc)
tab_model(stat_modelAlloc)

tableControlAlloc = table(joinedTableAlloc$active_placebo,joinedTableAlloc$allocation,useNA = 'ifany')
tableControlFreqAlloc <- joinedTableActivePlaceboAlloc %>% group_by(allocation) %>% mutate(per = round(prop.table(n)*100,1))
tableControlStatsAlloc <- sapply(1:nrow(tableControlAlloc),function(z) prop.test(tableControlAlloc[z,, drop = TRUE], n = colSums(tableControlAlloc)))
chisq.test(tableControlAlloc)

tableCountryAlloc = table(joinedTableAlloc$usaLoc,joinedTableAlloc$allocation,useNA = 'ifany')
tableCountryFreqAlloc <- joinedTableSummarizeCountryAlloc %>% group_by(allocation) %>% mutate(per = round(prop.table(n)*100,1))
tableCountryStatsAlloc <- sapply(1:nrow(tableCountryAlloc),function(z) prop.test(tableCountryAlloc[z,, drop = TRUE], n = colSums(tableCountryAlloc)))
chisq.test(tableCountryAlloc)

tableControlArmAlloc = table(joinedTableAlloc$active_placebo,joinedTableAlloc$allocation,useNA='ifany')
tableControlArmStatsAlloc <- sapply(1:nrow(tableControlArmAlloc),function(z) prop.test(tableControlArmAlloc[z,, drop = TRUE], n = colSums(tableControlArmAlloc)))


tableStatusAlloc = table(joinedTableAlloc$status_condensed,joinedTableAlloc$allocation,useNA = 'ifany')
tableStatusFreqAlloc <- joinedTableSummarizeOverallStatusAlloc %>% group_by(allocation) %>% mutate(per = round(prop.table(n)*100,1))
tableStatusStatsAlloc <- sapply(1:nrow(tableStatusAlloc),function(z) prop.test(tableStatusAlloc[z,, drop = TRUE], n = colSums(tableStatusAlloc)))
chisq.test(tableStatusAlloc)

tableSiteAlloc = table(joinedTableAlloc$multisite,joinedTableAlloc$allocation,useNA = 'ifany')
tableSiteFreqAlloc <- joinedTableSummarizeSiteAlloc %>% group_by(allocation) %>% mutate(per = round(prop.table(n)*100,1))
tableSiteStatsAlloc <- sapply(1:nrow(tableSiteAlloc),function(z) prop.test(tableSiteAlloc[z,, drop = TRUE], n = colSums(tableSiteAlloc)))
chisq.test(tableSiteAlloc)

tableFunderAlloc = table(joinedTableAlloc$fundingComb,joinedTableAlloc$allocation,useNA = 'ifany')
tableFunderFreqAlloc <- joinedTableSummarizeAgencyAlloc %>% group_by(allocation) %>% mutate(per = round(prop.table(n)*100,1))
tableFunderStatsAlloc <- sapply(1:nrow(tableFunderAlloc),function(z) prop.test(tableFunderAlloc[z,, drop = TRUE], n = colSums(tableFunderAlloc)))
chisq.test(tableFunderAlloc)

tablePhaseAlloc = table(joinedTableAlloc$phase_condensed,joinedTableAlloc$allocation,useNA = 'ifany')
tablePhaseFreqAlloc <- joinedTableSummarizePhaseAlloc %>% group_by(allocation) %>% mutate(per = round(prop.table(n)*100,1))
tablePhaseStatsAlloc <- sapply(1:nrow(tablePhaseAlloc),function(z) prop.test(tablePhaseAlloc[z,, drop = TRUE], n = colSums(tablePhaseAlloc)))
chisq.test(tablePhaseAlloc)

tablePubAlloc = table(joinedTableAlloc$pubCountBool,joinedTableAlloc$allocation,useNA = 'ifany')
tablePubFreqAlloc <- joinedTableSummarizePubCountAlloc %>% group_by(allocation) %>% mutate(per = round(prop.table(n)*100,1))
tablePubStatsAlloc <- sapply(1:nrow(tablePub),function(z) prop.test(tablePub[z,, drop = TRUE], n = colSums(tablePub)))
chisq.test(tablePubAlloc)


tableResultsAlloc = table(joinedTableAlloc$were_results_reported,joinedTableAlloc$allocation,useNA = 'ifany')
tableResultsFreqAlloc <- joinedTableSummarizeReportedAlloc %>% group_by(allocation) %>% mutate(per = round(prop.table(n)*100,1))
tableResultsStatsAlloc <- sapply(1:nrow(tableResults),function(z) prop.test(tableResults[z,, drop = TRUE], n = colSums(tableResults)))
chisq.test(tableResultsAlloc)


tableYearlyCountAlloc = table(joinedTableAlloc$yearStart,joinedTableAlloc$allocation,useNA='ifany')
tableYearlyCountStatsAlloc <- sapply(1:nrow(tableYearlyCount),function(z) prop.test(tableYearlyCount[z,, drop = TRUE], n = colSums(tableYearlyCount)))
chisq.test(tableYearlyCount)

# by blinding

joinedTableCountMask <- joinedTableMask %>%  group_by(yearStart,masking) %>%
  summarize(n=n()) %>%
  mutate(freq = n/sum(n))
joinedTableCountMask <- rename(joinedTableCountMask,yearlyCount = n)

# group by year and multi-arm group
joinedTableCountCatMask <- joinedTableMask %>% mutate(yearStart = as.factor(yearStart)) %>% group_by(yearStart,masking) %>%
  summarize(n=n()) %>%
  mutate(freq = n/sum(n))
joinedTableCountCatMask <- rename(joinedTableCountCatMask,yearlyCount = n)

joinedTableCatMask <- joinedTableMask %>% mutate(yearStart = as.factor(yearStart)) %>%
  #mutate(masking = as.factor(recode(masking,"Not Blinded" = 0,"Partially Blinded"=1,"Fully Blinded"=2)))
  mutate(masking = as.factor(masking))
  
#stat_model_catMask <- glm(masking~yearStart,data=joinedTableCatMask,family=binomial(link="logit"))

#summary(stat_model_catMask)
#confint(stat_model_catMask)
#emmeansModelMask <- emmeans(stat_model_catMask,'yearStart',type='response')
#pairs(emmeansModelMask,reverse=TRUE)
#confint(emmeansModelMask)
joinedTableCatMask$masking2 = relevel(joinedTableCatMask$masking, ref = "Not Blinded")
stat_model_catMask <- multinom(masking2~yearStart,data=joinedTableCatMask)
summary(stat_model_catMask)
stargazer(stat_model_catMask,type="html",out="stat_model_catMask.htm")
confint(stat_model_catMask)

joinedTableSampleSizeTestMask <- joinedTableMask %>% filter(enrollment>0) %>% select(masking,yearStart,enrollment,multisite,status_condensed,usaLoc,fundingComb,phase,phase_condensed)
joinedTableSampleSizeTestMask$masking <- as.factor(mapvalues(joinedTableSampleSizeTestMask$masking,from=c('Fully Blinded','Partially Blinded','Not Blinded'),to=c(2,1,0)))
joinedTableSampleSizeTestMask$yearStart <- as.integer(mapvalues(joinedTableSampleSizeTestMask$yearStart,from=c(min(joinedTableSampleSizeTestMask$yearStart):max(joinedTableSampleSizeTestMask$yearStart)),to=c(0:(length(unique(joinedTableSampleSizeTestMask$yearStart))-1))))
which(! complete.cases(joinedTableSampleSizeTestMask))

medianSampleSizeMask <- median_test(enrollment~masking,data = joinedTableSampleSizeTestMask)

yearlyCountMask = joinedTableCountMask$yearlyCount
lengthYC= length(yearlyCountMask)

stat_modelMask <- glm(masking~yearStart,family=binomial(link="logit"),data=joinedTableSampleSizeTestMask)
summary(stat_modelMask)
confint(stat_modelMask)
tab_model(stat_modelMask)

tableControlMask = table(joinedTableMask$active_placebo,joinedTableMask$masking,useNA = 'ifany')
tableControlFreqMask <- joinedTableActivePlaceboMask %>% group_by(masking) %>% mutate(per = round(prop.table(n)*100,1))
tableControlStatsMask <- sapply(1:nrow(tableControlMask),function(z) prop.test(tableControlMask[z,, drop = TRUE], n = colSums(tableControlMask)))
chisq.test(tableControlMask)

tableCountryMask = table(joinedTableMask$usaLoc,joinedTableMask$masking,useNA = 'ifany')
tableCountryFreqMask <- joinedTableSummarizeCountryMask %>% group_by(masking) %>% mutate(per = round(prop.table(n)*100,1))
tableCountryStatsMask <- sapply(1:nrow(tableCountry),function(z) prop.test(tableCountryMask[z,, drop = TRUE], n = colSums(tableCountryMask)))
chisq.test(tableCountryMask)

tableControlArmMask = table(joinedTableMask$active_placebo,joinedTableMask$masking,useNA='ifany')
tableControlArmStatsMask <- sapply(1:nrow(tableControlArmMask),function(z) prop.test(tableControlArmMask[z,, drop = TRUE], n = colSums(tableControlArmMask)))

tableStatusMask = table(joinedTableMask$status_condensed,joinedTableMask$masking,useNA = 'ifany')
tableStatusFreqMask <- joinedTableSummarizeOverallStatusMask %>% group_by(masking) %>% mutate(per = round(prop.table(n)*100,1))
tableStatusStatsMask <- sapply(1:nrow(tableStatus),function(z) prop.test(tableStatusMask[z,, drop = TRUE], n = colSums(tableStatusMask)))
chisq.test(tableStatusMask)

tableSiteMask = table(joinedTableMask$multisite,joinedTableMask$masking,useNA = 'ifany')
tableSiteFreqMask <- joinedTableSummarizeSiteMask %>% group_by(masking) %>% mutate(per = round(prop.table(n)*100,1))
tableSiteStatsMask <- sapply(1:nrow(tableSiteMask),function(z) prop.test(tableSiteMask[z,, drop = TRUE], n = colSums(tableSiteMask)))
chisq.test(tableSiteMask)


tableFunderMask = table(joinedTableMask$fundingComb,joinedTableMask$masking,useNA = 'ifany')
tableFunderFreqMask <- joinedTableSummarizeAgencyMask %>% group_by(masking) %>% mutate(per = round(prop.table(n)*100,1))
tableFunderStatsMask <- sapply(1:nrow(tableFunderMask),function(z) prop.test(tableFunderMask[z,, drop = TRUE], n = colSums(tableFunderMask)))
chisq.test(tableFunder)


tablePhaseMask = table(joinedTableMask$phase_condensed,joinedTableMask$masking,useNA = 'ifany')
tablePhaseFreqMask <- joinedTableSummarizePhaseMask %>% group_by(masking) %>% mutate(per = round(prop.table(n)*100,1))
tablePhaseStatsMask <- sapply(1:nrow(tablePhaseMask),function(z) prop.test(tablePhaseMask[z,, drop = TRUE], n = colSums(tablePhaseMask)))
chisq.test(tablePhaseMask)


tablePubMask = table(joinedTableMask$pubCountBool,joinedTableMask$masking,useNA = 'ifany')
tablePubFreqMask <- joinedTableSummarizePubCountMask %>% group_by(masking) %>% mutate(per = round(prop.table(n)*100,1))
tablePubStatsMask <- sapply(1:nrow(tablePubMask),function(z) prop.test(tablePubMask[z,, drop = TRUE], n = colSums(tablePubMask)))
chisq.test(tablePubMask)


tableResultsMask = table(joinedTableMask$were_results_reported,joinedTableMask$masking,useNA = 'ifany')
tableResultsFreqMask <- joinedTableSummarizeReportedMask %>% group_by(masking) %>% mutate(per = round(prop.table(n)*100,1))
tableResultsStatsMask <- sapply(1:nrow(tableResultsMask),function(z) prop.test(tableResultsMask[z,, drop = TRUE], n = colSums(tableResultsMask)))
chisq.test(tableResultsMask)


tableYearlyCountMask = table(joinedTableMask$yearStart,joinedTableMask$masking,useNA='ifany')
tableYearlyCountStatsMask <- sapply(1:nrow(tableYearlyCountMask),function(z) prop.test(tableYearlyCountMask[z,, drop = TRUE], n = colSums(tableYearlyCountMask)))
chisq.test(tableYearlyCountMask)


########################
if (saveData){
  saveRDS(joinedTable, file = "diabetesRdata_1_11_2021.rds")
  #write.csv(designTrialExamineExperimentalOnly,'experimentalOnly_1_11_2021.csv')
  write.csv(joinedTable,'diabetesTableTotal_1_11_2021.csv')
  #write.csv(joinedTableDiverseDiscontinued,'diabetesTableDiscDiverse_1_11_2021.csv')
  #write.csv(joinedTableSummarizeInterv,'diabetesTableInterv_1_11_2021.csv')
  #write.csv(joinedTableSummarizeType,'diabetesTableType_1_11_2021.csv')
  #write.csv(joinedTableSummarizePhase,'diabetesTablePhase_1_11_2021.csv')
  #write.csv(joinedTableSummarizeAgency,'diabetesTableAgency_1_11_2021.csv')
  #write.csv(joinedTableSummarizeReported,'diabetesTableReported_1_11_2021.csv')
  #write.csv(joinedTableSummarizeSite,'diabetesTableSite_1_11_2021.csv')
  #write.csv(joinedTableSummarizeStatus,'diabetesTableStatus_1_11_2021.csv')
  #write.csv(joinedTableSummarizeOverallStatus,'diabetesTableOverallStatus_1_11_2021.csv')
  #write.csv(joinedTableSummarizePubCount,'diabetesTablePubCount_1_11_2021.csv')
}

#########################################

# make plots
pInd<-ggplot(joinedTableCount, aes(x=yearStart,y=yearlyCount, group=control_status, color=control_status)) +
  geom_line()+
  geom_point() +
  labs(title="Number of Diabetes Clinical Trials \nRegistered by Control Arm Status, by Year",x = "Year Registered",y="Number of Trials",color='Control Arm Status') +
  ylim(0,max(joinedTableCount$yearlyCount)+10) +
  scale_x_continuous(breaks=seq(2009,2019,1),limits=c(2009,2019)) +
  scale_color_jama()
print(pInd)
if (savePlot){
  ggsave("trialsByYearMultiArm_1_11_2021.png", units="in", width=6, height=4, dpi=600)
}

# make plots
pInd<-ggplot(joinedTableCountAlloc, aes(x=yearStart,y=yearlyCount, group=allocation, color=allocation)) +
  geom_line()+
  geom_point() +
  labs(title="Number of Diabetes Clinical Trials \nRegistered by Randomization, by Year",x = "Year Registered",y="Number of Trials",color='Randomization') +
  ylim(0,max(joinedTableCountAlloc$yearlyCount)+10) +
  scale_x_continuous(breaks=seq(2009,2019,1),limits=c(2009,2019)) +
  scale_color_jama()
print(pInd)
if (savePlot){
  ggsave("trialsByYearAlloc_1_11_2021.png", units="in", width=6, height=4, dpi=600)
}

# make plots
pInd<-ggplot(joinedTableCountMask, aes(x=yearStart,y=yearlyCount, group=masking, color=masking)) +
  geom_line()+
  geom_point() +
  labs(title="Number of Diabetes Clinical Trials \nRegistered by Masking, by Year",x = "Year Registered",y="Number of Trials",color='Masking') +
  ylim(0,max(joinedTableCountMask$yearlyCount)+10) +
  scale_x_continuous(breaks=seq(2009,2019,1),limits=c(2009,2019)) +
  scale_color_jama()
print(pInd)
if (savePlot){
  ggsave("trialsByYearMask_1_11_2021.png", units="in", width=6, height=4, dpi=600)
}

#pHist<-ggplot(joinedTable, aes(x=number_of_arms,color=control_status,fill=control_status)) +
#  geom_histogram(binwidth=1,alpha=0.5) +
#  labs(x = "Number of Arms",y="Count",fill='Control Arm Status') +
#  coord_cartesian(xlim=c(0,max(joinedTable$number_of_arms,na.rm = TRUE)))  +
#  guides(color=FALSE)
#print(pHist)
#if (savePlot){
#  ggsave("trialsByYearHist_1_11_2021.png", units="in", width=5, height=4, dpi=600)
#}
