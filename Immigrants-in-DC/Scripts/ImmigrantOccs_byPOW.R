#Clear Environment
rm(list = ls())
gc()

#Load Packages
require(reshape2)
require(ergm)
require(dplyr)

#Load Data
occs <- list()
vars <- c('POWPUMA', 'POWSP','PWGTP','CIT', 'MIL',
          'OCCP', 'JWAP','WAGP','SEMP','PERNP')
occs[['dc_occ']] <- read.csv('../Data/ss15pdc.csv', stringsAsFactors = FALSE)[,vars]
occs[['va_occ']] <- read.csv('../Data/ss15pva.csv', stringsAsFactors = FALSE)[,vars]
occs[['md_occ']] <- read.csv('../Data/ss15pmd.csv', stringsAsFactors = FALSE)[,vars]
occs <- do.call(rbind, occs)
rm(vars)
msa_crosswalk <- read.csv('../Data/msa_puma_crosswalk.csv', stringsAsFactors = FALSE)
occ_index <- read.csv('../Data/2010 Occupation Index.csv', stringsAsFactors = FALSE)
occ_names <- occ_index$Name
names(occ_names) <- occ_index$Census2010

#Subset to those who work in DC (11), MD (24), and VA (51) around DC
occs <- occs[occs$POWSP %in% c(11,24,51),]
occs <- occs[occs$POWPUMA %in% msa_crosswalk$puma,]

#Map Citizenship and State
citMap <- c('BirthCitizen', "BirthCitizen",'BirthCitizen',
            'Immigrant', 'Immigrant')
occs$CIT <- citMap[occs$CIT]
occs$POWSP <- as.factor(occs$POWSP)
levels(occs$POWSP) <- c('DC', 'MD', 'VA')
rm(citMap)

#Aggregate
occ_totals <- occs %>% group_by(Occupation=OCCP,
                                State = POWSP) %>%
  summarise(medianWage = wtd.median(WAGP, weight = PWGTP, na.rm = TRUE),
            totalPop = sum(PWGTP))

occ_pivot <- occs %>% group_by(Citizenship=CIT,
                               Occupation=OCCP,
                               State=POWSP) %>%
  summarise(partialPop = sum(PWGTP))

occ_pivot <- dcast(occ_pivot, Occupation + State ~ Citizenship,
                   fill = 0, value.var = 'partialPop')

occ_pivot <- occ_pivot %>% left_join(occ_totals, by = c('Occupation', 'State'))

#Total and proportion
occ_pivot$Immigrant_Prop <- floor(occ_pivot$Immigrant / occ_pivot$totalPop * 100)

#Name Occupations
occ_pivot$OccName <- occ_names[as.character(occ_pivot$Occupation)]

#Wage Lev
occ_pivot$wageLev <- 'Mid-wage job'
occ_pivot$wageLev <- ifelse(occ_pivot$medianWage >= 85000, 'High-wage job', occ_pivot$wageLev) 
occ_pivot$wageLev <- ifelse(occ_pivot$medianWage <= 32000, 'Low-wage job', occ_pivot$wageLev) 

#Subset
# occ_totals <- occs %>% group_by(Occupation=OCCP) %>%
#   summarise(totalPop = sum(PWGTP)) %>% filter(totalPop > 10000) 
#occ_pivot <- occ_pivot[occ_pivot$Occupation %in% occ_totals$Occupation,]
occ_pivot <- occ_pivot[occ_pivot$totalPop >= 3000,]
#occ_pivot <- occ_pivot[occ_pivot$medianWage > 0,]

#Export
write.csv(occ_pivot, "../Data/TopImmigrantOccupations_byPOW.csv", row.names = FALSE, na = '0')

