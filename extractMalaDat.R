library(dplyr)
library(ggplot2)

rm(list=ls(all=TRUE))

db <- src_sqlite("data/DHS-WA.sqlite")

# Get sex, age, malaria data for ALL individual
query <-
  "SELECT HV000 as country, HV001 as cluster, HV002 as hh, HVIDX as idv,
HV104 as sex, HML16A as age, HML32 as micro_mala, HML35 as rapid_mala
FROM Individual
WHERE (micro_mala IS NOT NULL OR rapid_mala IS NOT NULL) AND age IS NOT NULL"

idvWTest <- tbl(db, sql(query)) %>% collect()

idvWTest %>%
  group_by(country) %>%
  summarise(micro_na=mean(is.na(micro_mala)), rapid_na=mean(is.na(rapid_mala))) 

# remove unknowns
idvWTestNoUK <- idvWTest %>%
  filter(!is.na(micro_mala) & micro_mala <= 1) %>%
  filter(!is.na(rapid_mala) & rapid_mala <= 1)

# Which region the cluster is in
# Note: region and region2 have quite different names and can screw
# up map drawing
query <- "SELECT HV000 as country, DHSCLUST as cluster, DHSREGNA as region,
                  ADM1NAME as region2
          FROM GIS"

clust_region <- tbl(db, sql(query)) %>% collect()

# Adding other information from Children and Household table
query <- "SELECT i.HV000 as country, i.HV001 as cluster, i.HV002 as hh, i.HVIDX as idv,
h.HV025 as urban, HV112 as motherinhh, c.H22 as fever, c.HW4 as hwp
FROM Individual as i
INNER JOIN 'Household' as h
ON i.HV000 = h.HV000 AND i.HV001 = h.HV001 AND i.HV002 = h.HV002
LEFT JOIN 'Children' as c
ON i.HV000 = c.V000 AND i.HV001 = c.V001 AND i.HV002 = c.V002 AND i.HVIDX = c.B16"

idvOtherCov <- tbl(db, sql(query))

malaMaster <- idvWTestNoUK %>%
  left_join(idvOtherCov, copy=T)

# Pick 4 covariates (sex, age, urban and fever), remove entries without
# fever data
malaMaster <- malaMaster %>% 
  left_join(clust_region) %>%
  dplyr::select(country, region, region2, micro_mala, rapid_mala, sex, 
                age, urban, fever) %>%
  filter(fever<=1 & !is.na(fever))

# For estimating prevalence, don't need RDT test results
malaMasterPrev <- malaMaster %>%
  dplyr::select(-rapid_mala)
write.csv(malaMasterPrev, "data/malaMasterPrev.csv", row.names = F, quote = F)

# For estimating sensitivity
malaMasterMicPos <- malaMaster %>%
  filter(micro_mala == 1) %>%
  dplyr::select(-micro_mala)
write.csv(malaMasterMicPos, "data/malaMicPos.csv", row.names = F, quote = F)

# For estimating specificity
malaMasterMicNeg <- malaMaster %>%
  filter(micro_mala == 0) %>%
  dplyr::select(-micro_mala)
write.csv(malaMasterMicNeg, "data/malaMicNeg.csv", row.names = F, quote = F)