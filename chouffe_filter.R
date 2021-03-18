require(RPostgres)
require(rpostgis)
require(sf)
require(dplyr)
require(caret)
require(mapview)
require(rgeos)
require(psych)

setwd("C:/Users/morleyd/OneDrive - TomTom/Desktop")
source("chouffe_filter_pgfunctions.R")
source("chouffe_filter_Rfunctions.R")


################################
## PG Conn to validation results
################################

#to.check <- "subsets.train_geom"
#labels <- "hackaton.labels"
#full.osm <- "hackaton.atlas"

user <-     "dba_admin@psql-amf-matcher-dev"
password <- ""
host <-     "psql-amf-matcher-dev.postgres.database.azure.com"
port <-     5432
database <- "labels"
schema <-   "labeling_week_20210215_nld"

to.check <- "nld_validation_results"
full.atlas <- "nld_atlas"
full.mds <- "nld_mds"

con <- dbConnect(RPostgres::Postgres(), dbname = database, host = host, port = port,
                 options= paste0("-c search_path=", schema, ",public"),
                 user = user, password = password, sslmode = 'require')

create.custom.pg.funcs(con)

################################
## Stupid Matcher
################################


## 1) For each mds case in the validation table, get the nearby osm roads that could be matches
execute.pg(con, "drop table if exists situation")
execute.pg(con, sprintf(
  "create temp table situation as
   select c.mds_id, d.id as atlas_id, c.mds_geom, d.geom as atlas_geom from
  	(select a.mds_id, b.geom as mds_geom 
  	from %s a left join %s b
  	on a.mds_id = b.id) c
  inner join %s d 
  on st_dwithin(c.mds_geom, d.geom, 0.00001 * 15)", to.check, full.mds, full.atlas))
execute.pg(con, "create index situation_indx1 on situation using gist(mds_geom)")
execute.pg(con, "create index situation_indx2 on situation using gist(atlas_geom)")


## 2) Get the features of the stupid matcher
execute.pg(con, "drop table if exists stupid_results")
execute.pg(con, 
  "create temp table stupid_results as
    with lgths as (
      	select 
      		mds_geom, atlas_geom, mds_id, atlas_id,
      		coalesce(common_lines(atlas_geom, mds_geom, 50), atlas_geom) as atlas_intr, 
      		coalesce(common_lines(mds_geom, atlas_geom, 50), mds_geom) as mds_intr
      	from 
      	situation
      )
      select 
      	mds_geom, atlas_geom, mds_id, atlas_id,
      	log((st_distance(mds_intr, atlas_intr) / 0.00001) + 1) as min_sep,
      	log((coalesce(
      		 greatest(
      			st_length(st_intersection(st_buffer(atlas_geom, 0.00001 * 10), mds_geom)),
      			st_length(st_intersection(st_buffer(mds_geom, 0.00001 * 10), atlas_geom))), 0) / 0.00001) + 1) as common,
      	greatest(
      		st_length(atlas_intr) / st_length(atlas_geom), 
      		st_length(mds_intr) / st_length(mds_geom)
      	) as intr,
      	normalised_azi_diff(mds_intr, atlas_intr) as azi_diff		
      from lgths")


## 3) Join back results of validation, including 'no match' cases from the situation
execute.pg(con, "drop table if exists valid_to_check")
execute.pg(con, sprintf("create temp table valid_to_check as 
                       select distinct a.*, coalesce(b.label, 'NO MATCH') as human from stupid_results a left join %s b
                       on a.atlas_id = b.atlas_id and a.mds_id = b.mds_id", to.check))

## 4) Put back into R
d.full <- st_read(con, query = "select * from valid_to_check where human != 'NOT SURE'")
d.full$id <- 1:nrow(d.full) ## add a unique key
d.full[which(d.full$human == "NO MATCH"), "human"] <- "NOMATCH" ## R doesn't like the space

head(d.full)
summary(d.full)
hist(d.full$azi_diff)
hist(d.full$intr)
hist(d.full$min_sep)
hist(d.full$common)

d <- data.frame(d.full)
d <- data.frame(d.full[1:2000,])


#############################
## Make filter SVMs
#############################

## human: is MATCH, NOMATCH as labelled by a person
## predicted: is MATCH, NOMATCH as predicted by the model

## 1) SVM on the full dataset
svm.A <- fit.tuned.svm(d)
get.metrics(svm.A, d)

## 2) Split training set into the SVs and non-SVs
SV <- unlist(slot(svm.A$finalModel, "alphaindex"))
non.SV <- setdiff(as.integer(row.names(d)), SV)
d.non.sv <- d[which(row.names(d) %in% non.SV), ]
d.sv <- d[which(row.names(d) %in% SV), ]

## 3) Fit new SVM model B on the non-SV set
svm.B <- fit.tuned.svm(d.non.sv)
get.metrics(svm.B, d.non.sv) 

## 4) Use model B to predict on the SV partition
pred.B <- predict(svm.B, type = "prob", newdata = d.sv)
get.metrics(svm.B, d.sv)

## 5) Sort SV partition on probablity of misclassification
miss.class <- cbind(pred.B, d.sv)
miss.class[, c(1, 2)] <- lapply(miss.class[, c(1, 2)], round, 4)

FP <- miss.class %>% filter(human == "MATCH") %>% filter(MATCH < 0.5) %>% arrange(desc(NOMATCH))
FN <- miss.class %>% filter(human == "NOMATCH") %>% filter(MATCH >= 0.5) %>% arrange(desc(MATCH))



#############################
## Explore results
#############################
a <- FP
a <- FN
a$rid <- 1:nrow(a)

r <- 1

mapview(SpatialLinesDataFrame(rbind(
  readWKT(st_as_text(a[r, "atlas_geom"]), p4s = CRS("+init=epsg:4326")), 
  readWKT(st_as_text(a[r, "mds_geom"]), p4s = CRS("+init=epsg:4326"))
), data = data.frame(z = c(1, 2)), match.ID = FALSE), lwd = 4, zcol = "z",
legend = FALSE, map.types = c("Esri.WorldImagery", "OpenStreetMap.Mapnik"))
a[r, ] %>% select(-(c("mds_geom", "atlas_geom")))
#d %>% filter(id == a[r, "id"])
r <- r + 1


#############################
## Write results back to DB
#############################

prob.cutoff <- 0.99

xxx <- rbind(
  miss.class %>% filter(human == "MATCH") %>% filter(NOMATCH > prob.cutoff),
  miss.class %>% filter(human == "NOMATCH") %>% filter(MATCH > prob.cutoff)) %>% select(MATCH, human, mds_id, atlas_id)

head(xxx)

dbDisconnect(con)



