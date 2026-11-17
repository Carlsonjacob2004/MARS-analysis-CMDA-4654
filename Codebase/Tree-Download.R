library(dplyr)


#'https://apps.fs.usda.gov/fia/datamart/datamart.html'

# ---- 0. Original (uncleaned) tables -----------------------------------------
# CN / PLT_CN are long ID numbers: read as text so they match exactly in joins
trees <- read.csv("VA_TREE.csv", colClasses = c(CN = "character", PLT_CN = "character"))
plot  <- read.csv("VA_PLOT.csv", colClasses = c(CN = "character"))
cond  <- read.csv("VA_COND.csv", colClasses = c(CN = "character", PLT_CN = "character"))

dim(trees); dim(plot); dim(cond)

# ---- 1. Clean PLOT: sampled forest plots, most recent visit only ------------
plot_clean <- plot %>%
  filter(PLOT_STATUS_CD == 1) %>%                    # 1 = sampled, forest present
  group_by(STATECD, UNITCD, COUNTYCD, PLOT) %>%      # same physical plot over time
  slice_max(MEASYEAR, n = 1, with_ties = FALSE) %>%  # keep latest visit
  ungroup() %>%
  select(PLT_CN = CN, MEASYEAR, ELEV, LAT, LON)

# ---- 2. Clean COND: accessible forest land only ------------------------------
cond_clean <- cond %>%
  filter(COND_STATUS_CD == 1) %>%                    # 1 = accessible forest land
  select(PLT_CN, CONDID, SLOPE, ASPECT, PHYSCLCD, FORTYPCD,
         STDAGE, SITECLCD, BALIVE, STDSZCD)

# ---- 3. Clean TREE: live, field-measured, unbroken, standard-size trees -----
trees_clean <- trees %>%
  filter(STATUSCD == 1,                     # 1 = live
         HTCD == 1,                         # 1 = height measured in field
         !is.na(HT), !is.na(ACTUALHT),
         HT == ACTUALHT,                    # no broken top
         DIA >= 5,                          # subplot trees (>= 5 in.)
         HT > 0) %>%
  select(PLT_CN, CONDID, HT, DIA, CR, CCLCD, SPCD, SPGRPCD, TREECLCD, AGENTCD)

# ---- 4. Merge ---------------------------------------------------------------
fia <- trees_clean %>%
  inner_join(plot_clean, by = "PLT_CN") %>%              # latest visits only
  inner_join(cond_clean, by = c("PLT_CN", "CONDID"))

cat("Trees after merge:", nrow(fia), "\n")

# ---- 5. Post-merge cleaning + recoding --------------------------------------
fia <- fia %>%
  # implausible height (ft) / diameter (ft) combinations
  mutate(hd_ratio = HT / (DIA / 12)) %>%
  filter(hd_ratio > 10, hd_ratio < 200) %>%
  # ASPECT is circular (0 = 360 = north) -> northness: 1 north, -1 south
  mutate(northness = cos(ASPECT * pi / 180)) %>%
  # AGENTCD: missing/0 = no damage
  mutate(AGENTCD = if_else(is.na(AGENTCD), 0L, as.integer(AGENTCD))) %>%
  # lump rare species and forest types (< 50 trees) into OTHER
  add_count(SPCD, name = "n_spcd") %>%
  mutate(SPCD = if_else(n_spcd >= 50, as.character(SPCD), "OTHER")) %>%
  add_count(FORTYPCD, name = "n_fortyp") %>%
  mutate(FORTYPCD = if_else(n_fortyp >= 50, as.character(FORTYPCD), "OTHER"))

# ---- 6. Final modeling table (all NAs dropped) -------------------------------
fia_model <- fia %>%
  select(HT,                                                   # response
         DIA, CR, CCLCD, SPCD, SPGRPCD, TREECLCD, AGENTCD,     # TREE (7)
         ELEV, LAT, LON, MEASYEAR,                             # PLOT (4)
         SLOPE, northness, PHYSCLCD, FORTYPCD, STDAGE,         # COND (8)
         SITECLCD, BALIVE, STDSZCD,
         PLT_CN) %>%                                           # ID for split only
  na.omit() %>%
  mutate(across(c(CCLCD, SPCD, SPGRPCD, TREECLCD, AGENTCD,
                  PHYSCLCD, FORTYPCD, STDSZCD), as.factor))

# ---- 7. Checks --------------------------------------------------------------
cat("Trees before NA drop:", nrow(fia), "\n")
cat("Trees in final table:", nrow(fia_model), "\n")
cat("Predictors:", ncol(fia_model) - 2, "\n")
colSums(is.na(fia[, c("CR", "CCLCD", "STDAGE", "SITECLCD", "BALIVE", "ELEV", "SLOPE")]))

table(fia_model$CCLCD)
table(fia_model$TREECLCD)
table(fia_model$AGENTCD)
table(fia_model$PHYSCLCD)
table(fia_model$STDSZCD)
summary(fia_model[, c("HT", "DIA", "CR", "ELEV", "STDAGE", "BALIVE")])

fia_model['HT'] = 'Tree Height'
fia <- fia %>%
  rename(tree_height = HT,
         diameter = DIA,
         tree_height_to_cron = CR,
         light_position_to_neighbors = CCLCD,
         Species = SPCD,
         Group_Code = SPGRPCD,
         damage_type = AGENTCD,
         elevation = ELEV,
         latitude = LAT,
         longitude = LON,
         year_measured = MEASYEAR,
         Slope_direction = northness,
         forest_type = FORTYPCD,
         age = STDAGE,
         potention_growth = SITECLCD,
         live_basal_area = BALIVE,
         stand_size = STDSZCD
         )
write.csv(fia, "fia_model.csv", row.names = FALSE)
write.csv(fia_model, "fia_trees_model.csv", row.names = FALSE)

####################################################################

data_dictionary <- data.frame(
  variable = c("HT", "DIA", "CR", "CCLCD", "SPCD", "SPGRPCD", "TREECLCD", "AGENTCD",
               "ELEV", "LAT", "LON", "MEASYEAR",
               "SLOPE", "northness", "PHYSCLCD", "FORTYPCD", "STDAGE",
               "SITECLCD", "BALIVE", "STDSZCD", "PLT_CN"),
  description = c(
    "Total tree height from ground to tip (field measured, unbroken top)",
    "Stem diameter at breast height (4.5 ft); trees >= 5 in. only",
    "Compacted crown ratio: percent of tree height with live crown",
    "Crown class: tree's light position relative to neighbors",
    "FIA species code; species with < 50 trees lumped as OTHER",
    "FIA species group code",
    "Tree class: timber quality",
    "Primary damage agent",
    "Plot elevation above sea level",
    "Plot latitude (publicly fuzzed up to ~1 mile)",
    "Plot longitude (publicly fuzzed up to ~1 mile)",
    "Year the plot was measured",
    "Slope steepness",
    "cos(ASPECT): direction the slope faces",
    "Physiographic class: landform and moisture regime",
    "Forest type code; types with < 50 trees lumped as OTHER",
    "Average age of dominant trees in the stand",
    "Site productivity class (potential wood growth)",
    "Live tree basal area per acre (stand crowding)",
    "Stand size class",
    "Plot control number; not used as a predictor"),
  codes = c(
    "", "", "",
    "1=open grown; 2=dominant; 3=codominant; 4=intermediate; 5=overtopped",
    "e.g. 131=loblolly pine; 316=red maple; 621=yellow-poplar; 802=white oak; 832=chestnut oak; 833=northern red oak; OTHER=rare species",
    "See FIADB User Guide species group appendix",
    "2=growing stock; 3=rough cull; 4=rotten cull",
    "0=none; 10=insect; 20=disease; 30=fire; 40=animal; 50=weather; 60=vegetation; 70=unknown/other; 80=human/silvicultural",
    "", "", "", "",
    "",
    "1=north-facing; 0=east/west; -1=south-facing",
    "11=dry tops; 12=dry slopes; 13=deep sands; 19=other xeric; 21=flatwoods; 22=rolling uplands; 23=moist slopes/coves; 24=narrow floodplains; 25=broad floodplains; 29=other mesic; 31=swamps/bogs; 32=small drains; 33=bays/wet pocosins; 34=beaver ponds; 35=cypress ponds; 39=other hydric",
    "e.g. 161=loblolly pine; 503=white oak/red oak/hickory; OTHER=rare types; see FIADB User Guide forest type appendix",
    "",
    "1=225+; 2=165-224; 3=120-164; 4=85-119; 5=50-84; 6=20-49; 7=0-19 cu ft/acre/yr (lower code = more productive)",
    "",
    "1=large diameter; 2=medium diameter; 3=small diameter; 5=nonstocked",
    "")
)

# Add observed range or number of levels from the final data
data_dictionary$observed <- sapply(data_dictionary$variable, function(v) {
  x <- fia_model[[v]]
  if (is.numeric(x)) sprintf("%.2f to %.2f", min(x), max(x))
  else if (is.factor(x)) paste(nlevels(x), "levels")
  else paste(length(unique(x)), "unique")
})

data_dictionary

