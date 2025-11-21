library(tidyverse)
library(janitor)
library(foreign)

res_22 <- read_csv('data/state_g22_sov_data_by_g22_srprec.csv')
cw_22 <- read_csv('crosswalk/state_g22_sr_blk_map.csv')
cw_24 <- read_csv('crosswalk/state_g24_sr_blk_map.csv')
demo <- read_csv('demographic_data/state_CVAP_with_DOJ_2019-2023_by_2020_Blocks.csv')

res_22 <- res_22 %>% select('SRPREC_KEY', starts_with('PR_'))

props <- c('PR_1', 'PR_26', 'PR_27', 'PR_28', 'PR_29', 'PR_30', 'PR_31')

for (pr in props) {
  res_22[[paste0(pr, "_TOT")]] <- res_22[[paste0(pr, "_Y")]] + res_22[[paste0(pr, "_N")]]
}

cons <- cw_22 %>% inner_join(res_22, join_by(SRPREC_KEY))

suffixes <- c("_Y", "_N", "_TOT")

for (pr in props) {
  for (suf in suffixes) {
    col <- paste0(pr, suf)
    cons[[col]] <- cons[[col]] * (cons[["PCTSRPREC"]] / 100)
  }
}

demo <- demo %>% rename(BLOCK_KEY = BLOCK20)

cols <- c(
  "PR_1_N", "PR_1_Y", "PR_26_N", "PR_26_Y",
  "PR_27_N", "PR_27_Y", "PR_28_N", "PR_28_Y",
  "PR_29_N", "PR_29_Y", "PR_30_N", "PR_30_Y",
  "PR_31_N", "PR_31_Y", "PR_1_TOT", "PR_26_TOT",
  "PR_27_TOT", "PR_28_TOT", "PR_29_TOT", "PR_30_TOT", "PR_31_TOT"
)

cons <- cons %>%
  group_by(BLOCK_KEY) %>%
  summarise(across(all_of(cols), sum), .groups = "drop")

cons <- cons %>% right_join(cw_24, join_by(BLOCK_KEY))

cols <- c(cols, colnames(demo)[-(1)])

cons <- cons %>% left_join(demo, join_by(BLOCK_KEY))

for (pr in props) {
  for (suf in suffixes) {
    col <- paste0(pr, suf)
    cons <- cons %>% mutate(!!col := replace_na(.data[[col]], 0))
    cons[[col]] <- cons[[col]] * (cons[["PCTBLK"]] / 100)
  }
}

cons <- cons %>%
  group_by(SRPREC_KEY) %>%
  summarize(across(all_of(cols), sum), .groups = "drop")

res_24 <- tibble(read.dbf("shapes/ca_24_allraces.dbf"))

# Will not contain Madera county precincts (FIPS = 06039)
res <- cons %>% inner_join(res_24, join_by('SRPREC_KEY'))

madera_res_24 <- read_csv('data/c039_g24_sov_data_by_g24_srprec.csv')

props <- c("PR_2", "PR_3", "PR_4", "PR_5", "PR_6", "PR_32", "PR_33", "PR_34", "PR_35",
           "PR_36")
new_suffixes <- suffixes <- c("_Y", "_N")

for (pr in props) {
  for (suf in suffixes) {
    col <- paste0(pr, suf)
    new_col <- paste0("G24", gsub("_", "", pr), suf)
    madera_res_24 <- madera_res_24 %>% rename(!!new_col := !!col)
  }
}

pres_cands <- c("Dem", "Rep")

for (cand in pres_cands) {
  col <- paste0("PRS", toupper(cand), "01")
  new_col <- paste0("G24P", cand)
  madera_res_24 <- madera_res_24 %>% rename(!!new_col := !!col)
}

madera_res_24 <- madera_res_24 %>% mutate(
  SRPREC_KEY = paste0("06039", srprec),
  G24POth = PRSPAF01 + PRSGRN01 + PRSAIP01 + PRSLIB01
) %>% mutate(
  G24PTot = G24PDem + G24PRep + G24POth
)

madera_res <- madera_res_24 %>% inner_join(cons, join_by('SRPREC_KEY'))

res <- bind_rows(res, madera_res)

write_csv(res, "transformed_data/all_results.csv")