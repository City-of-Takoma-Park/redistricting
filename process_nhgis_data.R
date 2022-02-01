library(tidyverse)
library(plotly)
library(glue)
library(tpfuncts)
library()

### process 3 decades place data
place_2020 <- read.csv("./data/nhgis/redistricting_1990to2020/nhgis0020_csv/nhgis0020_ds248_2020_place.csv")

# recode vars
vars_recode_2020 <- data.frame(
  var = c(
    "U7B001",
    "U7B003",
    "U7B004",
    "U7B005",
    "U7B006",
    "U7B007",
    "U7B008",
    "U7B009",
    "U7C002",
    "U7C003",
    "U7C005",
    "U7G001",
    "U7G002",
    "U7G003"
    ),
  new = c(
    "pop_tot",
    "pop_white",
    "pop_black",
    "pop_aian",
    "pop_asian",
    "pop_nhpi",
    "pop_other",
    "pop_multi",
    "pop_hisp",
    "pop_nh",
    "pop_white_nh",
    "hous_tot",
    "hous_occ",
    "hous_vac"
    )
)

place_2020_process <- place_2020 %>%
  recode_places(vars_recode_2020) %>%
  val_calc_redistrict

# read in 2010
place_2010 <- read.csv("./data/nhgis/redistricting_1990to2020/nhgis0020_csv/nhgis0020_ds172_2010_place.csv")

vars_recode_2010 <- data.frame(
  var = c(
    "H7X001",
    "H7X002",
    "H7X003",
    "H7X004",
    "H7X005",
    "H7X006",
    "H7X007",
    "H7X008",
    "H7Y002",
    "H7Y003",
    "H7Z003",
    "IFE001",
    "IFE002",
    "IFE003"
  ),
  new = c(
    "pop_tot",
    "pop_white",
    "pop_black",
    "pop_aian",
    "pop_asian",
    "pop_nhpi",
    "pop_other",
    "pop_multi",
    "pop_nh",
    "pop_hisp",
    "pop_white_nh",
    "hous_tot",
    "hous_occ",
    "hous_vac"
  )
)

place_2010_process <- place_2010 %>%
  recode_places(vars_recode_2010) %>%
  val_calc_redistrict

# read in 1990 place data
place_2000 <- read.csv("./data/nhgis/redistricting_1990to2020/nhgis0020_csv/nhgis0020_ds146_2000_place.csv") %>%
  rowwise %>%
  mutate(
    pop_tot = sum(
      FMR001,
      FMR002,
      FMR003,
      FMR004,
      FMR005,
      FMR006,
      FMR007
    ),
    pop_nh = sum(
      FMS001,
      FMS002,
      FMS003,
      FMS004,
      FMS005,
      FMS006,
      FMS007
    ),
    pop_hisp = sum(
      FMS008,
      FMS009,
      FMS010,
      FMS011,
      FMS012,
      FMS013,
      FMS014
    ),
    hous_tot = sum(
      FKL001,
      FKL002
    )
  )

vars_recode_2000 <- data.frame(
  var = c(
    "FMR001",
    "FMR002",
    "FMR003",
    "FMR004",
    "FMR005",
    "FMR006",
    "FMR007",
    "FMS001",
    "FKL001",
    "FKL002"
  ),

  new = c(
    "pop_white",
    "pop_black",
    "pop_aian",
    "pop_asian",
    "pop_nhpi",
    "pop_other",
    "pop_multi",
    "pop_white_nh",
    "hous_occ",
    "hous_vac"
  )
)

place_2000_process <- place_2000 %>%
  recode_places(vars_recode_2000) %>%
  val_calc_redistrict

# read in 1990
place_1990 <- read.csv("./data/nhgis/redistricting_1990to2020/nhgis0020_csv/nhgis0020_ds120_1990_place.csv") %>%
  rowwise() %>%
  mutate(
    pop_tot = sum(
      EUY001,
      EUY002,
      EUY003,
      EUY004,
      EUY005
    ),
    pop_hisp = sum(
      EU1002,
      EU1003,
      EU1004,
      EU1005
    ),
    pop_asian = sum(
      EUZ006,
      EUZ007,
      EUZ008,
      EUZ009,
      EUZ010,
      EUZ011,
      EUZ012,
      EUZ013,
      EUZ014,
      EUZ015,
      EUZ016
    ),
    pop_nhpi = sum(
      EUZ017,
      EUZ018,
      EUZ019,
      EUZ020,
      EUZ021,
      EUZ022,
      EUZ023,
      EUZ024,
      EUZ025
    )
  )

vars_recode_1990 <- data.frame(
  var = c(
    "EUY001",
    "EUY002",
    "EUY003",
    "EUY005",
    "EU1001",
    "ET2001",
    "ESA001",
    "ESN001",
    "ESN002"
  ),
  new = c(
    "pop_white",
    "pop_black",
    "pop_aian",
    "pop_other",
    "pop_nh",
    "pop_white_nh",
    "hous_tot",
    "hous_occ",
    "hous_vac"
  )
)

# oddlyt high nhpi in 1990 for tp
tp_base_1990 <- place_1990 %>%
  filter(grepl("Takoma Park", PLACE))

tp_base_race_1990 <- tp_base_1990 %>%
  select(matches("EUZ0"))

place_1990_process <- place_1990 %>%
  recode_places(vars_recode_1990) %>%
  val_calc_redistrict


# places all - two thta not in 1990 = multi
place_all <- bind_rows(
  place_1990_process,
  place_2000_process,
  place_2010_process,
  place_2020_process
) %>%
  group_by(gisjoin) %>%
  arrange(year) %>%
  mutate(
    across(
      .cols = matches("(pop)|(hous)"),
      .fns = list(
        lag = ~ lag(.x),
        chng = ~ .x - lag(.x),
        chng_pct = ~ pct_round(.x - lag(.x), lag(.x))
      ),
      .names = "{.col}_{.fn}"
    )
  )

write_rds(place_all, "./data/output/datafiles/redistricting_places_nation_1990to2020.rds")

# filter maryland
md_places <- place_all %>%
  filter(grepl("Maryland", state))

tp_place <- md_places %>%
  filter(grepl("Takoma Park", place))
# %>%
#   arrange(year) %>%
#   mutate(
#     across(
#       .cols = matches("(pop)|(hous)"),
#       .fns = list(
#         lag = ~ lag(.x),
#         chng = ~ .x - lag(.x),
#         chng_pct = ~ pct_round(.x - lag(.x), lag(.x))
#       ),
#       .names = "{.col}_{.fn}"
#     )
#   )


md_places_2020 <- md_places %>%
  group_by(year) %>%
  mutate(pop_rank = dense_rank(pop_tot))

tp <- md_places_2020 %>%
  filter(grepl("Takoma", place))

race_names <- data.frame(
  var = c(
    "pop_white_nh",
    "pop_black",
    "pop_hisp",
    "pop_asian",
    "pop_nhpi",
    "pop_aian",
    "pop_other",
    "pop_multi"
  ),

  new = c(
    "White (NH)",
    "Black",
    "Hispanic",
    "Asian",
    "NHPI",
    "AIAN",
    "Other",
    "Multi"
  )
)

hous_names <- data.frame(
  var = c(
    "hous_occ",
    "hous_vac"),

  new = c(
    "Occupied housing-units",
    "Vacant units"
  )
)

tp_housing <- pivot_deccalc(tp, hous_names, hous_tot, "housing") %>%
  mutate(year = factor(year))

tp_hous_tot <- plot_ly(tp_housing,
                       x = ~ year,
                      y = ~ value,
                      name = ~ housing,
                      color = ~ housing,
                      legendgroup = ~ housing,
                      type = "scatter",
                      text = ~ glue("Percent: {value_pct}
                      Change from prior decade: {yr_chng}
                      Percent decade change: {yr_chng_pct}
                      City population: {hous_tot}"),
                      mode = "line+marker") %>%
  layout(title = "Takoma Park population by housing",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Housing units")) %>%
  subplot_title("Total")

tp_hous_pct <- plot_ly(tp_housing,
                      x = ~ housing,
                      y = ~ value_pct,
                      name = ~ year,
                      color = ~ year,
                      colors = "Set3",

                      type = "bar",
                      text = ~ glue("Percent: {value}
                      Change from prior decade: {yr_chng}
                      Percent decade change: {yr_chng_pct}
                      City population: {hous_tot}"),
                      mode = "line+marker") %>%
  layout(title = "Takoma Park population by housing",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Housing units")) %>%
  subplot_title(c("Percent"))

tp_hous_pctchng <- plot_ly(tp_housing,
                          x = ~ year,
                          y = ~ yr_chng_pct,
                          name = ~ housing,
                          color = ~ housing,
                          legendgroup = ~ housing,
                          showlegend = F,
                          type = "scatter",
                          text = ~ glue("Total: {value}
                      Change from prior decade: {yr_chng}
                      Percent: {value_pct}
                      City population: {hous_tot}"),
                          mode = "line+marker") %>%
  layout(title = "Takoma Park population by housing",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Housing units")) %>%
  subplot_title("Year percent change")

tp_hous_chng <- plot_ly(tp_housing,
                       x = ~ year,
                       y = ~ yr_chng,
                       name = ~ housing,
                       color = ~ housing,
                       legendgroup = ~ housing,
                       showlegend = F,
                       type = "scatter",
                       text = ~ glue("Total: {value}
                      Percent change from prior decade: {yr_chng_pct}
                      Percent: {value_pct}
                      City population: {hous_tot}"),
                       mode = "line+marker") %>%
  layout(title = "Takoma Park population by housing",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Housing units")) %>%
  subplot_title("Year change")

subplot(tp_hous_chng, tp_hous_pctchng, tp_hous_tot, tp_hous_pct, nrows = 2, shareX = F) %>%
  layout(title = "Takoma Park housing units")



tp_race <- pivot_deccalc(tp, pivot_df = race_names, pop_tot, "race") %>%
  mutate(year = factor(year))


tp_pop_tot <- plot_ly(tp_race  %>%
                        filter(race != "NHPI"),
        x = ~ year,
        y = ~ value,
        name = ~ race,
        color = ~ race,
        legendgroup = ~ race,
        type = "scatter",
        text = ~ glue("Percent: {value_pct}
                      Change from prior decade: {yr_chng}
                      Percent decade change: {yr_chng_pct}
                      City population: {pop_tot}"),
        mode = "line+marker") %>%
  layout(title = "Takoma Park population by race",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Population")) %>%
  subplot_title("Total")

tp_pop_pct <- plot_ly(tp_race %>%
                        mutate(year = factor(year))  %>%
                        filter(race != "NHPI"),
                      x = ~ race,
                      y = ~ value_pct,
                      name = ~ year,
                      color = ~ year,
                      colors = "Set3",

                      type = "bar",
                      text = ~ glue("Percent: {value}
                      Change from prior decade: {yr_chng}
                      Percent decade change: {yr_chng_pct}
                      City population: {pop_tot}"),
                      mode = "line+marker") %>%
  layout(title = "Takoma Park population by race",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Population")) %>%
  subplot_title(c("Percent"))

tp_pop_pctchng <- plot_ly(tp_race %>%
                            mutate(year = factor(year)) %>%
                            filter(race != "NHPI"),
                      x = ~ year,
                      y = ~ yr_chng_pct,
                      name = ~ race,
                      color = ~ race,
                      legendgroup = ~ race,
                      showlegend = F,
                      type = "scatter",
                      text = ~ glue("Total: {value}
                      Change from prior decade: {yr_chng}
                      Percent: {value_pct}
                      City population: {pop_tot}"),
                      mode = "line+marker") %>%
  layout(title = "Takoma Park population by race",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Population")) %>%
  subplot_title("Year percent change")

tp_pop_chng <- plot_ly(tp_race %>%
                            mutate(year = factor(year)) %>%
                            filter(race != "NHPI"),
                          x = ~ year,
                          y = ~ yr_chng,
                          name = ~ race,
                          color = ~ race,
                          legendgroup = ~ race,
                          showlegend = F,
                          type = "scatter",
                          text = ~ glue("Total: {value}
                      Percent change from prior decade: {yr_chng_pct}
                      Percent: {value_pct}
                      City population: {pop_tot}"),
                          mode = "line+marker") %>%
  layout(title = "Takoma Park population by race",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Population")) %>%
  subplot_title("Year change")


subplot(tp_pop_chng, tp_pop_pctchng, tp_pop_tot, tp_pop_pct, nrows = 2, shareX = F)


# process multi 2020
place_2020_multi <- place_2020 %>%
  select(geog_keep, matches("U7B"))

View(place_2020_multi %>%
       filter(grepl("Takoma Park", PLACE)) %>%
       select(U7B009, U7B010, U7B026, U7B047, U7B063, U7B070))

# multi rename
vars_multi <- data.frame(
  var = c(
    "U7B001",
    "U7B009",
    "U7B010",
    "U7B011",
    "U7B012",
    "U7B013",
    "U7B014",
    "U7B015",
    "U7B016",
    "U7B017",
    "U7B018",
    "U7B019",
    "U7B020",
    "U7B021",
    "U7B022",
    "U7B023",
    "U7B024",
    "U7B025",
    "U7B026",
    "U7B047",
    "U7B063",
    "U7B070"
  ),

  "new" = c(
    "pop_tot",
    "pop_multi",
    "pop_multi_2",
    "pop_multi_2_whiteblack",
    "pop_multi_2_whiteaian",
    "pop_multi_2_whiteasian",
    "pop_multi_2_whitenhpi",
    "pop_multi_2_whiteother",
    "pop_multi_2_blackaian",
    "pop_multi_2_blackasian",
    "pop_multi_2_blacknhpi",
    "pop_multi_2_blackother",
    "pop_multi_2_aianasian",
    "pop_multi_2_aiannhpi",
    "pop_multi_2_aianother",
    "pop_multi_2_asiannhpi",
    "pop_multi_2_asianother",
    "pop_multi_2_nhpiother",
    "pop_multi_3",
    "pop_multi_4",
    "pop_multi_5",
    "pop_multi_6"
  ),

  "rename" =
    c("",
      "",
      "Two races",
      "White/Black",
      "White/AIAN",
      "White/Asian",
      "White/NHPI",
      "White/other",
      "Black/AIAN",
      "Black/Asian",
      "Black/NHPI",
      "Black/other",
      "AIAN/Asian",
      "AIAN/NHPI",
      "AIAN/other",
      "Asian/NHPI",
      "Asian/other",
      "NHPI/other",
      "",
      "",
      "",
      "")
)

place_2020_multi_process <- place_2020_multi %>%
  select(geog_keep, vars_multi[["var"]]) %>%
  rename_with(.fn = ~ vars_multi %>%
                filter(.x == var) %>%
                pull(new),
              .cols = vars_multi[["var"]]) %>%
  rename_all(tolower) %>%
  mutate(pop_multi_3plus = pop_multi_3 + pop_multi_4 + pop_multi_5 + pop_multi_6) %>%
  mutate(across(.cols = vars_multi[["new"]],
                .fns = ~ pct_round(.x, pop_tot),
                .names = "{.col}_pcttot")) %>%
  mutate(across(.cols = vars_multi[["new"]][-1],
                .fns = ~ pct_round(.x, pop_multi),
                .names = "{col}_pctmulti"))

tp_2020_multi_process <- place_2020_multi_process %>%
  filter(grepl("Takoma", place))

tp_2020_multi_pivot <- tp_2020_multi_process %>%
  select(tolower(geog_keep), vars_multi[["new"]], pop_multi_3plus) %>%
  pivot_longer(cols = c(vars_multi[["new"]][-c(1:2)], pop_multi_3plus)) %>%
  mutate(value_pcttot = pct_round(value, pop_tot),
         value_pctmulti = pct_round(value, pop_multi))


tp_2020_multi_pivot_stack <- tp_2020_multi_pivot %>%
  left_join(vars_multi %>% select(-var), by = c("name" = "new")) %>%
  mutate(multi_white = grepl("white", name),
         multi_black = grepl("black", name),
         multi_asian = grepl("asian", name),
         multi_other = grepl("other", name),
         rename = ifelse(grepl("3plus", name), "Three or more", rename))

plot_tp_multiracial <- plot_ly(tp_2020_multi_pivot_stack %>%
          filter(name != "pop_multi_2" & rename != "") %>%
          arrange(desc(value)) %>%
          mutate(rename = factor(rename, .$rename, .$rename)),
        x = ~ place,
        y = ~ value,
        color = ~ rename,
        text = ~ glue("{rename}:\n{value_pctmulti}%"),
        textposition = "inside",
        hovertext = ~ glue("Percent all: {value_pcttot}
                           Total: {value}"),
        textfont = list(color = "black"),
        name = ~ rename,
        type = "bar") %>%
  layout(barmode = "stack",
         title = "Takoma Park's multiracial population",
         xaxis = list(title = ""),
         yaxis = list(title = "Population"))

####### recode race/in combination
vars_multi_edit <- data.frame(
  var = c(
    paste0("U7B00", 1:8),
    "U7B009",
    "U7B010",
    "U7B011",
    "U7B012",
    "U7B013",
    "U7B014",
    "U7B015",
    "U7B016",
    "U7B017",
    "U7B018",
    "U7B019",
    "U7B020",
    "U7B021",
    "U7B022",
    "U7B023",
    "U7B024",
    "U7B025",
    "U7B026",
    "U7B027",
    "U7B028",
    "U7B029",
    "U7B030",
    "U7B031",
    "U7B032",
    "U7B033",
    "U7B034",
    "U7B035",
    "U7B036",
    "U7B037",
    "U7B038",
    "U7B039",
    "U7B040",
    "U7B041",
    "U7B042",
    "U7B043",
    'U7B044',
    "U7B045",
    "U7B046",
    "U7B047",
    "U7B048",
    "U7B049",
    "U7B050",
    "U7B051",
    "U7B052",
    "U7B053",
    "U7B054",
    "U7B055",
    "U7B056",
    "U7B057",
    "U7B058",
    "U7B059",
    "U7B060",
    "U7B061",
    "U7B062",
    "U7B063",
    "U7B064",
    "U7B065",
    "U7B066",
    "U7B067",
    "U7B068",
    "U7B069",
    "U7B070",
    "U7B071"
  ),

  "new" = c(
    "pop_total",
    "pop_one",
    "pop_white",
    "pop_black",
    "pop_aian",
    "pop_asian",
    "pop_nhpi",
    "pop_other",
    "pop_multi",
    "pop_multi_2",
    "pop_multi_2_whiteblack",
    "pop_multi_2_whiteaian",
    "pop_multi_2_whiteasian",
    "pop_multi_2_whitenhpi",
    "pop_multi_2_whiteother",
    "pop_multi_2_blackaian",
    "pop_multi_2_blackasian",
    "pop_multi_2_blacknhpi",
    "pop_multi_2_blackother",
    "pop_multi_2_aianasian",
    "pop_multi_2_aiannhpi",
    "pop_multi_2_aianother",
    "pop_multi_2_asiannhpi",
    "pop_multi_2_asianother",
    "pop_multi_2_nhpiother",
    "pop_multi_3",
    "pop_multi_3_whiteblackaian",
    "pop_multi_3_whiteblackasian",
    "pop_multi_3_whiteblacknhpi",
    "pop_multi_3_whiteblackother",
    "pop_multi_3_whiteaianasian",
    "pop_multi_3_whiteaiannhpi",
    "pop_multi_3_whiteaianother",
    "pop_multi_3_whiteasiannhpi",
    "pop_multi_3_whiteasianother",
    "pop_multi_3_whitenhpiother",
    "pop_multi_3_blackaianasian",
    "pop_multi_3_blackaiannhpi",
    "pop_multi_3_blackaianother",
    "pop_multi_3_blackasiannhpi",
    "pop_multi_3_blackasianother",
    "pop_multi_3_blacknhpiother",
    "pop_multi_3_aianasiannhpi",
    "pop_multi_3_aianasianother",
    "pop_multi_3_aiannhpiother",
    "pop_multi_3_asiannhpiother",
    "pop_multi_4",
    "pop_multi_4_whiteblackaianasian",
    "pop_multi_4_whiteblackaiannhpi",
    "pop_multi_4_whiteblackaianother",
    "pop_multi_4_whiteblackasiannhpi",
    "pop_multi_4_whiteblackasianother",
    "pop_multi_4_whiteblacknhpiother",
    "pop_multi_4_whiteaianasiannhpi",
    "pop_multi_4_whiteaianasianother",
    "pop_multi_4_whiteaiannhpiother",
    "pop_multi_4_whiteasiannhpiother",
    "pop_multi_4_blackaianasiannhpi",
    "pop_multi_4_blackaianasianother",
    "pop_multi_4_blackaiannhpiother",
    "pop_multi_4_blackasiannhpiother",
    "pop_multi_4_aianasiannhpiother",
    "pop_multi_5",
    "pop_multi_5_whiteblackaianasiannhpi",
    "pop_multi_5_whiteblackaianasianother",
    "pop_multi_5_whiteblackaiannhpiother",
    "pop_multi_5_whiteblackasiannhpiother",
    "pop_multi_5_whiteaianasiannhpiother",
    "pop_multi_5_blackaianasiannhpiother",
    "pop_multi_6",
    "pop_multi_6_whiteblackaianasiannhpiother"
  ),

  "rename" =
    c("Total: population",
      "Total: one race",
      "White",
      "Black",
      "AIAN",
      "Asian",
      "NHPI",
      "Other",
      "Total: multiple races",
      "Two races",
      "White/Black",
      "White/AIAN",
      "White/Asian",
      "White/NHPI",
      "White/other",
      "Black/AIAN",
      "Black/Asian",
      "Black/NHPI",
      "Black/other",
      "AIAN/Asian",
      "AIAN/NHPI",
      "AIAN/other",
      "Asian/NHPI",
      "Asian/other",
      "NHPI/other",
      "Three races",
      "White/Black/AIAN",
      "White/Black/Asian",
      "White/Black/NHPI",
      "White/Black/other",
      "White/AIAN/Asian",
      "White/AIAN/NHPI",
      "White/AIAN/other",
      "White/Asian/NHPI",
      "White/Asian/other",
      "White/NHPI/other",
      "Black/AIAN/Asian",
      "Black/AIAN/NHPI",
      "Black/AIAN/other",
      "Black/Asian/NHPI",
      "Black/Asian/other",
      "Black/NHPI/other",
      "AIAN/Asian/NHPI",
      "AIAN/Asian/other",
      "AIAN/NHPI/other",
      "Asian/NHPI/other",
      "Four races",
      "White/Black/AIAN/Asian",
      "White/Black/AIAN/NHPI",
      "White/Black/AIAN/other",
      "White/Black/Asian/NHPI",
      "White/Black/Asian/other",
      "White/Black/NHPI/other",
      "White/AIAN/Asian/NHPI",
      "White/AIAN/Asian/other",
      "White/AIAN/NHPI/other",
      "White/Asian/NHPI/other",
      "Black/AIAN/Asian/NHPI",
      "Black/AIAN/Asian/other",
      "Black/AIAN/NHPI/other",
      "Black/Asian/NHPI/other",
      "AIAN/Asian/NHPI/other",
      "Five races",
      "White/Black/AIAN/Asian/NHPI",
      "White/Black/AIAN/Asian/other",
      "White/Black/AIAN/NHPI/other",
      "White/Black/Asian/NHPI/other",
      "White/AIAN/Asian/NHPI/other",
      "Black/AIAN/Asian/NHPI/other",
      "Six races",
      "White/Black/AIAN/Asian/NHPI/other")
)


vars_hisp_lat_edit <- data.frame(
  var = c(
    c(
      paste0("U7C00", 1:9),
      paste0("U7C0", 10:73)
    )
  ),

  new = c(
    "pop_tot",
    "pop_hisp",
    "pop_nh",
    "pop_nh_one",
    "pop_nh_white",
    "pop_nh_black",
    "pop_nh_aian",
    "pop_nh_asian",
    "pop_nh_nhpi",
    "pop_nh_other",
    "pop_nh_multi",
    "pop_nh_multi_2",
    "pop_nh_multi_2_whiteblack",
    "pop_nh_multi_2_whiteaian",
    "pop_nh_multi_2_whiteasian",
    "pop_nh_multi_2_whitenhpi",
    "pop_nh_multi_2_whiteother",
    "pop_nh_multi_2_blackaian",
    "pop_nh_multi_2_blackasian",
    "pop_nh_multi_2_blacknhpi",
    "pop_nh_multi_2_blackother",
    "pop_nh_multi_2_aianasian",
    "pop_nh_multi_2_aiannhpi",
    "pop_nh_multi_2_aianother",
    "pop_nh_multi_2_asiannhpi",
    "pop_nh_multi_2_asianother",
    "pop_nh_multi_2_nhpiother",
    "pop_nh_multi_3",
    "pop_nh_multi_3_whiteblackaian",
    "pop_nh_multi_3_whiteblackasian",
    "pop_nh_multi_3_whiteblacknhpi",
    "pop_nh_multi_3_whiteblackother",
    "pop_nh_multi_3_whiteaianasian",
    "pop_nh_multi_3_whiteaiannhpi",
    "pop_nh_multi_3_whiteaianother",
    "pop_nh_multi_3_whiteasiannhpi",
    "pop_nh_multi_3_whiteasianother",
    "pop_nh_multi_3_whitenhpiother",
    "pop_nh_multi_3_blackaianasian",
    "pop_nh_multi_3_blackaiannhpi",
    "pop_nh_multi_3_blackaianother",
    "pop_nh_multi_3_blackasiannhpi",
    "pop_nh_multi_3_blackasianother",
    "pop_nh_multi_3_blacknhpiother",
    "pop_nh_multi_3_aianasiannhpi",
    "pop_nh_multi_3_aianasianother",
    "pop_nh_multi_3_aiannhpiother",
    "pop_nh_multi_3_asiannhpiother",
    "pop_nh_multi_4",
    "pop_nh_multi_4_whiteblackaianasian",
    "pop_nh_multi_4_whiteblackaiannhpi",
    "pop_nh_multi_4_whiteblackaianother",
    "pop_nh_multi_4_whiteblackasiannhpi",
    "pop_nh_multi_4_whiteblackasianother",
    "pop_nh_multi_4_whiteblacknhpiother",
    "pop_nh_multi_4_whiteaianasiannhpi",
    "pop_nh_multi_4_whiteaianasianother",
    "pop_nh_multi_4_whiteaiannhpiother",
    "pop_nh_multi_4_whiteasiannhpiother",
    "pop_nh_multi_4_blackaianasiannhpi",
    "pop_nh_multi_4_blackaianasianother",
    "pop_nh_multi_4_blackaiannhpiother",
    "pop_nh_multi_4_blackasiannhpiother",
    "pop_nh_multi_4_aianasiannhpiother",
    "pop_nh_multi_5",
    "pop_nh_multi_5_whiteblackaianasiannhpi",
    "pop_nh_multi_5_whiteblackaianasianother",
    "pop_nh_multi_5_whiteblackaiannhpiother",
    "pop_nh_multi_5_whiteblackasiannhpiother",
    "pop_nh_multi_5_whiteaianasiannhpiother",
    "pop_nh_multi_5_blackaianasiannhpiother",
    "pop_nh_multi_6",
    "pop_nh_multi_6_whiteblackaianasiannhpiother"
  ),

  rename = c(
    "Total: population",
    "Total: Hispanic/Latino",
    "Total: non-Hispanic/Latino",
    "Total NH: one race",
    "White NH",
    "Black NH",
    "AIAN NH",
    "Asian NH",
    "NHPI NH",
    "Other NH",
    "Total: multiracial",
    "Two races NH",
    "White/Black NH",
    "White/AIAN NH",
    "White/Asian NH",
    "White/NHPI NH",
    "White/other NH",
    "Black/AIAN NH",
    "Black/Asian NH",
    "Black/NHPI NH",
    "Black/other NH",
    "AIAN/Asian NH",
    "AIAN/NHPI NH",
    "AIAN/other NH",
    "Asian/NHPI NH",
    "Asian/other NH",
    "NHPI/other NH",
    "Three races NH",
    "White/Black/AIAN NH",
    "White/Black/Asian NH",
    "White/Black/NHPI NH",
    "White/Black/other NH",
    "White/AIAN/Asian NH",
    "White/AIAN/NHPI NH",
    "White/AIAN/other NH",
    "White/Asian/NHPI NH",
    "White/Asian/other NH",
    "White/NHPI/other NH",
    "Black/AIAN/Asian NH",
    "Black/AIAN/NHPI NH",
    "Black/AIAN/other NH",
    "Black/Asian/NHPI NH",
    "Black/Asian/other NH",
    "Black/NHPI/other NH",
    "AIAN/Asian/NHPI NH",
    "AIAN/Asian/other NH",
    "AIAN/NHPI/other NH",
    "Asian/NHPI/other NH",
    "Four races NH",
    "White/Black/AIAN/Asian NH",
    "White/Black/AIAN/NHPI NH",
    "White/Black/AIAN/other NH",
    "White/Black/Asian/NHPI NH",
    "White/Black/Asian/other NH",
    "White/Black/NHPI/other NH",
    "White/AIAN/Asian/NHPI NH",
    "White/AIAN/Asian/other NH",
    "White/AIAN/NHPI/other NH",
    "White/Asian/NHPI/other NH",
    "Black/AIAN/Asian/NHPI NH",
    "Black/AIAN/Asian/other NH",
    "Black/AIAN/NHPI/other NH",
    "Black/Asian/NHPI/other NH",
    "AIAN/Asian/NHPI/other NH",
    "Five races NH",
    "White/Black/AIAN/Asian/NHPI NH",
    "White/Black/AIAN/Asian/other NH",
    "White/Black/AIAN/NHPI/other NH",
    "White/Black/Asian/NHPI/other NH",
    "White/AIAN/Asian/NHPI/other NH",
    "Black/AIAN/Asian/NHPI/other NH",
    "Six races NH",
    "White/Black/AIAN/Asian/NHPI/other NH"
  )
)

# hisp_calc <- function(df, vector){
#   hisp_cols <- colnames(df)
#
#   df %>%
#     mutate(
#       across(
#         .cols = vector,
#         .fns = ~ ,
#
#         .names = "{.col}_hisp"))
# }

place_2020_hispmulti <- place_2020 %>%
  select(geog_keep, contains("U7B0"), contains("U7C0"))

# rename variables
place_2020_hispmulti_process <- recode_places(
  place_2020_hispmulti,
  rbind(vars_hisp_lat_edit,
        vars_multi_edit))

calc_hisp_race <- function(df){
  nh_vars <- vars_multi_edit$new[-1]

  walk(nh_vars, ~
         {
           nh_var <- gsub("pop_", "pop_nh_", .x)
           new_var <- gsub("pop_", "pop_hisp_", .x)

           df <<- df %>%
             mutate(!!sym(new_var) := !!sym(.x) - !!sym(nh_var))
         }
  )
  df
}

place_2020_hispmulti_hisp <- calc_hisp_race(place_2020_hispmulti_process)


calc_root <- function(df, col_root, name_replace = "pop_", hisp_mod = NULL){

  col_val <- col_root

  if (!is.null(hisp_mod)){
    col_val <- gsub(
      "pop_",
      paste0(
        "pop_",
        hisp_mod,
        "_"
        ),
      col_root)
  }

  name_replace <- gsub(name_replace, "", col_root)

  # browser()

  df %>%
    mutate(
      across(
        matches(col_val),
        ~ pct_round(.x, !!sym(col_root)),
        .names = "{.col}_pct_{name_replace}"
      )
    )
}

race_vec <- c(
  "white",
  "black",
  "asian",
  "aian",
  "nhpi",
  "other"
)

# calculate o
calc_race_hisp_pct <- function(df){
  race_vec <- c(
    "white",
    "black",
    "asian",
    "aian",
    "nhpi",
    "other"
  )

  walk(
    race_vec,
    ~ {
      df <<- df %>%
        calc_root(
          paste0("pop_", .x), hisp_mod = "hisp"
        ) %>%
        calc_root(
          paste0("pop_", .x), hisp_mod = "nh"
        )
    })

  df

}

vars_multi_edit$new

combination_calc <- function(df, race_val, hisp_mod = NULL){

  if (!is.null(hisp_mod) & !hisp_mod %in% c("hisp", "nh")){
    stop("Error; invalue hisp_mod value. Must be hisp or nh")
  }

  pop <- case_when(is.null(hisp_mod) ~ "pop_",
                   T ~ paste0("pop_", hisp_mod, "_"))

  new_name <- paste0(pop, race_val, "_combo")
  old <- paste0(pop, race_val)
  total <- paste0(pop, race_val, "_total")

  if (is.null(hisp_mod)){
    df %>%
      mutate(
        !!sym(new_name) := sum(
          ifelse(
            grepl(race_val,
                  var) &
              multi,
            val,
            0
          )
        ))
  }

  else {
    # browser()

    df %>%
      mutate(
        !!sym(new_name) := sum(
          ifelse(
            grepl(race_val,
                  var) &
            grepl(hisp_mod,
                  hisp) &
              multi,
            val,
            0
          )
        ))
  }
  # sum across multi-race categories
}

calc_multicombos <- function(df, race_val, hisp_mod){


  if (!is.null(hisp_mod) & !hisp_mod %in% c("hisp", "nh")){
    stop("Error; invalue hisp_mod value. Must be hisp or nh")
  }

  pop <- case_when(is.null(hisp_mod) ~ "pop_",
                   T ~ paste0("pop_", hisp_mod, "_"))

  # browser()
  new_name <- paste0(pop, race_val, "_combo")
  old <- paste0(pop, race_val)
  total <- paste0(pop, race_val, "_total")

  df <- df %>%
    mutate(!!sym(total) := !!sym(old) + !!sym(new_name),
           !!sym(paste0(new_name, "_pct")) := pct_round(!!sym(new_name), pop_total),
           !!sym(paste0(old, "_pct")) := pct_round(!!sym(old), pop_total),
           !!sym(paste0(total, "_pct")) := pct_round(!!sym(total), pop_total))

  if (!is.null(hisp_mod)){
    pop_col <- paste0("pop_", hisp_mod)

    df <- df %>%
      mutate(!!sym(paste0(new_name, "_pct_hisp")) := pct_round(!!sym(new_name), !!sym(pop_col)),
             !!sym(paste0(old, "_pct_hisp")) := pct_round(!!sym(old), !!sym(pop_col)),
             !!sym(paste0(total, "_pct_hisp")) := pct_round(!!sym(total), !!sym(pop_col)))
  }

}

# multiracial alone or in combo
multiracialcalc <- function(df, hispmod = F){

  race_vec <- c(
    "white",
    "black",
    "asian",
    "aian",
    "nhpi",
    "other"
  )

  # browser()

  if (hispmod) {
    vars_df <- c(vars_hisp_lat_edit[["new"]],
                                   gsub("_nh_", "_hisp_", vars_hisp_lat_edit[["new"]])) %>%
                         unique()
  }

  else{
    vars_df <- vars_multi_edit[["new"]]
  }

  df <- df %>%
    pivot_longer(
      cols = vars_df[-1],
      names_to = "var",
      values_to = "val"
    ) %>%
    mutate(multi = grepl("multi", var))

  if (hispmod){
    df <- df %>%
      mutate(hisp = case_when(grepl("hisp", var) ~ "hisp",
                              grepl("nh", var) ~ "nh"))
  }

  df <- df %>%
    group_by(GISJOIN)

  # calculate races alone and in combination
  walk(race_vec, function(race){

    if (hispmod){
      walk(c("hisp", "nh"), ~ {
        df <<- df %>%
          combination_calc(race_val = race,
                           hisp_mod = .x)
      })

      df <<- df
    }

    else{
      df <<- df %>%
        combination_calc(race_val = race)
    }

    # browser()
  })

  # browser()

  deselect_cols <- c("multi")

  if (hispmod){
    deselect_cols <- c(deselect_cols, "hisp")
  }

  # pivot back
  df <- df %>%
    ungroup() %>%
    select(-deselect_cols) %>%
    pivot_wider(names_from = var,
                values_from = val)

  walk(race_vec, function(race){

    if (hispmod){
      walk(c("hisp", "nh"), ~ {
        df <<- df %>%
          calc_multicombos(race_val = race,
                           hisp_mod = .x)
      })

      df <<- df
    }

    else{
      df <<- df %>%
        calc_multicombos(race_val = race)
    }

  })

  # browser()
  df <- df %>%
    mutate(across(
      .cols = matches("_combo$"),
      .fns = ~ pct_round(.x, pop_multi),
      .names = "{.col}_pct_multi"
    )) %>%
    mutate(across(
      .cols = paste0("pop_", race_vec),
      .fns = ~ pct_round(.x, pop_one),
      .names = "{.col}_pct_multi"
    ))

  if (hispmod){
    df <- df %>%
      mutate(across(
        .cols = matches("_combo$") & matches("_nh_"),
        .fns = ~ pct_round(.x, pop_nh_multi),
        .names = "{.col}_pct_hisp_multi"
      )) %>%
      mutate(across(
        .cols = matches("_combo$") & matches("_hisp_"),
        .fns = ~ pct_round(.x, pop_hisp_multi),
        .names = "{.col}_pct_hisp_multi"
      )) %>%
      mutate(across(
        .cols = paste0("pop_nh_", race_vec),
        .fns = ~ pct_round(.x, pop_nh_one),
        .names = "{.col}_pct_hisp_multi"
      )) %>%
      mutate(across(
        .cols = paste0("pop_hisp_", race_vec),
        .fns = ~ pct_round(.x, pop_hisp_one),
        .names = "{.col}_pct_hisp_multi"
      ))
  }

  df
}

place_2020_hispmulti_pctcalc <- calc_race_hisp_pct(place_2020_hispmulti_hisp)

place_2020_hispmulti_multicalc <- place_2020_hispmulti_pctcalc %>%
  multiracialcalc()

place_2020_hispmulti_hispcalc <- place_2020_hispmulti_pctcalc %>%
  multiracialcalc(hispmod = T)

place_2020_test <- place_2020_hispmulti_multicalc %>%
  filter(grepl("Takoma", PLACE)) %>%
  select(geog_keep, contains("pop_white"))

tp_race_full <- place_2020_hispmulti_multicalc %>%
  filter(grepl("Takoma", PLACE))

tp_race_hisp_full <-place_2020_hispmulti_hispcalc %>%
  filter(grepl("Takoma", PLACE))

# function to pivot set of columns and join back to base dataset
pivot_join <- function(df,
                       cols_vec,
                       names = "name",
                       values = "value",
                       hisp = NULL) {

  # browser()

  race_factor <- c(
    "White",
    "Black",
    "Asian",
    "Other",
    "AIAN",
    "NHPI"
  )

  race_collapse <- paste0(paste0("(", race_vec, collapse = ")|"), ")")

  pivot_df <- df %>%
    select(geog_keep, cols_vec) %>%
    distinct %>%
    pivot_longer(cols = cols_vec,
                 names_to = names,
                 values_to = values)

    pivot_df <- pivot_df %>%
      mutate(multi = case_when(grepl("combo", !!sym(names)) ~ "In combination",
                               T ~ "Alone"),
             race = str_extract(!!sym(names),
                                pattern = race_collapse) %>%
               str_to_title() %>%
               ifelse(. %in% c("Aian", "Nhpi"), toupper(.), .),
             race = factor(race, race_factor, race_factor))

    if (hisp){
      pivot_df <- pivot_df %>%
        mutate(hisp = case_when(grepl("nh", !!sym(names)) ~ "Non-Hispanic",
                                grepl("hisp", !!sym(names)) ~ "Hispanic"))
    }

  df %>%
    left_join(pivot_df)
}


pivot_multigraph <- function(df, hisp = F){

  # browser()

  cols_df <- colnames(df) %>%
    grep("(pop_)|(_combo)", x = ., value = T)

  if (!hisp){
    cols_df <- cols_df %>%
      grep("(_multi)|(hisp)|(nh_)|(tot)|(_one)", x = ., value = T, invert = T)
  }

  else if (hisp){
    cols_df <- cols_df %>%
      grep("(_multi)|(tot)|(_one)", x = ., value = T, invert = T) %>%
      grep("(hisp_)|(nh_)", x = ., value = T)
  }

  cols_df_pct <- cols_df %>%
    grep("_pct", x = ., value = T)

  cols_df_tot <- cols_df %>%
    grep("_pct", x = ., value = T, invert = T)

  if (!hisp){
    cols_df_multipct <- colnames(df) %>%
      grep("pct_multi", x = ., value = T)
  }

  if (hisp){
    cols_df_multipct <- colnames(df) %>%
      grep("pct_hisp_multi", x = ., value = T)
  }

  df %>%
    pivot_join(cols_df_pct,
               names = "name_pct",
               values = "val_pct",
               hisp = hisp) %>%
    pivot_join(cols_df_tot,
               "name_tot",
               "val_tot",
               hisp = hisp) %>%
    pivot_join(cols_df_multipct,
               "name_multipct",
               "val_multipct",
               hisp = hisp)

}

tp_race_combos <- tp_race_full %>%
  pivot_multigraph()

tp_hisp_combos <- tp_race_hisp_full %>%
  pivot_multigraph(hisp = T)

tp_hisp_combos_pctall <- tp_hisp_combos %>%
  filter(grepl("pct$", name_pct))

plot_race_multi <- plot_ly(tp_race_combos,
                                x = ~ multi,
                                y = ~ val_multipct,
                                name = ~ race,
                                color = ~ race,
                                text = ~ glue("{race}:\n{val_multipct}%"),
                                textposition = "inside",
                           legendgroup = ~ race,
                                textfont = list(color = "black"),
                                hovertext = ~ glue("Total: {val_tot}
                           Percent population: {val_pct}%"),
                                type = "bar") %>%
  layout(title = "Takoma Park population by race alone and in-combination",
         yaxis = list(title = "Percent one or multi-race"),
         xaxis = list(title = "Population"))


plot_race_multi_hisp <- plot_ly(tp_hisp_combos_pctall %>%
                                  filter(grepl("hisp", name_pct)),
                                x = ~ multi,
                                y = ~ val_pct,
                                name = ~ race,
                                legendgroup = ~ race,
                                showlegend = F,
                                color = ~ race,
                                text = ~ glue("{race}:\n{val_pct}%"),
                                textposition = "inside",
                                textfont = list(color = "black"),
                                hovertext = ~ glue("Total: {val_tot}
                                                   Percent population: {val_pct}%"),
                                type = "bar") %>%
  layout(title = "Takoma Park population by race alone and in-combination",
         yaxis = list(title = "Percent one or multi-race"),
         barmode = "stack",

         xaxis = list(title = "Population")) %>%
  subplot_title("Hispanic")

plot_race_multi_nh <- plot_ly(tp_hisp_combos_pctall %>%
                                  filter(grepl("nh", name_pct)),
                                x = ~ multi,
                                y = ~ val_pct,
                              legendgroup = ~ race,

                                name = ~ race,
                                color = ~ race,
                                text = ~ glue("{race}:\n{val_pct}%"),
                                textposition = "inside",
                                textfont = list(color = "black"),
                                hovertext = ~ glue("Total: {val_tot}
                                                   Percent population: {val_pct}%"),
                                type = "bar") %>%
  layout(title = "Takoma Park population by race alone and in-combination",
         yaxis = list(title = "Percent one or multi-race"),
         barmode = "stack",
         xaxis = list(title = "Population")) %>%
  subplot_title("Nonhispanic")

subplot(plot_race_multi_hisp, plot_race_multi_nh, shareY = T)


plot_race_hisp_multipct <- plot_ly(tp_hisp_combos_pctall %>%
                                  filter(grepl("hisp", name_pct)),
                                x = ~ multi,
                                y = ~ val_tot,
                                name = ~ race,
                                legendgroup = ~ race,
                                showlegend = F,
                                color = ~ race,
                                text = ~ glue("{race}:\n{val_multipct}%"),
                                textposition = "inside",
                                textfont = list(color = "black"),
                                hovertext = ~ glue("Total: {val_tot}
                                                   Percent population: {val_pct}%"),
                                type = "bar") %>%
  layout(title = "Takoma Park population by race alone and in-combination",
         yaxis = list(title = "Percent one or multi-race"),
         xaxis = list(title = "Population")) %>%
  subplot_title("Hispanic")

plot_race_multi_nh_multipct <- plot_ly(tp_hisp_combos_pctall %>%
                                filter(grepl("nh", name_pct)),
                              x = ~ multi,
                              y = ~ val_tot,
                              legendgroup = ~ race,
                              name = ~ race,
                              color = ~ race,
                              text = ~ glue("{race}:\n{val_multipct}%"),
                              textposition = "inside",
                              textfont = list(color = "black"),
                              hovertext = ~ glue("Total: {val_tot}
                                                 Percent population: {val_pct}%"),
                              type = "bar") %>%
  layout(title = "Takoma Park population by race alone and in-combination",
         yaxis = list(title = "Percent one or multi-race"),
         xaxis = list(title = "Population")) %>%
  subplot_title("Nonhispanic")

subplot(plot_race_hisp_multipct, plot_race_multi_nh_multipct, nrows = 2, shareX = T)

# hispanic demographics by race
tp_hisp_combos <- tp_race_full %>%
  pivot_multigraph(hisp = T)

calc_hisp_onevmulti <- function(df){

  cols_hisp_onemulti <- colnames(df) %>%
    grep("(pop_nh_)|(pop_hisp_)", x = ., value = T) %>%
    grep("(multi_[2-9])|(_combo)|(tot)", x = ., invert = T, value = T)

  cols_hisp_disag_v_multi <- cols_hisp_onemulti %>%
    grep("(_one)|(_pct)", x = ., invert = T, value = T)

  cols_hisp_disag_pct <- cols_hisp_onemulti %>%
    grep("_one", x = ., invert = T, value = T) %>%
    grep("_pct", x = ., value = T)

  df %>%
    pivot_join(cols_vec = cols_hisp_disag_v_multi,
               names = "name_tot",
               values = "val_tot",
               hisp = T) %>%
    pivot_join(cols_vec = cols_hisp_disag_pct,
               names = "name_pct",
               values = "val_pct",
               hisp = T)


}

# plot hispanic one race vs. multiracial
tp_hisp_multione <- tp_race_full %>%
  calc_hisp_onevmulti()

tp_hisp_multione

###### process 2010 data




