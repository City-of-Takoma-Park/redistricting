library(leaflet)
library(leafletwrappers)
library(tidyverse)
library(sf)
library(tpfuncts)
library(RColorBrewer)

census_change_all <- st_read("./data/TP_Census_Blocks_2020_w_Options.gdb", layer = "Blocks_2010_2020_Diff") %>%
  rename_all(tolower) %>%
  st_transform(4326) %>%
  mutate(diff_pop_pct = pct_round(diff_pop_total, f2010_pop_total))

tp_blocks <- st_read("./data/TP_Census_Blocks_2020_w_Options.gdb", layer = "Takoma_Park_Census_Blocks_w_Revised_Ward_Options_AandB") %>%
  rename_all(tolower) %>%
  st_transform(4326)

tp_block_list <- tp_blocks$geoid20

st_layers("./data/TP_Census_Blocks_2020_w_Options.gdb")

census_change <- census_change_all %>%
  filter(geoid %in% tp_block_list)

df_census_change <- census_change %>%
  st_drop_geometry()

quantile_palette <- leaflet::colorBin(domain = df_census_change$f2020_pop_total, palette = "Purples", bins = c(0, 25, 50, 75, 100, 150, 250, 1200))

quantile(df_census_change$diff_pop_total, probs = seq(0, 1, 0.1))

color_pop_diff <- colorBin(
  domain = df_census_change$diff_pop_total,
  bins = c(-113, -25, -10, -5, 0, 5, 10, 15, 20, 25, 331),
  palette =
    c(rev(RColorBrewer::brewer.pal(4, "Oranges")),
      RColorBrewer::brewer.pal(6, "Purples")
    )
)

quantile(df_census_change$diff_pop_pct,na.rm = T, probs = seq(0, 1, 0.1))

color_pop_diff_pct <- colorBin(
  domain = df_census_change$diff_pop_pct,
  bins = c(-100, -25, -10, -5, 0, 5, 10, 20, 30, 50, 100, 200),
  palette =
    c(rev(RColorBrewer::brewer.pal(4, "Oranges")),
      RColorBrewer::brewer.pal(8, "Purples")
    )
)

color_pop <- pal_numeric(var = "diff_pop_total",colors = "PuOr", df_census_change)

color_pop_norm <- pal_numeric(var = "f2010_pop_total",colors = "Purples", df_census_change)


label_funct <- function(df){
  label_output(df, label_text =
                 "Block name: {name}<p></p>
  Population 2010: {f2010_pop_total}<p></p>
  Population 2020: {f2020_pop_total}<p></p>
  Population difference: {diff_pop_total}<p></p>
  Percent population different: {diff_pop_pct}%")
}

labels_census <- label_funct(df_census_change)

layer_pop <- function(basemap, title = "Population change 2010-2020", varval = "diff_pop_total", palval = color_pop_diff){
  basemap %>%
    addpoly_legend(
      df_select = df_census_change,
      pal_funct_select = palval,
      variable_select = varval,
      group_select = title,
      title_select = title,
      labels_select = labels_census,
      .polfillopacity = 0.55,
    )
}



map_pop_change <- leaflet(census_change) %>%
  addProviderTiles("Esri") %>%
  layer_pop("Population 2020", varval = "f2020_pop_total", palval = quantile_palette) %>%
  layer_pop("Population 2010", varval = "f2010_pop_total", palval = quantile_palette) %>%
  layer_pop() %>%
  layer_pop(title = "Percent population change 2010-2020",varval = "diff_pop_pct", palval = color_pop_diff_pct) %>%
  addLayersControl(overlayGroups = c("Population 2020", "Population 2010", "Population change 2010-2020", "Percent population change 2010-2020"), position = "topleft", options = layersControlOptions(collapsed = F)) %>%
  hideGroup(c("Population 2010", "Population change 2010-2020", "Percent population change 2010-2020")) %>%
  add_wards()

htmlwidgets::saveWidget(widget = map_pop_change, "./data/output/maps/pop-change-2010-2020.html", selfcontained = T)
