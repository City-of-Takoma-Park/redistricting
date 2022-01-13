library(leaflet)
library(leafletwrappers)
library(tidyverse)
library(sf)

wards_new <- st_read("./data/new_wards/TP_Census_Blocks_2020_w_Options.gdb") %>%
  rename_all(tolower) %>%
  st_transform(4326)

new_ward_funct <- function(df, opt_num){
  ward <- paste0("option_", opt_num)

  ward_chng <- paste0(ward, "_wardchng")

  df %>%
    mutate(!!sym(ward_chng) := !!sym(ward) == current_district)

}

wards_new <- wards_new %>%
  new_ward_funct(1) %>%
  new_ward_funct(2) %>%
  new_ward_funct(3) %>%
  new_ward_funct(4) %>%
  new_ward_funct(5)

label_output <- function (df, label_text) {
  label_standardize(df, label_text) %>% label_html(.)
}


label_standardize <- function (df, label_text) {
  # browser()

  df %>% dplyr::mutate(label_col = glue::glue(label_text)) %>%
    dplyr::pull(label_col)
}


addpoly_standard <- function(basemap,
                             df,
                             pal_funct,
                             variable,
                             group,
                             labels,
                             .weight = 1,
                             .color = "transparent",
                             .opacity = 1,
                             .fillopacity = 0.4,
                             .data = leaflet::getMapData(basemap)){

  basemap %>%
    leaflet::addPolygons(color = .color,
                         weight = .weight,
                         fill = T,
                         opacity = .opacity,
                         fillOpacity = .fillopacity,
                         fillColor = ~ pal_funct(df[[variable]]),
                         group = group,
                         highlight = leaflet::highlightOptions(stroke = TRUE,
                                                               weight = 3.5,
                                                               fillOpacity = 0.6,
                                                               color = "#555EE7",
                                                               opacity = 1,
                                                               bringToFront = TRUE),
                         label = purrr::map(labels, htmltools::HTML),
                         labelOptions = leaflet::labelOptions(
                           style = list("font-weight" = "normal",
                                        "padding" = "0.2px 0.2px",
                                        "line-height" = 0.8),
                           textsize = "10px",
                           direction = "auto",
                           opacity = 0.8),
                         data = .data)
}

addlegend_standard <- function(basemap,
                               df,
                               pal_funct,
                               variable,
                               group,
                               title,
                               .opacity = 0.7,
                               .data = leaflet::getMapData(basemap)){
  basemap %>%
    leaflet::addLegend("topright",
                       pal = pal_funct,
                       values = ~ df[[variable]],
                       opacity = .opacity,
                       group = group,
                       title = title,
                       data = .data)
}


addpoly_legend <- function(basemap_select,
                           df_select,
                           pal_funct_select,
                           variable_select,
                           group_select,
                           title_select,
                           labels_select,
                           .legopacity = 0.7,
                           .polopacity = 1,
                           .polfillopacity= 0.6,
                           .polweight = 1,
                           .pollinecolor = "transparent",
                           .data = leaflet::getMapData(basemap_select)){

  basemap_select %>%
    addpoly_standard(df = df_select,
                     pal_funct = pal_funct_select,
                     variable = variable_select,
                     group = group_select,
                     labels = labels_select,
                     .weight = .polweight,
                     .color = .pollinecolor,
                     .opacity = .polopacity,
                     .fillopacity = .polfillopacity,
                     .data = .data) %>%
    addlegend_standard(df = df_select,
                       pal_funct = pal_funct_select,
                       variable = variable_select,
                       group = group_select,
                       title = title_select)
}


colnames(wards_new)

label_funct <- function(df, option){
  label_output(df, label_text =
  "Block name: {name}<p></p>
  Current Ward: {current_district}<p></p>
  Population: {pop_total}<p></p>
  White residents: {pop_race_white}<p></p>
  Black residents: {pop_race_black}<p></p>
  Hispanic residents: {pop_hisprace_hisp_tot}<p></p>
  Asian residents: {pop_race_asian}<p></p>
  Other residents: {pop_race_other}<p></p>
  Multiracial residents: {pop_race_tom}")
}

colors_wards <- leaflet::colorFactor(palette = c("#f0c45c", "#98ccf4", "#80ccb4", "#f8ec7c", "#7cb8d4", "#e8945c"), domain = 1:6, ordered = T)

leaflet_wards <- function(basemap, df, opt_num, grp = ": changed blocks", curr_ward = F){

  if (grp == ": changed blocks"){
    col <- paste0("option_", opt_num, "_wardchng")

    df <- df %>%
      filter(!!sym(col) == F)
  }

  addpoly_legend(basemap_select = basemap, df_select = df %>% st_drop_geometry(), pal_funct_select = colors_wards, variable_select = ifelse(curr_ward, "current_district", paste0("option_", opt_num)), group_select =ifelse(curr_ward, "Current wards", paste0("Option ", opt_num, grp)), title_select = ifelse(curr_ward, "Current wards", paste0("Option ", opt_num, grp)), labels_select = label_funct(df %>% st_drop_geometry(), opt_num), .data = df)
}

grp_vac <- c(
  "Current wards",
  map_chr(1:5, ~ paste0("Option ", .x)),
  map_chr(1:5, ~ paste0("Option ", .x, ": changed blocks"))
)

chng_map <- leaflet(wards_new) %>%
  addTiles(options = tileOptions(opacity = 0.4)) %>%
  leaflet_wards(wards_new, "", curr_ward = T,grp = "") %>%
  leaflet_wards(wards_new, 1, curr_ward = F, grp = "") %>%
  leaflet_wards(wards_new, 2, curr_ward = F, grp = "") %>%
  leaflet_wards(wards_new, 3, curr_ward = F, grp = "") %>%
  leaflet_wards(wards_new, 4, curr_ward = F, grp = "") %>%
  leaflet_wards(wards_new, 5, curr_ward = F, grp = "") %>%
  leaflet_wards(wards_new, 1, curr_ward = F) %>%
  leaflet_wards(wards_new, 2, curr_ward = F) %>%
  leaflet_wards(wards_new, 3, curr_ward = F) %>%
  leaflet_wards(wards_new, 4, curr_ward = F) %>%
  leaflet_wards(wards_new, 5, curr_ward = F) %>%
  addLayersControl(overlayGroups = grp_vac, position = "topleft", options = layersControlOptions(collapsed = F)) %>%
  hideGroup(group = grp_vac[-1]) %>%
  add_wards()


htmlwidgets::saveWidget(chng_map, file = "./redistrictingoptions.html")
