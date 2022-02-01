library(dplyr)
library(tidyverse)
library(leaflet)
library(sf)
library(glue)

library(leafletwrappers)
library(tpfuncts)

md_data <- st_read("./data/output/shapefiles/md_data.geojson") %>%
  mutate(across(matches("pop|hous|area"), .fns = ~ as.numeric(.x))) %>%
  st_transform(4326)

pg_mont_data <- st_read("./data/output/shapefiles/pg_mont_data.geojson")%>%
  mutate(across(matches("pop|hous|area"), .fns = ~ as.numeric(.x))) %>%
  st_transform(4326)

city_blocks <- st_read("./data/output/shapefiles/city_blocks.geojson")

md_places <- st_read("./data/output/shapefiles/md_places_data.geojson")

md_as_data <- md_data %>% st_drop_geometry()

par <- "<p></p>"

labels <- map(1:nrow(md_data), ~ glue("{as_data[.x, 'name20']}{par}
                                      Population: {as_data[.x, 'pop_tot']}"))

bin_select <- function(vector, bins, .var, .data){

  correct <- F

  while (!correct){

    if (bins == 0){
      browser()
    }

    print(bins)

    bin_val <- quantile(x = vector, probs = seq(0, 1, 1 / bins), na.rm = T)

    unique_bins <- unique(bin_val)

    if (length(unique_bins) == length(bin_val)){

      return(bins)

      correct <<- T
    }

    bins <- bins - 1

  }
}


addpoly_decennial <- function(map, var, group, labels, as_data, colors = "Blues", pal_type = "quantile", .palbins = 4){

  # browser()

  if (pal_type == "numeric") {
    map %>%
      addpoly_legend(df_select = as_data,
                     pal_funct_select = pal_numeric(df = as_data,
                                                    var = var,
                                                    colors = colors),
                     variable_select = var,
                     group_select = group, title_select = group,
                     labels_select = labels)

  }

  else if (pal_type == "quantile"){

    # decide right number of bins
    bins_correct <- bin_select(as_data[[var]], .palbins, .var = var, .data = as_data)

    # https://stackoverflow.com/questions/56241152/addlegend-display-value-range-instead-of-percentage-when-using-colorquantile
    correct <- F

    # sometimes colors produced by pal_funct not equal to number of bins making assigning labels harder - figure out right labels
    while (!correct){

      pal_funct <-leaflet::colorQuantile(palette = colors,
                                         domain = as_data[[var]],
                                         n = bins_correct)

      pal_colors <- unique(pal_funct(sort(as_data[[var]])))

      pal_labels <- quantile(as_data[[var]], probs = seq(0, 1, by = 1 / bins_correct), na.rm = T)
      pal_labels <- paste(lag(pal_labels) %>% commafy, pal_labels %>% commafy, sep = " - ")[-1]

      if (length(pal_colors) != length(pal_labels)){
        message(glue("Adjusting bins for {var}"))
        bins_correct <- bins_correct - 1
      }

      else if (length(pal_colors) == length(pal_labels)){
        correct <- T
      }

      if (bins_correct == 0){
        stop("error; palette labels won't equal palette colors")
      }
    }

    # if (length(pal_labels) != length(pal_colors)){
    #
    # }

    #

    # browser()

    map %>%
      addpoly_standard(df = as_data,
                       pal_funct = pal_funct,
                       variable = var,
                       group = group,
                       labels = labels) %>%
      addLegend(position = "topright", colors = pal_colors, opacity = 0.7, group =  group, title = group, labels = pal_labels)
  }

}

# test_city <- city_blocks %>%
#   st_drop_geometry()
#
# test_city_restrict <- test_city %>%
#   filter(pop_asian_pct > 0)
#
# pal_funct <-leaflet::colorQuantile(palette = "Blues",
#                                    domain = test_city_restrict[["pop_asian_pct"]],
#                                    n = 5)
# test_colors <- unique(pal_funct(sort(test_city_restrict[["pop_asian_pct"]])))
#
# pal_labels <- quantile(test_city_restrict[["pop_asian_pct"]], probs = seq(0, 1, by = 1 / 5), na.rm = T)
# pal_labels <- paste(lag(pal_labels), pal_labels, sep = " - ")[-1]


addpoly_decennial_pop <- function(basemap, var, group, as_data, labels, colors = "Blues", pal_type = "quantile", .palbins = 5){

  as_data <- as_data %>%
    filter(!!sym(var) > 0)

  basemap %>%
    addpoly_decennial(var = var, group = group, labels = labels, as_data = as_data, colors = colors, pal_type = pal_type, .palbins = .palbins)

}

generate_addpoly_thismap <- function(as_data, labels_funct, colors = "PuBu", numbins = 10){

  labels <- labels_funct(as_data)


  addpoly_thismap <- function(basemap, var, group, pal_type, pal_pop_ctrl = F){

    pal_funct <- addpoly_decennial

    if (pal_pop_ctrl){
      pal_funct <- addpoly_decennial_pop
    }

    basemap %>%
      pal_funct(var = var,
                as_data = as_data,
                group = group,
                pal_type = pal_type,
                labels = labels,
                colors = colors,
                .palbins = numbins)

  }

  return(addpoly_thismap)
}

# generate_labels_pop <- function(place_type = "name20"){
#
#   labels_pop <- function(as_data){
#     labels <- map(1:nrow(as_data), ~ glue("{as_data[.x, place_type]}{par}
#   Population: {as_data[.x, 'pop_tot']  %>% commafy}{par}
#   White population: {as_data[.x, 'pop_white_nh']}{par}
#   Black population: {as_data[.x, 'pop_black']}{par}
#   Hispanic population: {as_data[.x, 'pop_hisp']}{par}
#   Asian population: {as_data[.x, 'pop_asian']}{par}
#                                         'Other' population: {as_data[.x, 'pop_other']}{par}
#                                         AIAN population: {as_data[.x, 'pop_aian']}{par}
#                                         NHPI population: {as_data[.x, 'pop_nhpi']}"))
#
#   }
#
#   return(labels_pop)
#
# }

generate_labels_pop_pct <- function(place_type = "name20"){

  labels_pop <- function(as_data){
    labels <- map(1:nrow(as_data), ~ glue("{as_data[.x, place_type]}{par}
  Population: {as_data[.x, 'pop_tot']  %>% commafy}{par}
  White population: {as_data[.x, 'pop_white_nh_pct']}%{par}
  Black population: {as_data[.x, 'pop_black_pct']}%{par}
  Hispanic population: {as_data[.x, 'pop_hisp_pct']}%{par}
  Asian population: {as_data[.x, 'pop_asian_pct']}%{par}
                                        'Other' population: {as_data[.x, 'pop_other_pct']}%{par}
                                        AIAN population: {as_data[.x, 'pop_aian_pct']}%{par}
                                        NHPI population: {as_data[.x, 'pop_nhpi_pct']}%"))

  }

  return(labels_pop)

}


#### resume - trying to figure out how to do pop controlled basemapss
test <- leaflet(md_places) %>%
  addpoly_standard(df = md_places %>% st_drop_geometry(), pal_funct = colorNumeric("Blues", md_places[["pop_tot"]]), group = "Cats", labels = label)

base_creator <- function(basemap, pal_pop_ctrl = F, place_type = "name20", .num_paltype = "quantile", .pct_paltype = "numeric"){

  as_data <- basemap %>% st_drop_geometry()

  labels_pop <- generate_labels_pop_pct(place_type = place_type)

  overlaygroups <-
    c(
      "Population",
      "Population: white",
      "Population: Black",
      "Population: Hispanic",
      "Population: Asian",
      "Population: AIAN",
      "Population: NHPI",
      "Population: 'other'",
      "Population-percent: white",
      "Population-percent: Black",
      "Population-percent: Hispanic",
      "Population-percent: Asian",
      "Population-percent: AIAN",
                     "Population-percent: NHPI",
      "Population-percent: 'other'")

  addpoly_thismap <- generate_addpoly_thismap(as_data = as_data, labels_funct = labels_pop, colors = "PuBu", numbins = 10)

  leaflet(basemap) %>%
    add_city() %>%
    add_wards() %>%
    addTiles() %>%
    addpoly_thismap(var = "pop_tot", group = "Population", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    addpoly_thismap(var = "pop_white_nh", group = "Population: white", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    addpoly_thismap(var = "pop_black", group = "Population: Black", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    addpoly_thismap(var = "pop_hisp", group = "Population: Hispanic", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
  addpoly_thismap(var = "pop_asian", group = "Population: Asian", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    addpoly_thismap(var = "pop_other", group = "Population: 'other'", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    addpoly_thismap(var = "pop_aian", group = "Population: AIAN", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    addpoly_thismap(var = "pop_nhpi", group = "Population: NHPI", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    addpoly_thismap(var = "pop_white_nh_pct", group = "Population-percent: white", pal_type = .pct_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    addpoly_thismap(var = "pop_black_pct", group = "Population-percent: Black", pal_type = .pct_paltype, pal_pop_ctrl = F) %>%
    addpoly_thismap(var = "pop_hisp_pct", group = "Population-percent: Hispanic", pal_type = .pct_paltype, pal_pop_ctrl = F) %>%
  addpoly_thismap(var = "pop_asian_pct", group = "Population-percent: Asian", pal_type = .pct_paltype, pal_pop_ctrl = F) %>%
  addpoly_thismap(var = "pop_other_pct", group = "Population-percent: 'other'", pal_type = .pct_paltype, pal_pop_ctrl = F) %>%
  addpoly_thismap(var = "pop_aian_pct", group = "Population-percent: AIAN", pal_type = .pct_paltype, pal_pop_ctrl = F) %>%
  addpoly_thismap(var = "pop_nhpi_pct", group = "Population-percent: NHPI", pal_type = .pct_paltype, pal_pop_ctrl = F) %>%
    addLayersControl(overlayGroups = overlaygroups) %>%
    hideGroup(overlaygroups[-1])

}

generate_labels_housing_pct <- function(place_type){
  labels_housing <- function(as_data){
    labels <- map(1:nrow(as_data), ~ glue("{as_data[.x, place_type]}{par}
  Housing units: {as_data[.x, 'hous_tot']} %>% commafy{par}
  Occupied units: {as_data[.x, 'hous_occ_pct'] %>% commafy}%{par}
  Vacant units: {as_data[.x, 'hous_vac_pct']  %>% commafy}%{par}
  Housing density: {as_data[.x, 'housing_density'] %>% commafy}%{par}
  Population: {as_data[.x, 'pop_tot']  %>% commafy}%{par}
  Population density: {as_data[.x, 'pop_density']  %>% commafy}%{par}"))
  }

  return(labels_housing)
}

base_creator_test <- function(basemap, pal_pop_ctrl = F, place_type = "name20", .num_paltype = "quantile", .pct_paltype = "numeric"){

  as_data <- basemap %>% st_drop_geometry()

  labels_pop <- generate_labels_pop_pct(place_type = place_type)

  overlaygroups <-
    c(
      # "Population",
      # "Population: white",
      "Population: Black"
      # "Population: Hispanic",
      # "Population: Asian",
      # "Population: AIAN",
      # "Population: NHPI",
      # "Population: 'other'",
      # "Population-percent: white",
      # "Population-percent: Black",
      # "Population-percent: Hispanic",
      # "Population-percent: Asian",
      # "Population-percent: AIAN",
      # "Population-percent: NHPI",
      # "Population-percent: 'other'")
    )

  addpoly_thismap <- generate_addpoly_thismap(as_data = as_data, labels_funct = labels_pop, colors = "PuBu", numbins = 10)

  leaflet(basemap) %>%
    add_city() %>%
    add_wards() %>%
    addTiles() %>%
    # addpoly_thismap(var = "pop_tot", group = "Population", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    # addpoly_thismap(var = "pop_white_nh", group = "Population: white", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    addpoly_thismap(var = "pop_black", group = "Population: Black", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    # addpoly_thismap(var = "pop_hisp", group = "Population: Hispanic", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    # addpoly_thismap(var = "pop_asian", group = "Population: Asian", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    # addpoly_thismap(var = "pop_other", group = "Population: 'other'", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    # addpoly_thismap(var = "pop_aian", group = "Population: AIAN", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    # addpoly_thismap(var = "pop_nhpi", group = "Population: NHPI", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    # addpoly_thismap(var = "pop_white_nh_pct", group = "Population-percent: white", pal_type = .pct_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    # addpoly_thismap(var = "pop_black_pct", group = "Population-percent: Black", pal_type = .pct_paltype, pal_pop_ctrl = F) %>%
    # addpoly_thismap(var = "pop_hisp_pct", group = "Population-percent: Hispanic", pal_type = .pct_paltype, pal_pop_ctrl = F) %>%
    # addpoly_thismap(var = "pop_asian_pct", group = "Population-percent: Asian", pal_type = .pct_paltype, pal_pop_ctrl = F) %>%
    # addpoly_thismap(var = "pop_other_pct", group = "Population-percent: 'other'", pal_type = .pct_paltype, pal_pop_ctrl = F) %>%
    # addpoly_thismap(var = "pop_aian_pct", group = "Population-percent: AIAN", pal_type = .pct_paltype, pal_pop_ctrl = F) %>%
    # addpoly_thismap(var = "pop_nhpi_pct", group = "Population-percent: NHPI", pal_type = .pct_paltype, pal_pop_ctrl = F) %>%
    addLayersControl(overlayGroups = overlaygroups) %>%
    hideGroup(overlaygroups[-1])

}

base_creator_test(md_places, pal_pop_ctrl = T, "name", "quantile")



# generate_labels_housing <- function(place_type){
#   labels_housing <- function(as_data){
#     labels <- map(1:nrow(as_data), ~ glue("{as_data[.x, place_type]}{par}
#   Housing units: {as_data[.x, 'hous_tot']}{par}
#   Occupied units: {as_data[.x, 'hous_occ']}{par}
#   Vacant units: {as_data[.x, 'hous_vac']}{par}
#   Housing density: {as_data[.x, 'housing_density']}{par}
#   Population: {as_data[.x, 'pop_tot']}{par}
#   Population density: {as_data[.x, 'pop_density']}{par}"))
#
#   }
#
#   return(labels_housing)
# }


base_creator_housing <- function(basemap, pal_pop_ctrl = F, place_type = "name20", .num_paltype = "quantile", .pct_paltype = "numeric"){

  as_data <- basemap %>% st_drop_geometry()

  labels_housing <- generate_labels_housing_pct(place_type = place_type)

  overlaygroups <- c("Housing units",
                     "Housing: occupied",
                     "Housing: vacant",
                     "Housing: unit-density",
                     "Population",
                     "Population density",
                     "Housing-percent: occupied",
                     "Housing-percent: vacant")

  addpoly_thismap <- generate_addpoly_thismap(as_data = as_data, labels_funct = labels_housing, colors = "PuBu", numbins = 10)

  leaflet(basemap) %>%
    add_city() %>%
    add_wards() %>%
    addTiles() %>%
    addpoly_thismap(var = "hous_tot", group = "Housing units", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    addpoly_thismap(var = "hous_occ", group = "Housing: occupied", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    addpoly_thismap(var = "hous_vac", group = "Housing: vacant", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    addpoly_thismap(var = "housing_density", group = "Housing: unit-density", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    addpoly_thismap(var = "pop_tot", group = "Population", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    addpoly_thismap(var = "pop_density", group = "Population density", pal_type = .num_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    addpoly_thismap(var = "hous_occ_pct", group = "Housing-percent: occupied", pal_type = .pct_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    addpoly_thismap(var = "hous_vac_pct", group = "Housing-percent: vacant", pal_type = .pct_paltype, pal_pop_ctrl = pal_pop_ctrl) %>%
    addLayersControl(overlayGroups = overlaygroups) %>%
    hideGroup(overlaygroups[-1])

}


# base_creator(md_data)

# takoma park maps
map_tp_pop_over0_quantpal <- base_creator(city_blocks,
                                 pal_pop_ctrl = T)


map_tp_pop_over0_numpal <- base_creator(city_blocks,
                                 pal_pop_ctrl = T,
                                 .num_paltype = "numeric")

htmlwidgets::saveWidget(map_tp_pop_over0_quantpal, "./data/output/maps/map_tp_pop_over0_quantpal.html", selfcontained = F)

htmlwidgets::saveWidget(map_tp_pop_over0_numpal, "./data/output/maps/map_tp_pop_over0_numpal.html", selfcontained = F)


# pg/montgomery county
map_pgmont_pop_over0_quantpal <- base_creator(pg_mont_data,
                                          pal_pop_ctrl = T)


map_pgmont_pop_over0_numpal <- base_creator(pg_mont_data,
                                        pal_pop_ctrl = T,
                                        .num_paltype = "numeric")

htmlwidgets::saveWidget(map_pgmont_pop_over0_quantpal, "./data/output/maps/map_pgmont_pop_over0_quantpal.html", selfcontained = F)

htmlwidgets::saveWidget(map_pgmont_pop_over0_numpal, "./data/output/maps/map_pgmont_pop_over0_numpal.html", selfcontained = F)

# montgomery county
mont_data <- pg_mont_data %>%
  filter(countyfp20 == "031")

map_mont_pop_over0_quantpal <- base_creator(mont_data,
                                              pal_pop_ctrl = T)


map_mont_pop_over0_numpal <- base_creator(mont_data,
                                            pal_pop_ctrl = T,
                                            .num_paltype = "numeric")

htmlwidgets::saveWidget(map_mont_pop_over0_quantpal, "./data/output/maps/map_mont_pop_over0_quantpal.html", selfcontained = F)

htmlwidgets::saveWidget(map_mont_pop_over0_numpal, "./data/output/maps/map_mont_pop_over0_numpal.html", selfcontained = F)

# maryland
map_md_pop_over0_quantpal <- base_creator(md_data,
                                              pal_pop_ctrl = T)
htmlwidgets::saveWidget(map_md_pop_over0_quantpal, "./data/output/maps/map_md_quantpal.html", selfcontained = F)


map_md_pop_over0_numpal <- base_creator(md_data,
                                            pal_pop_ctrl = T,
                                            .num_paltype = "numeric")


htmlwidgets::saveWidget(map_md_pop_over0_numpal, "./data/output/maps/map_md_pop_over0_numpal.html", selfcontained = F)


# housing maps tp
map_tp_hous_over0_quantile <- base_creator_housing(basemap = city_blocks, pal_pop_ctrl = T, .num_paltype = "quantile")

map_tp_hous_over0_num <- base_creator_housing(basemap = city_blocks, pal_pop_ctrl = T, .num_paltype = "numeric")

htmlwidgets::saveWidget(map_tp_hous_over0_quantile, "./data/output/maps/map_tp_hous_over0_quantile.html", selfcontained = F)

htmlwidgets::saveWidget(map_tp_hous_over0_num, "./data/output/maps/map_tp_hous_over0_num.html", selfcontained = F)


# split blocks
sf::sf_use_s2(FALSE)

wards_string <- wards %>%
  st_cast("MULTILINESTRING")

leaflet(wards_string) %>%
  addPolylines() %>%
  addTiles()

city_blocks_intersect <- lengths(st_intersects(city_blocks, wards_string, sparse = T)) > 0

city_blocks_housing_split <- city_blocks[city_blocks_intersect, ] %>%
  filter(hous_tot > 0)

map_tp_hous_split <- leaflet(city_blocks_housing_split) %>%
  addTiles(
    options = tileOptions(
      opacity = 0.25
    )
  ) %>%
  add_wards() %>%
  addPolygons(
    weight = 1,
    color = "black",
    opacity = 0.5,
    fill = T,
    fillColor = "blue",
    fillOpacity = 0.2,
    stroke = F,
    label = ~ glue("Block: {block}
                   \nHousing units: {hous_tot}"),
    highlightOptions = highlightOptions(
      stroke = T,
      color = "red",
      weight = 1.5,
      opacity = 0.5
    )
  )


htmlwidgets::saveWidget(map_tp_hous_split, "./data/output/maps/map_tp_hous_split.html")

# pop maps places

# housing maps pgmont
map_pgmont_hous_over0_quantile <- base_creator_housing(basemap = pg_mont_data, pal_pop_ctrl = T, .num_paltype = "quantile")

map_pgmont_hous_over0_num <- base_creator_housing(basemap = pg_mont_data, pal_pop_ctrl = T, .num_paltype = "numeric")

htmlwidgets::saveWidget(map_pgmont_hous_over0_quantile, "./data/output/maps/map_pgmont_hous_over0_quantile.html", selfcontained = F)

htmlwidgets::saveWidget(map_pgmont_hous_over0_num, "./data/output/maps/map_pgmont_hous_over0_num.html", selfcontained = F)

# housing maps mont
map_mont_hous_over0_quantile <- base_creator_housing(basemap = mont_data, pal_pop_ctrl = T, .num_paltype = "quantile")

map_mont_hous_over0_num <- base_creator_housing(basemap = mont_data, pal_pop_ctrl = T, .num_paltype = "numeric")

htmlwidgets::saveWidget(map_mont_hous_over0_quantile, "./data/output/maps/map_mont_hous_over0_quantile.html", selfcontained = F)

htmlwidgets::saveWidget(map_mont_hous_over0_num, "./data/output/maps/map_mont_hous_over0_num.html", selfcontained = F)


# place maps dc
map_places_over0_quantile <- base_creator(md_places, pal_pop_ctrl = T, place_type = "name", .num_paltype = "quantile")

map_places_over0_num <- base_creator(md_places, pal_pop_ctrl = T, place_type = "name", .num_paltype = "numeric")

htmlwidgets::saveWidget(map_places_over0_quantile, "./data/output/maps/map_places_over0_quantile.html", selfcontained = F)

htmlwidgets::saveWidget(map_places_over0_num, "./data/output/maps/map_places_over0_num.html", selfcontained = F)




# # housing maps maryland
# map_md_hous_over0_quantile <- base_creator_housing(basemap = md_data, pal_pop_ctrl = T, .num_paltype = "quantile")
#
# map_md_hous_over0_num <- base_creator_housing(basemap = md_data, pal_pop_ctrl = T, .num_paltype = "numeric")
#
# htmlwidgets::saveWidget(map_md_hous_over0_quantile, "./data/output/maps/map_md_hous_over0_quantile.html", selfcontained = F)
#
# htmlwidgets::saveWidget(map_md_hous_over0_num, "./data/output/maps/map_md_hous_over0_num.html", selfcontained = F)


