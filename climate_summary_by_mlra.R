# load packages
library(terra)
library(stringr)

# ********** CHANGES *********
# load ak statsgo
ak_mu <- terra::vect("C:/Users/Nathan.Roe/Documents/MattMayerGIS/OneDrive_3_9-12-2024/gsmsoilmu_a_ak.shp")
ak_mlra <- terra::vect("C:/Users/Nathan.Roe/Documents/MattMayerGIS/OneDrive_2_9-12-2024/AK_MLRA_2012.shp")
ak_mlra <- terra::subset(ak_mlra, ak_mlra$MLRA == "239")

ak_mu <- terra::project(ak_mu, terra::crs(ak_mlra))
ak_mlra <- terra::crop(ak_mu, ak_mlra)

# *********** CHANGES *******************
# look at the object you just created by typing it into the console below.
# determine what column you want to use for a polygon identifier. For an
# MLRA analysis, the column might be "MLRA". For a mapunit approach, the
# column identifier might be "MUSYM" or "MUKEY"
poly_id <- "MUSYM"

# ********** CHANGES ****************
# this lists all the files locate in a directory - all the desired rasters
# should be located in this directory

# my_layers <- list.files("C:/Users/Nathan.Roe/Documents/MattMayerGIS/OneDrive_1_9-12-2024/tas_AK_CAN_2km_CRU_TS31_historical/tas_1980_2009",
#                         full.names = TRUE,
#                         pattern = ".tif$")

my_layers <- list.files("C:/Users/Nathan.Roe/Documents/MattMayerGIS/OneDrive_1_9-12-2024/pr_AK_771m_CRU_TS31_historical (1)/ppt_1980_2009",
                        full.names = TRUE,
                        pattern = ".tif$")

# create a sequence of all the years involved in analysis
my_years <- my_layers |> stringr::str_sub(start = -8L, end = -5L) |> unique()
my_years_name <- paste("year", my_years, sep = "_")

# lapply loops through the sequence of years in my_years and applies a function
years_list <- lapply(my_years, FUN = function(x){
  # the function is a pattern match, which chooses all of the files that
  # have the year in the name
  stringr::str_subset(my_layers, pattern = paste0(x, "\\.", "tif$"))
})

# create SpatRasters from filenames
years_rast <- lapply(years_list, FUN = function(x){
  my_rast <- terra::rast(x)
  terra::project(my_rast, terra::crs(ak_mlra))
})

my_stats <- c("mean", "min", "max")

# loop through years list, which contains all the .tif files assoc. with
# a particular year
year_stat <- lapply(seq_along(years_rast), FUN = function(x){
  lapply(my_stats, FUN = function(y){
    # calc cell value stat for each monthly raster
    temp_stat <- terra::zonal(years_rast[[x]], ak_mlra, fun = y, as.polygons = TRUE, touches = TRUE,
                              na.rm = TRUE) |>
      as.data.frame() |> tidyr::pivot_longer(cols = dplyr::all_of(years_rast[[x]] |> names()),
                                             names_to = "rast_name",
                                             values_to = y) |>
      dplyr::select(dplyr::all_of(poly_id), rast_name, dplyr::all_of(y)) |>
      dplyr::group_by(!!dplyr::sym(poly_id), rast_name) |>
      dplyr::summarise(!!dplyr::sym(y) := mean(get(y), na.rm = TRUE)) |>
      dplyr::mutate(year = my_years[x]) |>
      dplyr::select(year, dplyr::everything())
    # take the mean across the months
    #temp_stat |> rowMeans()
  })
})


year_stat_flat <- do.call(c, year_stat) |> dplyr::bind_rows() |>
  tidyr::pivot_longer(cols = dplyr::all_of(my_stats),
                      names_to = "stat",
                      values_to = "value") |>
  dplyr::filter(!is.na(value))

# ******* CHANGES **********
# you most likely want this value to be
# mean. if you use min or max, it will pull the lowest
# or highest value from across the whole
# study area at each month, rather than taking the average
# across the study area for the month. using lows or highs
# is a pretty extreme way of summarizing the conditions.
# for precip, min and max are not appropriate, as the
# summing process would then be summing the most extreme
# conditions from across the study area.

across_monthly_rast <- "mean"

yearly_stats <- lapply(my_years, FUN = function(x){
  year_stat_flat |> dplyr::filter(stat == across_monthly_rast & year == x) |>
    dplyr::group_by(year, !!dplyr::sym(poly_id)) |> dplyr::summarise(mean = mean(value),
                                                     max = max(value),
                                                     min = min(value),
                                                     sum = sum(value),
                                                     sd = sd(value))
})

yearly_stats_collapse <- do.call(rbind, yearly_stats)

write.csv(yearly_stats_collapse, "C:/Users/Nathan.Roe/Documents/MattMayerGIS/annual_stats.csv")

####################

mus <- c("E39C1", "E39LM")

yearly_stats_collapse2 |> dplyr::filter(MUSYM %in% mus) |>
  dplyr::group_by(year) |> dplyr::summarise(mean = mean(mean),
                                            max = mean(max),
                                            min = mean(min),
                                            sum = mean(sum))


