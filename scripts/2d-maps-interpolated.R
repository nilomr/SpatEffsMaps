# ──── DEPENDENCIES ───────────────────────────────────────────────────────────

config <- config::get()
box::use(compute = src / compute)
box::use(src / utils[get_spatial_preds, get_raster, interpolate_to_raster])
box::use(plt = src / plot)
box::use(readr[read_csv])
box::use(brms[...])
box::use(ggplot2[...])
box::use(dplyr)
box::use(terra)
box::use(sf)
box::use(ggtext)
box::use(patchwork[...])


# ──── IMPORT DATA ────────────────────────────────────────────────────────────


# Nestbox information
nestbox_df <- read_csv(file.path(config$path$resources, "nestbox_data.csv")) |>
    janitor::clean_names()

# Breeding data*
breeding_df <- read_csv(file.path(config$path$data, "breeding_data.csv")) |>
    dplyr::mutate(box = toupper(box)) |>
    dplyr::left_join(nestbox_df, by = "box")


# Oak data
oak_df <- read_csv(file.path(config$path$resources, "oak_data.csv")) |>
    janitor::clean_names()

# (add density of oak trees within 75m)
oak_density <-
    oak_df |>
    dplyr::mutate(
        n_oak = log(purrr::map2_dbl(
            x, y,
            ~ sum(
                (oak_df$x - .x)^2 + (oak_df$y - .y)^2 < 75^2
            )
        ))
    )

# Load perimeter shapefile
pop_contour <- terra::vect(
    file.path(config$path$resources, "wytham_map", "perimeter.shp")
) |>
    terra::project("EPSG:27700")
pop_contour_sf <- pop_contour |> sf::st_as_sf()

# Define crs
crs = terra::crs(pop_contour)


# ──── INTERPOLATE DATA TO A RASTER ───────────────────────────────────────────

std_laydates <-
    breeding_df |>
    dplyr::filter(!is.na(x) & !is.na(y) & !is.na(laydate)) |>
    dplyr::filter(species == "g") |>
    dplyr::group_by(year) |>
    dplyr::mutate(
        laydate = laydate - mean(laydate)
    ) |>
    dplyr::ungroup()



# Oak tree density
oaks <- interpolate_to_raster(oak_density, "n_oak", 200, crs) |>
    terra::disagg(5) |>
    terra::mask(pop_contour)

# Lay dates
laydates <- interpolate_to_raster(std_laydates, "laydate", 100, crs) |>
    terra::focal(w = 11, fun = "mean", expand = TRUE, na.rm = TRUE) |>
    terra::disagg(5) |>
    terra::mask(pop_contour)


# Save the rasters as .tif files
rasters <- list(
    laydates = laydates,
    oaks = oaks
)
for (i in names(rasters)) {
    terra::writeRaster(
        rasters[[i]],
        file.path(
            config$path$resources, "2d_maps",
            paste0(i, "_2d_raster.tif")
        ),
        overwrite = TRUE
    )
}



# Plot the interpolated data
palette <- colorRampPalette(c(
    "#992e16", "#bb5743", "#be764c", "#C08E39",
    "#565C33", "#184948", "#022A2A"
))(200)
terra::plot(laydates, col = palette)
