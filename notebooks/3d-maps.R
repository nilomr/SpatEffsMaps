# ──── DEPENDENCIES ───────────────────────────────────────────────────────────

config <- config::get()
box::use(compute = src / compute)
box::use(src / utils[get_spatial_preds, get_raster])
box::use(plt = src / plot)
box::use(readr[read_csv])
box::use(brms[...])
box::use(ggplot2[...])
box::use(dplyr)
box::use(terra)
box::use(sf)
box::use(ggtext)
box::use(patchwork[...])
box::use(rayshader[...])

# ──── FUNCTION DEFINITIONS ───────────────────────────────────────────────────

# To be refactored

#' @title Resample elevation raster to a new resolution
#'
#' This function resamples the elevation raster to a new resolution using either
#' aggregation or disaggregation based on the factor between the current and new resolution.
#'
#' @param elevation The elevation raster to be resampled.
#' @param new_res The desired new resolution in meters.
#'
#' @return The resampled elevation raster.
#'
#' @examples
#' elevation <- resample_elevation(elevation, 5)
#'
#' @export
resample_elevation <- function(elevation, new_res) {
    current_res <- terra::yres(elevation) # (m)
    factor <- new_res / current_res

    if (factor > 1) {
        # if larger, aggregate
        elevation <- terra::aggregate(elevation, factor, fun = mean)
    } else {
        # if smaller, disaggregate
        disagg_factor <- 1 / factor
        elevation <- terra::disagg(elevation, disagg_factor, method = "bilinear")
    }

    return(elevation)
}


#' Set Alpha Layer in a RGB Array
#'
#' This function sets the alpha layer in the rgb array based on the values
#' in the previous three layers.
#'
#' @param rgbarray The rgbarray array with dimensions 261 x 312 x 4.
#' @param opacity The opacity value to set for the alpha layer.
#' @return The modified rgbarray array with the alpha layer updated.
#' @examples
#' rgbarray <- set_alpha_layer(rgbarray, 0.6)
set_alpha_layer <- function(rgbarray, opacity) {
    h <- dim(rgbarray)[1]
    w <- dim(rgbarray)[2]
    rgbarray <- array(c(rgbarray, rep(1, h * w)), dim = c(h, w, 4))
    # Iterate over each pixel in the rgbarray array
    for (i in 1:dim(rgbarray)[1]) {
        for (j in 1:dim(rgbarray)[2]) {
            # Check if the values in the previous three layers are all 1
            if (rgbarray[i, j, 1] == 1 & rgbarray[i, j, 2] == 1 &
                rgbarray[i, j, 3] == 1) {
                # Set the opacity value in the alpha layer
                rgbarray[i, j, 4] <- opacity
            }
        }
    }
    return(rgbarray)
}


# ──── IMPORT DATA ────────────────────────────────────────────────────────────

# Bird data

# Nestbox information
nestbox_df <- read_csv(file.path(config$path$resources, "nestbox_data.csv")) |>
    janitor::clean_names()

# Breeding data*
breeding_df <- read_csv(file.path(config$path$data, "breeding_data.csv")) |>
    dplyr::mutate(box = toupper(box)) |>
    dplyr::left_join(nestbox_df, by = "box")


# Geographic data

# Load perimeter shapefile
pop_contour <- terra::vect(
    file.path(config$path$resources, "wytham_map", "perimeter.shp")
) |>
    terra::project("EPSG:27700")
pop_contour_sf <- pop_contour |> sf::st_as_sf()

# Load elevation raster
elevation <- raster::raster(
    file.path(
        config$path$resources, "elevation_map",
        "10m_DTM_2022_wytham_wider.tif"
    )
) |>
    terra::rast() |>
    terra::project("EPSG:27700") |>
    terra::crop(raster::extent(445000, 448700, 206500, 209180))

# Resample to 5m resolution
elevation <- resample_elevation(elevation, 4) # 4m resolution for final render

res <- terra::yres(elevation) # spatial resolution (assumes square pixels)
hc <- 4 # height exaggeration factor


# ──── LOAD 2D RASTERS (VARIABLES TO PLOT) ────────────────────────────────────


# Load laydates raster
laydates <- raster::raster(
    file.path(config$path$resources, "2d_maps", "nsm1_rast.tif")
) |>
    terra::rast()

terra::crs(laydates) <- terra::crs(elevation)

# set data to the same extent as elevation
laydates <- terra::resample(laydates, elevation, method = "bilinear")
laydates_matrix <- raster_to_matrix(laydates)

# get the extent of the elevation raster
extent <- terra::ext(elevation)

# pop_contour_sf to linestring
pop_contour_ls <- pop_contour_sf |>
    sf::st_cast("LINESTRING")

# And convert it to a matrix:
elmat <- raster_to_matrix(elevation)


# ──── SIMPLE 3D PLOT W/ RAYSHADER ────────────────────────────────────────────


# Prepare a heightmap for the base 3D plot
pal <- "Demuth"
water_palette <- rev(colorRampPalette(MetBrewer::met.brewer(pal))(200))
elevation_masked <- terra::mask(elevation, pop_contour_sf)
elevation_hs <- height_shade(raster_to_matrix(elevation_masked),
    texture = water_palette
)

# Set the alpha layer to 0.6
elevation_hs <- set_alpha_layer(elevation_hs, 0.6)


# Plot the 3D map
elmat %>%
    sphere_shade(texture = "imhof2") |>
    add_overlay(
        height_shade(
            elmat,
            texture = (grDevices::colorRampPalette(c("white")))(256)
        ),
        alphalayer = 1,
    ) |>
    add_overlay(
        overlay = elevation_hs,
        alphalayer = 1,
        alphamethod = "max"
    ) |>
    add_overlay(
        generate_line_overlay(
            pop_contour_sf, # using pop_contour_sf instead == solid area
            extent,
            heightmap = elmat,
            linewidth = 3
        )
    ) |>
    add_shadow(lamb_shade(elmat, zscale = 6), 0) |>
    add_shadow(
        ray_shade(
            elmat,
            sunaltitude = 20,
            sunangle = -20,
            zscale = res / hc,
            multicore = TRUE
        ),
        max_darken = 0.02
    ) |>
    add_shadow(
        ambient_shade(
            elmat,
            zscale = res / hc,
        ), 0.01
    ) |>
    plot_3d(
        elmat,
        zscale = res / hc,
        baseshape = "rectangle",
        fov = 10,
        theta = 10,
        zoom = 0.55,
        phi = 30,
        windowsize = c(5200, 3000),
        shadow = FALSE,
        solid = FALSE,
        soliddepth = 0.01
    )

render_snapshot(
    filename = file.path(config$path$figures, "3d_map_test.png"),
    clear = TRUE
)


# ──── PLOT VARIABLES ─────────────────────────────────────────────────────────


# Prepare the heightmap with a relevant variable
laydates_palette <- rev(colorRampPalette(c("#006577", "#fc4700"))(200))
laydates_hs <- height_shade(laydates_matrix, texture = laydates_palette)


elmat %>%
    sphere_shade(texture = "imhof2") |>
    add_overlay(
        overlay = laydates_hs,
        alphalayer = 1,
        alphamethod = "max"
    ) |>
    add_overlay(
        generate_line_overlay(
            pop_contour_ls, # using pop_contour_sf instead == solid area
            extent,
            heightmap = elmat,
            linewidth = 3
        )
    ) |>
    # add_shadow(lamb_shade(elmat, zscale = 6), 0) |>
    # add_shadow(
    #     ray_shade(
    #         elmat,
    #         sunaltitude = 20,
    #         sunangle = -20,
    #         zscale = res / hc,
    #         multicore = TRUE
    #     ),
    #     max_darken = 0.02
    # ) |>
    # add_shadow(
    #     ambient_shade(
    #         elmat,
    #         zscale = res / hc,
    #     ), 0.01
    # ) |>
    plot_3d(
        elmat,
        zscale = res / hc,
        baseshape = "rectangle",
        fov = 10,
        theta = 10,
        zoom = 0.55,
        phi = 30,
        windowsize = c(5200, 3000),
        shadow = FALSE,
        solid = FALSE,
        soliddepth = 0.01
    )

render_snapshot(
    filename = file.path(config$path$figures, "3d_map_test.png"),
    clear = TRUE
)
