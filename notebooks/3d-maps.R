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


# ──── IMPORT DATA ────────────────────────────────────────────────────────────


# Nestbox information
nestbox_df <- read_csv(file.path(config$path$resources, "nestbox_data.csv")) |>
    janitor::clean_names()

# Breeding data*
breeding_df <- read_csv(file.path(config$path$data, "breeding_data.csv")) |>
    dplyr::mutate(box = toupper(box)) |>
    dplyr::left_join(nestbox_df, by = "box")


# Load perimeter shapefile
pop_contour <- terra::vect(
    file.path(config$path$resources, "wytham_map", "perimeter.shp")
) |>
    terra::project("+init=epsg:27700")
pop_contour_sf <- pop_contour |> sf::st_as_sf()


# Load elevation raster
elevation <- raster::raster(
    file.path(config$path$resources, "elevation_map", "10m_DTM_2022_wytham_wider.tif")
) |>
    terra::rast() |>
    terra::project("EPSG:27700")


# resample to 20m
elevation <- terra::aggregate(elevation, 2, fun = mean)

# ──── LOAD 2D RASTERS (VARIABLES TO PLOT) ────────────────────────────────────


# Load elevation raster
laydates <- raster::raster(
    file.path(config$path$resources, "2d_maps", "nsm1_rast.tif")
) |>
    terra::rast()

terra::crs(laydates) <- terra::crs(elevation)

# set laydates to the same extent as elevation
laydates <- terra::resample(laydates, elevation, method = "bilinear")
laydates_matrix <- raster_to_matrix(laydates)

# create a 2d RGB array with a color palette of the same size as the elevation raster.
overlay_template = array(0, dim = c(nrow(elevation), ncol(elevation), 4))

# map the elevation raster to a color palette




# # Resample to 5m
# elevation <- terra::disagg(elevation, 4, method = "bilinear")


# ──── SIMPLE 3D PLOT W/ RAYSHADER ────────────────────────────────────────────


# And convert it to a matrix:
elmat <- raster_to_matrix(elevation)


# We use another one of rayshader's built-in textures:
elmat |>
    sphere_shade(texture = "desert") |>
    add_shadow(ray_shade(elmat, zscale = 4), 0.8) |>
    add_shadow(ambient_shade(elmat), 0) |>
    add_overlay(generate_altitude_overlay(elmat, laydates_matrix, 100, 400))
plot_3d(
    elmat,
    zscale = 5,
    baseshape = "circle",
    fov = 10,
    theta = 0,
    zoom = 0.5,
    phi = 40,
    windowsize = c(1500, 700),
    shadow = FALSE,
    solid = FALSE,
    soliddepth = 0
)
Sys.sleep(0.2)

render_snapshot(
    filename = file.path(config$path$figures, "3d_map.png"),
    clear = TRUE
)


# ──── PLOT MAP ───────────────────────────────────────────────────────────────

# Plot settings
psetts <- list(
    text.colour = "#f2f2f2",
    background.colour = "#262626", # "transparent"
    text.size = 10
)

# Plot the raster using ggplot

m_0_plot <- ggplot2::ggplot() +
    ggplot2::geom_raster(
        data = nsm1_rast, aes(x = x, y = y, fill = !!sym(type))
    ) +
    ggplot2::geom_sf(
        data = sf::st_as_sf(pop_contour), fill = NA,
        size = 0.5,
        colour = psetts$text.colour
    ) +
    geom_point(
        data = nestbox_df, aes(
            x = x, y = y
        ),
        pch = 21, fill = "NA",
        color = "#414141", alpha = 0.4, size = 0.1
    ) +
    scale_fill_distiller(
        palette = "Spectral",
        name = ("Estimate\n(april day)\n"),
        breaks = plt$plot_ticks(type, nsm1_rast, 5),
        direction = 1,
        guide = guide_colorbar(
            ticks = FALSE,
            frame.colour = psetts$text.colour,
            frame.linewidth = 1.5,
            nbin = 1000
        )
    ) +
    plt$settheme(
        text.size = psetts$text.size,
        text.colour = psetts$text.colour,
        back.fill = psetts$background.colour
    ) +
    labs(
        x = NULL,
        y = NULL,
        title = "Estimates of first egg date across space",
        subtitle = "*Posterior conditional effects of spatial predictor (GP)*<br>"
    )
