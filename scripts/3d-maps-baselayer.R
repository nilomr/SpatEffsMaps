# ──── DEPENDENCIES ───────────────────────────────────────────────────────────

config <- config::get()
box::use(src / utils[resample_elevation])
box::use(src / plot[generate_color_bar_plot, set_alpha_layer])
box::use(readr[read_csv])
box::use(ggplot2[...])
box::use(dplyr)
box::use(terra)
box::use(sf)
box::use(rayshader[...])

# ──── FUNCTION DEFINITIONS ───────────────────────────────────────────────────



# ──── IMPORT DATA ────────────────────────────────────────────────────────────

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


# get the extent of the elevation raster
extent <- terra::ext(elevation)

# pop_contour_sf to linestring
pop_contour_ls <- pop_contour_sf |>
    sf::st_cast("LINESTRING", warn = FALSE)

# And convert it to a matrix:
elmat <- raster_to_matrix(elevation)


# ──── SIMPLE 3D PLOT W/ RAYSHADER ────────────────────────────────────────────


# Prepare a heightmap for the base 3D plot
height_palette <- rev(colorRampPalette(MetBrewer::met.brewer("Demuth"))(200))
elevation_masked <- terra::mask(elevation, pop_contour_sf)
elevation_hs <- height_shade(raster_to_matrix(elevation_masked),
    texture = height_palette
)

# # Set the alpha layer to 0.6
elevation_hs <- set_alpha_layer(elevation_hs, 0)


# Plot the 3D map
elmat %>%
    sphere_shade(texture = "imhof2") |>
    add_overlay(
        height_shade(
            elmat,
            texture = (grDevices::colorRampPalette(c("#ffffff")))(256)
        ),
        alphalayer = 1,
    ) |>
    add_overlay(
        overlay = elevation_hs
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
    add_overlay(
        height_shade(
            elmat,
            texture = (grDevices::colorRampPalette(c("#ffffff")))(256)
        ),
        alphalayer = .2,
    ) |>
    add_overlay(
        overlay = elevation_hs,
        alphalayer = 0.7,
    ) |>
    add_overlay(
        generate_line_overlay(
            pop_contour_sf, # pop_contour_sf instead of '*_ls' == solid area
            extent,
            heightmap = elmat,
            linewidth = 4
        )
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
        solid = TRUE,
        soliddepth = 5,
        linewidth = 0.5,
        solidcolor = "#00ff00",
        solidlinecolor = "#006eff",
    )

render_snapshot(
    filename = file.path(config$path$figures, "base_3d_map_mask.png"),
    clear = TRUE
)
