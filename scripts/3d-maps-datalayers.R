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

# Import great tit box locations:
box_coords <- read_csv(file.path(
    config$path$resources,
    "nestbox_data.csv"
))



# Resample to 5m resolution
elevation <- resample_elevation(elevation, 10) # 4m resolution for final render

res <- terra::yres(elevation) # spatial resolution (assumes square pixels)
hc <- 4 # height exaggeration factor


# get the extent of the elevation raster
extent <- terra::ext(elevation)

# pop_contour_sf to linestring
pop_contour_ls <- pop_contour_sf |>
    sf::st_cast("LINESTRING", warn = FALSE)

# And convert it to a matrix:
elmat <- raster_to_matrix(elevation)


# ──── PLOT VARIABLES ─────────────────────────────────────────────────────────

# Mask elevation raster to the extent of the perimeter shapefile
elevation_masked <- terra::mask(elevation, pop_contour_sf)
elmat_masked <- raster_to_matrix(elevation_masked)


# Get the file paths of all .tif rasters in the "2d_maps" folder
raster_files <- list.files(file.path(config$path$resources, "2d_maps"),
    pattern = "\\.tif$", full.names = TRUE
)
# assign a name to each path
names(raster_files) <- c("laydates", "oaks")

# Define palettes for each raster

# oak_palette <- rev(colorRampPalette(MetBrewer::met.brewer("VanGogh3"))(200))
oak_palette <- scico::scico(n = 200, palette = "lipari")

palettes <- list(
    laydates = c(
        "#992e16", "#bb5743", "#be764c", "#C08E39",
        "#565C33", "#184948", "#022A2A"
    ),
    oaks = oak_palette
)

# Create an empty list to store the height maps
height_maps <- list()

# Loop through each raster file and its name
for (file in names(raster_files)) {

    # Load the raster
    raster <- raster::raster(raster_files[[file]]) |> terra::rast()

    # Set the CRS to match elevation_masked
    terra::crs(raster) <- terra::crs(elevation_masked)

    # Resample the raster to match elevation_masked
    raster <- terra::resample(raster, elevation_masked, method = "bilinear")

    # Convert the raster to a matrix
    raster_matrix <- raster_to_matrix(raster, verbose = FALSE)

    # Generate the height map using a color palette
    palette <- colorRampPalette(palettes[[file]])(1000)
    height_map <- height_shade(raster_matrix, texture = palette)

    # Add the height map to the list
    height_maps[[file]] <- height_map

    # generate color bar plot for laydates
    laydates_color_bar_plot <- generate_color_bar_plot(
        raster, palette, file
    )

    # save plot
    ggsave(
        filename = file.path(
            config$path$figures,
            paste0("colorbar_", file, ".svg")
        ),
        plot = laydates_color_bar_plot,
        width = 600,
        height = 1500,
        units = "px",
        dpi = 300
    )

    elmat_masked %>%
        sphere_shade(texture = "bw") |>
        add_overlay(
            overlay = height_maps[[file]]
        ) |>
        add_shadow(lamb_shade(elmat_masked, zscale = 6), 0) |>
        add_shadow(
            ray_shade(
                elmat_masked,
                sunaltitude = 20,
                sunangle = -20,
                zscale = res / hc,
                multicore = TRUE
            ),
            max_darken = 0.02
        ) |>
        add_shadow(
            ambient_shade(
                elmat_masked,
                zscale = res / hc,
            ), 0.01
        ) |>
        add_overlay(
            overlay = height_maps[[file]],
            alphalayer = 0.7,
        ) |>
        add_overlay(
            generate_line_overlay(
                pop_contour_ls, # using pop_contour_sf instead == solid area
                extent,
                heightmap = elmat_masked,
                linewidth = 4
            )
        ) |>
        plot_3d(
            elmat_masked,
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

    render_points(
        extent = extent, lat = box_coords$y, long = box_coords$x,
        offset = 0, zscale = res / hc, color = "black", heightmap = elmat_masked
    )

    render_snapshot(
        filename = file.path(config$path$figures, paste0(file, "_datalayer.png")),
        clear = TRUE
    )
}


# ──── HELLO THIS IS A TEST ───────────────────────────────────────────────────


# Load the raster
file <- "laydates"
raster <- raster::raster(raster_files[[file]]) |> terra::rast()

# Set the CRS to match elevation_masked
terra::crs(raster) <- terra::crs(elevation_masked)

# Resample the raster to match elevation_masked
raster <- terra::resample(raster, elevation_masked, method = "bilinear")

# Convert the raster to a matrix
raster_matrix <- raster_to_matrix(raster, verbose = FALSE)

# Generate the height map using a color palette
palette <- colorRampPalette("white")(1000)
height_map <- height_shade(raster_matrix, texture = palette)



elmat_masked %>%
    sphere_shade(texture = "bw") |>
    # add_overlay(
    #     overlay = height_map
    # ) |>
    add_shadow(lamb_shade(elmat_masked, zscale = 6), 0) |>
    add_shadow(
        ray_shade(
            elmat_masked,
            sunaltitude = 20,
            sunangle = -20,
            zscale = res / hc,
            multicore = TRUE
        ),
        max_darken = 0.02
    ) |>
    add_shadow(
        ambient_shade(
            elmat_masked,
            zscale = res / hc,
        ), 0.01
    ) |>
    add_overlay(
        overlay = height_map,
        alphalayer = 0.9,
    ) |>
    add_overlay(
        generate_line_overlay(
            pop_contour_ls, # using pop_contour_sf instead == solid area
            extent,
            heightmap = elmat_masked,
            linewidth = 4
        )
    ) |>
    plot_3d(
        elmat_masked,
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

render_points(
    extent = extent, lat = box_coords$y, long = box_coords$x,
    size = 4, color = "#ff0000a4",
    offset = 5, zscale = (res / hc) * 0.9, heightmap = elmat_masked
)
render_snapshot(
    filename = file.path(config$path$figures, paste0("boxes_datalayer.png")),
    clear = TRUE, point_radius = 5
)
