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


# ──── IMPORT DATA ────────────────────────────────────────────────────────────


# Nestbox information
nestbox_df <- read_csv(file.path(config$path$resources, "nestbox_data.csv")) |>
    janitor::clean_names()

# Breeding data*
breeding_df <- read_csv(file.path(config$path$data, "breeding_data.csv")) |>
    dplyr::mutate(box = toupper(box)) |>
    dplyr::left_join(nestbox_df, by = "box")

# Laydates
laydates_df <- breeding_df |>
    dplyr::filter(year > 2014) |>
    dplyr::filter(!is.na(x) & !is.na(y)) |>
    dplyr::filter(species == "g")

# Oak data
oak_df <- read_csv(file.path(config$path$resources, "oak_data.csv")) |>
    janitor::clean_names()

# (add density of oak trees within 75m)
oak_density <-
    oak_df |>
    dplyr::mutate(
        n_oak = purrr::map2_dbl(
            x, y,
            ~ sum(
                (oak_df$x - .x)^2 + (oak_df$y - .y)^2 < 75^2
            )
        )
    )

# Load perimeter shapefile
pop_contour <- terra::vect(
    file.path(config$path$resources, "wytham_map", "perimeter.shp")
) |>
    terra::project("EPSG:27700")
pop_contour_sf <- pop_contour |> sf::st_as_sf()


# ──── MODELS ─────────────────────────────────────────────────────────────────

## Formulae

# I'll use default priors since this is just a minimal example

# For info on approximate gaussian process, choice of basis functions, etc,
# see https://arxiv.org/abs/2004.11408

# if you have a lot more data, fit a spline-based smooth
# [e.g., s(x,y, by=year)] instead

# Spatial distribution of lay dates (2 year only)
m_0_f <- brms::brmsformula(
    laydate ~ 1 + year + s(x, y),
    family = gaussian()
)

# Spatial distribution of oak tree density
m_1_f <- brms::brmsformula(
    n_oak ~ 1 + s(x, y, k = 100, bs = "tp"),
    family = gaussian()
)


# ──── FIT MODELS ─────────────────────────────────────────────────────────────


m_0 <-
    brms::brm(
        formula = m_0_f,
        data = laydates_df,
        iter = 1000,
        warmup = 300,
        chains = 4,
        cores = parallel::detectCores(),
        seed = 444,
        threads = brms::threading(2),
        sample_prior = FALSE,
        file = file.path(config$path$fits, "m_0"),
        file_refit = "on_change",
        backend = "cmdstanr"
    )

m_1 <-
    brms::brm(
        formula = m_1_f,
        data = oak_density,
        iter = 1000,
        warmup = 300,
        chains = 4,
        cores = parallel::detectCores(),
        seed = 444,
        threads = brms::threading(2),
        sample_prior = FALSE,
        file = file.path(config$path$fits, "m_1"),
        file_refit = "on_change",
        backend = "cmdstanr"
    )



# ──── EXTRACT SPATIAL PREDICTIONS FROM THE MODELS ────────────────────────────

# Settings
resolution <- 20
ndraws <- 1000
year <- 2022
type <- "estimate" # or "se" if you want standard errors instead

models <- list(m_0, m_1)
model_data <- list(laydates_df, oak_density)
model_names <- c("m_0", "m_1")

for (i in seq_along(models)) {
    # Get marginal predictions from the model across a spatial grid
    preds <- get_spatial_preds(
        models[[i]], model_data[[i]],
        resolution, ndraws
    )

    # Now build a raster from the predictions
    rast <- get_raster(
        preds, pop_contour,
        year = NULL, type, resolution,
        fact = 4
    )

    # Save the raster as a .tif file
    terra::writeRaster(
        rast |> terra::rast(),
        file.path(
            config$path$resources, "2d_maps",
            paste0(model_names[i], "_rast.tif")
        ),
        overwrite = TRUE
    )
}

# ──── PLOT MAP ───────────────────────────────────────────────────────────────

# This is just a minimal example, you can customise the plot as you wish

# Plot settings
psetts <- list(
    text.colour = "#f2f2f2",
    background.colour = "#262626", # "transparent"
    text.size = 10
)

# Plot the raster using ggplot

m_0_plot <- ggplot2::ggplot() +
    ggplot2::geom_raster(
        data = rast, aes(x = x, y = y, fill = !!sym(type))
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
        breaks = plt$plot_ticks(type, rast, 5),
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
