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
box::use(rayshader)


# ──── IMPORT DATA ────────────────────────────────────────────────────────────


# Nestbox information
nestbox_df <- read_csv(file.path(config$path$resources, "nestbox_data.csv")) %>%
    janitor::clean_names()

# Breeding data*
breeding_df <- read_csv(file.path(config$path$data, "breeding_data.csv")) %>%
    dplyr::mutate(box = toupper(box)) %>%
    dplyr::left_join(nestbox_df, by = "box")


# Load perimeter shapefile
pop_contour <- terra::vect(
    file.path(config$path$resources, "wytham_map", "perimeter.shp")
) |>
    terra::project("+init=epsg:27700")
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


# ──── FIT MODELS ─────────────────────────────────────────────────────────────

m_0_data <- breeding_df |>
    dplyr::filter(year > 2014) |>
    dplyr::filter(!is.na(x) & !is.na(y)) |>
    # select where species == g
    dplyr::filter(species == "g")

m_0 <-
    brms::brm(
        formula = m_0_f,
        data = m_0_data,
        # prior = priors,
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


# ──── EXTRACT SPATIAL PREDICTIONS ────────────────────────────────────────────

# Settings
resolution <- 20
ndraws <- 1000
year <- 2022
type <- "estimate" # or "se" if you want standard errors instead

# Get marginal predictions from the model across a spatial grid
m_0_preds <- get_spatial_preds(m_0, m_0_data, resolution, ndraws)

# Now build a raster from the predictions
nsm1_rast <- get_raster(
    m_0_preds, pop_contour,
    year = NULL, type, resolution,
    fact = 4
)
# This nsm1_rast object is a tibble with columns x, y, and the estimate.


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
