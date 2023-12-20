## Oh my lordy lord, there's spatial structure everywhere

![](https://img.shields.io/badge/-R_Markdown-blue?style=flat&labelColor=white&logo=RStudio&logoColor=blue)
![](https://img.shields.io/badge/license-MIT-green)

And we want maps!
![](/reports/figures/sample_maps.png)

### Project Organization


    ├── LICENSE
    ├── README.md          <- The top-level README
    ├── data               <- Source data needed to reproduce the analyses,
    │                         derived datasets and model fits
    │
    ├── notebooks          <- .Rmd notebooks or .R scripts
    │   └── main.Rmd
    │   ...
    │                                 
    ├── reports            <- Analysis as HTML, PDF, LaTeX, etc.
    │   └── figures        <- Graphics and figures.
    │
    ├── renv.lock          <- Requirements to reproduce the dev environment,
    │                         generated with renv::snapshot()
    │
    ├── .Rprofile          <- Used to activate renv for new R sessions
    │
    ├── maps.Rproj         <- To load the project if using RStudio
    │
    ├── renv         
    │   └── activate.R     <- Activation script run by the project's Rprofile
    │
    ├── src                <- Source code for this project
    │   ├── read.R 
    │   ├── compute.R
    │   ├── dirs.R
        └── plot.R


<br>

### To use this repository:

1. Navigate to the folder where you want to install the repository. Then type
   `git clone https://github.com/nilomr/SpatEffsMaps.git`

2. Open the `maps.Rproj` file. The `renv` package, used for
   dependency management, will be automatically installed if it isn't already.

3. In your R console, type `renv::restore(lockfile = "renv.lock")`. This will
   install **the project's R dependencies**\
— you might still need to manually fix some unresolved system dependencies.

1. Open and run the `main.Rmd` notebook.

<br>

### Use notes

#### 2D maps
There are many ways to get a 2d surface from the data. Here we use two:
1. Interpolate directly from the data to a raster
2. Fit a model to the data and extract the predictions to a raste
2 allows you to deal with multilevel data, include other predictors, etc.,
as well as get standard errors for the predictions. 1 is faster and simpler.

#### 3D maps

[...]

### Changelog
You can see project history and work in progress in the [changelog](./docs/CHANGELOG.md).

### License
The project is licensed under the [MIT license](./LICENSE).

--------

<p><small>2022 | Nilo M. Recalde</small></p>
