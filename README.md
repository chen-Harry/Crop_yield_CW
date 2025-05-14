# Crop Yield Project

The project aims to model the relationship between the yield of 3 crops, barley, rapeseed, and wheat, against temperature and precipitation. It then aims to use this model to forecast the yield of crops in the future by projecting expected trend in temperature.

## Project outline

-   `analyses/` - contains main analyses work, scripts for modelling, sampling, plotting. Contains all Stan code.
-   `data/` - contains all data used for the project, for raw and derived data.
-   `outputs/` - contains model outputs and plots.
-   `docs/` - contains rendered html webpages

## How to reproduce

To install all required dependencies, in an R terminal run\
`install.packages("renv")` (if renv is not installed on your machine)\
`renv::restore()`\
The project uses Stan software for all of the analyses, so that will need to be installed on your machine. For more information, consult [cmdstan installation](https://mc-stan.org/docs/cmdstan-guide/installation.html)\
To run the full analyses,\
`source("run.R")`

To render the quarto website, run `quarto render` (if quarto is not installed on your machine, run `install.packages("quarto")`). The rendered websites can be accessed in `docs/`.

## License

This project is open source under the MIT License.
