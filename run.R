# run file to reproduce all the simulations and plots
require(here)

# 100-eda ======================================================================

source(here("src", "100_get_data.R"))
source(here("analyses", "100-eda", "100-eda.R"))

# 200-model-fit ================================================================

source(here("analyses", "200-model-fit", "fit_model.R"))
source(here("analyses", "200-model-fit", "posterior_check.R"))
source(here("analyses", "200-model-fit", "marginal_effects.R"))

# 200-model-fit ================================================================

source(here("analyses", "300-forecast", "temp_forecast.R"))
source(here("analyses", "300-forecast", "yield_forecast.R"))
