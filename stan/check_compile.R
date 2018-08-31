
library(rstan)
rstan_options(auto_write = TRUE)

models <- c("Y000_00", "YA00_00", "YA00_D0", "YA00_0I", "YA00_DI", "YAM0_00", "YAM0_D0", "YAM0_0I", "YAM0_DI", "YAMC_00", "YAMC_D0", "YAMC_0I", "YAMC_DI")

for (model in paste0(models, ".stan"))
	tryCatch(stan_model(file = model), error = function(e) print(paste(model, "failed")))
	
if (geterrmessage() == "") {
    for (model in paste0(models, ".rds"))
        file.remove(model)
}
