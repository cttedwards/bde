
library(rstan)
rstan_options(auto_write = TRUE)

models <- c(
"Y000_000",
"Y000_00H",  
"YA00_00H", 
"YA00_D0H", 
"YA00_0IH", 
"YA00_DIH",
"YA00_000",
"YA00_D00", 
"YA00_0I0", 
"YA00_DI0",
"YAM0_00H", 
"YAM0_D0H", 
"YAM0_0IH", 
"YAM0_DIH", 
"YAMC_00H", 
"YAMC_D0H", 
"YAMC_0IH", 
"YAMC_DIH")

for (model in paste0(models, ".stan"))
	tryCatch(stan_model(file = model), error = function(e) print(paste(model, "failed")))
	
if (geterrmessage() == "") {
    for (model in paste0(models, ".rds"))
        file.remove(model)
}
