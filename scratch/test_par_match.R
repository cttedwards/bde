

sim_names <- c("sigma[1]", "sigma[2]", "sigma[3]", "sigma_z")

# regular expression match
# parameter matches strings that finish in "[" or end
sim_names[regexpr("sigma(\\[|$)", sim_names) > 0]


pars <- "sigma"
sim_names[grep(paste0(pars, "(\\[|$)"), sim_names)]

pars <- "sigma_z"
sim_names[grep(paste0(pars, "(\\[|$)"), sim_names)]
