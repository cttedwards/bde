bde
===

Model-based bycatch and discard estimation for New Zealand fisheries.

The `bde` package is designed to facilitate estimation of bycatch and discards in New Zealand fisheries using a two-part statistical model. The model is fitted to observer sampling data using the Bayesian `rstan` estimation framework. Estimated parameters are then used to predict the catch for unobserved commercial fishing effort.

The required `rstan` package can be installed using `install.packages("rstan")`.

