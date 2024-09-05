# BaBayesian-Regression-Models
#climate change impacts on Migration timing and egg laying date 
library(brms)
library(rstan)

formula <-saDOY ~ lat + lon + sdDOY + artemps1 + deptemps1 +
  depsnows1 + tema + snowa + s1du + km

# Define normal priors for the coefficients and intercept
priors <- c(
  prior(normal(0, 10), class = "b"),
  prior(normal(0, 10), class = "Intercept")
)

# Fit the model using the brm function
model <- brm(
  formula = saDOY ~ lat + lon + sdDOY + artemps1 + deptemps1 + depsnows1 + tema + snowa + s1du + km,
  data = data,
  family = gaussian(),  # Assuming 'saDOY' is a continuous variable
  prior = priors,
  warmup = 4000,  # Number of warmup iterations
  chains = 4,  # Number of MCMC chains
  iter = 8000,  # Total number of iterations including warmup
  control = list(adapt_delta = 0.95)  # Adjust for convergence issues
)




# Define normal priors for the coefficients
prior1 <- prior(normal(0, 10), class = "b")

# Define normal priors for the intercept
prior2 <- prior(normal(0, 10), class = "Intercept")

# Define normal priors for the random effects
prior3 <- prior(normal(0, 5), class = "sd")

# Fit the model
model <- brm(
  formula = saDOY ~ lat + lon + sdDOY + artemps1 + deptemps1 + depsnows1 + tema + snowa + s1du + km,
  data = data,
  family = gaussian(),  # Assuming your response variable is continuous
  prior = c(prior1, prior2, prior3),
  warmup= 4000,
  chains = 4,  # Number of MCMC chains
  iter = 8000,  # Number of MCMC iterations per chain
  control = list(adapt_delta = 0.99)  # Increase adapt_delta to handle potential convergence issues
)



model <- brm(
  formula = doy ~ age + lat + long + sex + KGCC + (1 | loc),
  data = data, prior = priors, family = gaussian(),
  warmup= 4000, iter= 8000, chains= 4,
  control = list(adapt_delta = 0.95)) # Increase max_treedepth

model1 <- brm(
  formula = doy ~ age + (1 | loc),
  data = data, prior = priors , family = gaussian(),
  warmup= 4000, iter= 8000, chains= 4,
  control = list(adapt_delta = 0.99, max_treedepth = 15))

model2 <- brm(
  formula = doy ~ age + KGCC + (1 | loc),
  data = data, prior = priors, family = gaussian(),
  warmup= 4000, iter= 8000, chains= 4,
  control = list(adapt_delta = 0.99, max_treedepth = 20))

model3 <- brm(
  formula = doy ~ age + lat +KGCC + (1 | loc),
  data = data, prior = priors, family = gaussian(),
  warmup= 4000, iter= 8000, chains= 4,
  control = list(adapt_delta = 0.995, max_treedepth = 20))

model4 <- brm(
  formula = doy ~ age + lat +KGCC + sex+(1 | loc),
  data = data, prior = priors, family = gaussian(),
  warmup= 4000, iter= 8000, chains= 4,
  control = list(adapt_delta = 0.995, max_treedepth = 25))

model5 <- brm(
  formula = doy ~ age + lat +KGCC + long+(1 | loc),
  data = data, prior = priors, family = gaussian(),
  warmup= 4000, iter= 8000, chains= 4,
  control = list(adapt_delta = 0.995, max_treedepth = 25))

loo <- loo(model)
loo1 <- loo(model1)
loo2 <- loo(model2)
loo3 <- loo(model3)
loo4 <- loo(model4)
loo5 <- loo(model5)

loo_compare(loo,loo1, loo2, loo3,loo4,loo5)

bf12 <- bayes_factor(model1, model2)
bf13 <- bayes_factor(model1, model3)

pp_check(model)

summary(model2)
