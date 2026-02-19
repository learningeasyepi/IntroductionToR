library(lme4)
library(tidyverse)
library(MASS)

##Load data
data <- readRDS("Lesson3/regression_data.RDS")
summary(data)

##Simple linear regression
linear_model <- lm(weight ~ age, data = data)
linear_model <- glm(weight ~ age, data = data, family = "gaussian")
summary(linear_model)
coef(linear_model)
confint(linear_model)

##Logistic regression
logistic_model <- glm(gastro ~ age, data = data, family = "binomial")
summary(logistic_model)
exp(coef(logistic_model))
exp(confint(logistic_model))

##Poisson regression
poisson_model <- glm(dr_visits ~ age, data = data, family = "poisson")
summary(poisson_model)
exp(coef(poisson_model))
exp(confint(poisson_model))

##negative binomial regression
negbin_model <- glm.nb(hosp_visits ~ age, data = data)
summary(negbin_model)
exp(coef(negbin_model))
exp(confint(negbin_model))
exp(confint(negbin_model, level = 0.9))


###Linear GLMM with sleepstudy data
###Random intercept + slope
data(sleepstudy)

sleepstudy %>%
  ggplot()+
  geom_line(aes(x = Days, y = Reaction, color = Subject))+
  theme_bw()

###Model with no random effects
fe_model  <- lm(Reaction ~ Days, data = sleepstudy)

###Random intercept only (every participant has their own baseline reaction time at 0 days
ri_model  <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)

###Random intercept + random slope (the relationship between Days and reaction varies by subject)
ri_rs_model <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)

###What do our models look like?
df <- crossing(Days = seq(0,9),
              Subject = unique(sleepstudy$Subject))

df$fixed_effects <- predict(fe_model, newdata = df)
df$random_slope <- predict(ri_model, newdata = df)
df$random_int <- predict(ri_rs_model, newdata = df)

df%>%
ggplot()+
  geom_line(aes(x = Days, y = fixed_effects, color = Subject)) +
  theme_bw()

df %>%
ggplot()+
  geom_line(aes(x = Days, y = random_slope, color = Subject))+
  theme_bw()

df %>%
  ggplot()+
  geom_line(aes(x = Days, y = random_int, color = Subject))+
  theme_bw()


confint(fe_model)
confint(ri_model)
confint(ri_rs_model)


AIC(fe_model, ri_model, ri_rs_model)
