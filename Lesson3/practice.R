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


###Confounding, multivariate models, and interaction
###We have a new dataset with variables for health condition (severe, mild/moderate), 
##bedsores (yes/no), and mortality (yes/no)

df <- readRDS("Lesson3/simpsons.RDS")
##Are bedsores associated with mortality?

prop.table(table(df$bedsores, df$mortality),1)
prop.table(table(df$bedsores, df$mortality),2)

s1 <- glm(mortality ~ bedsores, data = df, family = binomial)
exp(confint(s1))

##Is condition associated with mortality?
s2 <- glm(mortality ~  condition, data = df, family = binomial)
exp(confint(s2))

##Is condition associated with bedsores?
s3 <- glm(bedsores_num ~ condition, data =df, family = binomial)
exp(confint(s3))

##What happens when we allow the relationship between bedsores and mortality
##to change by condition?
s4 <- glm(mortality ~  condition * bedsores, data = df, family = binomial)
summary(s4)

AIC(s2,s4)
anova(s2,s4)

tapply( df$mortality,list(df$bedsores), mean)
tapply( df$mortality,list(df$bedsores,df$condition), mean)

df %>%
  group_by(bedsores) %>%
  summarise(mortality = mean(mortality)) %>%
  ggplot()+
  geom_bar(aes(x = bedsores, y = mortality),
           stat = "identity")+
  theme_bw() +
  labs(x = "Bedsores", y = "Mortality rate")

df %>%
  group_by(condition) %>%
  summarise(bedsores = mean(bedsores_num)) %>%
  ggplot()+
  geom_bar(aes(x = condition, y = bedsores),
           stat = "identity")+
  theme_bw() 

df %>%
  group_by(bedsores, condition) %>%
  summarise(mortality = mean(mortality)) %>%
  ggplot()+
  geom_bar(aes(x = condition, y = mortality, fill = bedsores),
           stat = "identity", position = position_dodge())+
  theme_bw() +
  scale_fill_manual(values = c("darkblue","gold2")) +
  labs(x = "Severity", y = "Mortality rate",
       fill = "Bedsores",
       title = "Bedsores and mortality confounded by illness severity")


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

