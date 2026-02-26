library(readr)
df <- read_csv("AllVariablesSpreadsheet_AllWeeks_filled_updatedJan272026_corrected.csv")
df$Remark_ID <- as.character(df$Remark)
df$week_baseline <- df$`measurement week` - 8 
head(df)
library(ggplot2)

#geompoint by treatment
ggplot(df, aes( x = `measurement week`, y = CO2flux
                ,color = Remark_ID))+
  geom_point()+
  geom_path()+
  facet_wrap(~Treatment)+
  theme_bw()

#geom histogram by remark
ggplot(df, aes( x =  meanGCC))+
geom_histogram()+
  facet_wrap(~Remark_ID)+
  theme_bw()
  #theme(legend.position = 'bottom')

#geom histogram by Treatment
ggplot(df, aes( x =  meanGCC))+
  geom_histogram()+
  facet_wrap(~Treatment)+
  theme_bw()
#theme(legend.position = 'bottom')

  
library(brms)

my_model <- brm( meanGCC ~ week_baseline * Treatment + (1 + week_baseline | Remark_ID),
                 data = df, # linear growth
                 family = gaussian(),
                 chains = 3,
                 cores = 3)  
my_model_polynomial <- brm( meanGCC ~ poly(week_baseline,2) * Treatment + (1 + poly(week_baseline,2) | Remark_ID),
                 data = df, # ppolynomial  growth
                 family = gaussian(),
                 chains = 3,
                 cores = 3) 
prior_canopy <-  prior(normal(25, 3),  class = 'Intercept')
prior_canopy <- c(prior_canopy, prior(normal(5, 3),  class = 'b',coef = 'TreatmentExtended'))
prior_canopy <- c(prior_canopy, prior(normal(2, 2),  class = 'b',coef = 'TreatmentHeatwave'))
prior_canopy <- c(prior_canopy, prior(normal(4, 1),  class = 'sigma'))


my_model_canopy <- brm( canopyheight ~   Treatment + (1  | Remark_ID), # mean
                     data = df[df$week >= 8, ],
                     family = gaussian(),
                     prior = prior_canopy,
                     chains = 3,
                     cores = 3) 

summary(my_model_canopy, prob = 0.9)
# non linear growth
prior_nl <- prior(normal(40, 20), nlpar = "plateau")
my_model_nl <- brm( bf(canopyheight ~ Intercept + (slope * week_baseline) * (1 - (canopyheight/plateau)),
                       slope + Intercept + plateau ~ 1 + Treatment+  (1  | Remark_ID),nl = TRUE),
                 data = df, # non linear growth
                 family = gaussian(),
                 prior = prior_nl,
                 chains = 3,
                 cores = 3)  




plot(my_model_canopy)
pp_check(my_model)
marginal_effects(my_model, type = 'pred', effects = 'week_baseline:Treatment')


plot(my_model)
