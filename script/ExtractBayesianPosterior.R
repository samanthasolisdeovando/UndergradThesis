######## Written by Jeremy Borderieux ########
##### Edited by Samantha Solis de Ovando #####

#### Packages ####
library(brms)
library(data.table)
library(dplyr)

#### getting the posterior distribution ####

# The models we got are a lot of parameters adding to each other to predict the mean 
# of a given treatment/season, we can use those coeficients to assess significances of the treatment,
# but another way is to use the model to predict the distributino of the mean of a given treatment, or difference 
# between a treatment a the control, and do stats of that distribution (e.g. 90% credible interval)

# I'm using the data.table syntaxe, I'm sorry, that's the only one I know
dataframe_treatment <- data.table(expand.grid(Treatment = unique(thesis_data$Treatment),Season=unique(thesis_data$Season)))

# current model
model_current <- model_cover ## change here the model to run this script

# the fitted function can be used to retrieved the predicted mean and CI of the mean of each treatment:season
# replace with the desired model
mean_effect <- fitted(model_current,summary = T,# summary = T means we got a mean and CI, not the full model
                      probs = c(0.05,0.95), # width of the credible interval
                      re_formula = NA, # ingnore the random PotID effect to get the mean prediction
                      newdata = dataframe_treatment) ## feeding a dataframe with every class of treatment and season
mean_effect <- cbind(dataframe_treatment,mean_effect)

## this is the mean and the CI of the mean of that variable,
mean_effect

ggplot(mean_effect,aes(x = Treatment, y = Estimate, ymin = Q5,ymax = Q95,color = Treatment))+
  geom_pointrange()+
  facet_grid(~factor(Season, levels=c('Summer', 'Heatwave', 'Autumn warm', 'Autumn cold')))+
  theme_classic()+
  scale_color_manual(values = c("Control" = "#1F77B4",
                               "Heatwave" = "#FF7F0E",
                               "Extended" = "#2CA02C"))


## interesting,but we can also compute how that is different from the control plot
full_model <- fitted(model_current,summary = F,# summary = F we get the full psoterior distribution, and play with it
                      re_formula = NA, # ingnore the random PotID effect to get the mean prediction
                      newdata = dataframe_treatment) ## feeding a dataframe with every class of treatment and season
full_model <- cbind(dataframe_treatment,t(full_model))
full_model <- melt(full_model,id.vars = c("Treatment","Season"))

## this model contains of all the simulation of the posterio, we can now do treatment - control to see how much more/less 
## the variable compared to the treatment
full_model[,diff_from_control := value - value[Treatment == "Control"] ,by = .(Season,variable)] # create a now col containing the difference
full_model_diff <- full_model[Treatment != "Control",] #we don't need the control anymore

summary_full_model <- full_model_diff[,.(mean_diff = mean(diff_from_control),
              Q5_diff = quantile(diff_from_control,prob = 0.05),
              Q95_diff = quantile(diff_from_control,prob = 0.95),
              prob_to_be_higher = mean(diff_from_control>0),
              prob_to_be_smaller = mean(diff_from_control<0)),by = .(Treatment,Season)]

## compute the cutoff, but watch out sometime things are almost signif but still worth discussing
summary_full_model[,signif := case_when(
  prob_to_be_higher  > 0.999 ~ "***",
  prob_to_be_higher  > 0.99 ~ "**",
  prob_to_be_higher  > 0.95 ~ "*",
  prob_to_be_higher  > 0.9 ~ "+",
  .default =NA)]
summary_full_model[,signif_smaller := case_when(
  prob_to_be_smaller  > 0.999 ~ "***",
  prob_to_be_smaller  > 0.99 ~ "**",
  prob_to_be_smaller  > 0.95 ~ "*",
  prob_to_be_smaller  > 0.9 ~ "+",
  .default = NA)]

## as expected, the difference of Heatwave during Heatwave is significant in the CO2 model
summary_full_model


## now let's plot our results

#### making a plot ####
# I will propose two way to plot the model, you can pick and chose based on what makes more sense

color_vector <- c("Control" = "#1F77B4","Heatwave" = "#FF7F0E",  "Extended" = "#2CA02C")

# plot 1: displaying the data and the estimate of the mean by the model
(plot_to_export <- ggplot(data = full_model ,aes(x = Treatment, y = value,fill =Treatment,color = Treatment ))+
  geom_violin(alpha = 0.75,show.legend = T,trim = T)+ ## full prediction
  geom_pointrange(data = mean_effect,aes( y = Estimate, ymin = Q5,ymax = Q95),
                  color = "white",size = 0.55,lwd = 1 ,show.legend = F,lineend = "round")+ ## mean prediction
  geom_point(data = thesis_data,aes ( y = Cover),## change here which variable you are predicting
             color = "grey20",alpha = 1,size = 0.65,position = position_jitter(height = 0,width = 0.2),show.legend = F)+  ## real data
  geom_text(aes(y = max (full_model$value), ## change here too
                label = signif),summary_full_model,color = "grey5",size = 9)+
 geom_text(aes(y = max (full_model$value), ## change here too
                  label = signif_smaller),summary_full_model,color = "grey5",size = 9)+
  facet_grid(~factor(Season, levels=c('Summer', 'Heatwave', 'Autumn warm', 'Autumn cold'), labels = c("Summer", "Late summer", "Autumn warm", "Autumn cold")))+
  theme_classic()+
    theme_classic()+
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = 26),
        axis.title.y = element_text(size = 20))+
  labs( x = NULL, 
        y = "Cover (0-1)")+ ## fitting name here
  scale_x_discrete(label = c("Ext","Heat","Ctrl"))+ ## shortening the name, keep it or not
  scale_fill_manual(values =color_vector)+
  scale_color_manual(values = color_vector))

# plot 2: displayed the distribution of the posterior of the difference with the control
(plot_to_export2 <- ggplot(data = full_model_diff ,aes(x = diff_from_control,fill = Treatment,color = Treatment ))+
  geom_density(alpha = 0.75,trim = T)+
  facet_grid(Treatment ~factor(Season, levels=c('Summer', 'Heatwave', 'Autumn warm', 'Autumn cold')))+
  facet_grid(factor(Season, levels=c('Summer', 'Heatwave', 'Autumn warm', 'Autumn cold'), labels = c("Summer", "Late summer", "Autumn warm", "Autumn cold")) ~ Treatment)+
  geom_vline(xintercept = 0,lty = 2)+
  theme_classic()+
  scale_fill_manual(values = color_vector)+
  scale_color_manual(values = color_vector)+
  labs(y = "Density distribution", x = bquote("Posterior distribution of the difference in " ~ CO[2] ~ " respiration between control and treatment pots")))

## save plot1 in publ format for paper pdf
ggsave(file.path("figures","bayesian_results","CO2_PaperFormat.pdf"),plot_to_export,
       width = 180,height = 130,unit= "mm",dpi = 400)
## save the plot in pbl format for paper jpg
ggsave(file.path("figures","bayesian_results","CO2_PaperFormatBigText.jpg"),plot_to_export,
       width = 120,height = 130,unit= "mm",dpi = 400)

## save plot 2 in publ format for paper
ggsave(file.path("figures","bayesian_results","CO2_PDist_PaperFormat_.pdf"),plot_to_export2,
       width = 180,height = 130,unit= "mm",dpi = 400)
## save the plot in pbl format for paper jpg
ggsave(file.path("figures","bayesian_results","CO2_PDist_PaperFormat.jpg"),plot_to_export2,
       width = 180,height = 130,unit= "mm",dpi = 400)

## saving plot 1 in publ format (for presentation)
ggsave(file.path("figures","bayesian_results","Cover_wide_bayes.jpg"),plot_to_export,
       width = 180,height = 100,unit= "mm",dpi = 400)

ggsave(file.path("figures","bayesian_results","Cover_bayes.pdf"),plot_to_export,
       width = 180,height = 140,unit= "mm",dpi = 400)

#### model summaries (for reporting / stats table) ####
## use this to report your number in the result section
mean_effect

summary_full_model
