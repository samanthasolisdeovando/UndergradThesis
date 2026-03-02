# load in regression functions ----
## simple linear regression function ----
runlm <- function(dataset, response, predictor) {
  lmlist <- list(model = list(), summary = list()) # list of all models
  sig_lms <- list(model = list(), summary = list()) # list of significant models
  response_var <- deparse(substitute(response)) # convert response variable into usable format
  
  for (i in 1:length(predictor)) {
    pred <- predictor[i] # get the specific predictor from the list of predictors
    
    formula <- as.formula(paste(response_var, "~", pred))
    lm_model <- lm(formula, data = dataset) #fit the LM model
    lm_sum <- summary(lm_model) # get the summary 
    
    lmlist$model[[pred]] <- lm_model # append the model
    lmlist$summary[[pred]] <- lm_sum # append the summary 
    
    coef <- lm_sum$coefficients
    print(coef)
    
    pval <- coef[2,4]
    
    if (!is.na(pval) && pval < 0.05) { # append only significant models in this list 
      sig_lms$model[[pred]] <- lm_model # append the model
      sig_lms$summary[[pred]] <- lm_sum # append the summary 
    }
  }
  
  models <- c(all_models = lmlist,
              sig_models = sig_lms)
  
  return(models)
}

## mixed linear regression function ----
runmlm <- function(dataset, response, predictor, random_effect) {
  lmlist <- list(model = list(), summary = list()) 
  sig_lms <- list(model = list(), summary = list())
  
  response_var <- deparse(substitute(response))
  
  for (pred in predictor) {
    # Build formula
    formula <- as.formula(
      paste0(response_var, " ~ ", pred, " + (1|", random_effect, ")")
    )
    
    # Filter out groups with <=1 obs
    df_filtered <- dataset %>%
      group_by(across(all_of(random_effect))) %>%
      filter(n() > 1) %>%
      ungroup()
    
    # Skip if less than 2 levels remain
    if (n_distinct(df_filtered[[random_effect]]) < 2) {
      message("Skipping predictor '", pred, "' because grouping factor has <2 levels after filtering")
      next
    }
    
    # Fit model
    lmm_model <- lmer(formula, data = df_filtered)
    lmm_sum <- summary(lmm_model)
    
    # Store all models
    lmlist$model[[pred]] <- lmm_model
    lmlist$summary[[pred]] <- lmm_sum
    
    # Extract p-value of the predictor
    pval <- lmm_sum$coefficients[2, "Pr(>|t|)"]
    
    # Store significant models
    if (!is.na(pval) && pval < 0.05) {
      sig_lms$model[[pred]] <- lmm_model
      sig_lms$summary[[pred]] <- lmm_sum
    }
  }
  
  list(all_models = lmlist, sig_models = sig_lms)
}

## model fitness plot function ----
modelfit <- function(model, xvar = NULL, main_label = "Model") {
  par(mfrow = c(2, 2), mai = c(0.6, 0.6, 0.6, 0.6), cex = 0.8)
  
  # Residual Plot
  plot(model$fitted.values,
       model$residuals,
       main = paste(main_label, "- Residual Plot"),
       xlab = "Fitted Values (ŷ)",
       ylab = "Residuals")
  abline(h = 0, col = "red")
  
  # Fitted Line Plot (requires x-variable)
  if (!is.null(xvar)) {
    plot(xvar,
         model$fitted.values,
         main = paste(main_label, "- Fitted Line Plot"),
         xlab = "x",
         ylab = "Fitted Values (ŷ)")
    
    abline(model, col = "red", lwd = 2)
  } else {
    plot(model$fitted.values,
         main = paste(main_label, "- Fitted Values"),
         ylab = "Fitted Values (ŷ)",
         xlab = "")
  }
  
  # Normal Q–Q Plot
  qqnorm(model$residuals,
         main = paste(main_label, "- Normal Q-Q Plot"))
  qqline(model$residuals, col = "red")
  
  # Histogram of Residuals
  hist(model$residuals,
       breaks = 8,
       density = 10,
       col = "green",
       border = "black",
       main = paste(main_label, "- Residual Distribution"))
  
  # Reset plotting parameters
  par(mfrow = c(1, 1), mai = c(1, 1, 1, 1), cex = 1)
}

## anova ----
runaov <- function(dataset, response, predictor) {
  aovlist <- list(model = list(), summary = list()) # list of all models
  sig_aov <- list(model = list(), summary = list()) # list of significant models
  response_var <- deparse(substitute(response)) # convert response variable into usable format
  
  for (i in 1:length(predictor)) {
    pred <- predictor[i] # get the specific predictor from the list of predictors
    
    formula <- as.formula(paste(response_var, "~", pred))
    aov_model <- aov(formula, data = dataset) # fit the AOV model
    aov_sum <- summary(aov_model) # get the summary 
    
    aovlist$model[[pred]] <- aov_model # append the model
    aovlist$summary[[pred]] <- aov_sum # append the summary 
    
    coef <- aov_sum$coefficients
    print(coef)
    
    pval <- coef[2,4]
    
    if (!is.na(pval) && pval < 0.05) { # append only significant models in this list 
      sig_aov$model[[pred]] <- aov_model # append the model
      sig_aov$summary[[pred]] <- aov_sum # append the summary 
    }
  }
  
  models <- c(all_models = aovlist,
              sig_models = sig_aov)
  
  return(models)
}
