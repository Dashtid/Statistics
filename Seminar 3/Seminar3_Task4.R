# --- Preparation --- #
library(mle4)
library(jtools)
library(lmerTest)
library(ggplot2)
library(ggeffects)

# Setting Working Directory
setwd("D:/Skola/KTH/CM2009 - Statistics in Medical Engineering/Git/Data")

# Reading in required libraries
oncdata = read.csv(file = "oncdata.csv", head = TRUE , sep=",")
attach(oncdata)

# Disease stage:    Stage 3 = 0 & Stage 4 = 1
# Treatment group:  Control = 0 & Treatment = 1

# ----------- Task 4 ----------- #


# Creating model of how TumourSize depens on Months passed
basic.lm <- lm(TumourSize ~ Months, data = oncdata)
summary(basic.lm)

# Plotting said model
(prelim_plot <- ggplot(oncdata, aes(x = Months, y = TumourSize)) +
    geom_point() +
    geom_smooth(method = "lm"))

# Plotting the residuals
plot(basic.lm, which = 1)

# Showing that both Stage and Treatment
boxplot(TumourSize ~ Stage , data = oncdata)
boxplot(TumourSize ~ Treatment, data = oncdata)

# Showing tha subject is an independent variable which we need to control for
boxplot(TumourSize ~ Subject, data = oncdata)

# Mixed-effects model
fit1_model = lmer(TumourSize ~ 1 + Months + Treatment + ( 1 | Stage/Subject)  , data = oncdata)
fit2_model = lmer(TumourSize ~  1 + Months + Treatment + Stage + ( 1 | Subject)  , data = oncdata)
fit2_model_reduced = lmer(TumourSize ~  1 + Months + Treatment + (1 | Subject)  , data = oncdata)

# --- SUMMARIES --- #

# Model 1
summary(fit1_model) # default lme4
summ(fit1_model)    # jtools
ranova(fit1_model)  # lmerTest

# Model 2
summary(fit2_model) # default lme4
summ(fit2_model)    # jtools
ranova(fit2_model)  # lmerTest

# Model 2 reduced
summary(fit2_model_reduced) # default lme4
summ(fit2_model_reduced)    # jtools
ranova(fit2_model_reduced)  # lmerTest

anova(fit1_model, fit2_model, fit2_model_reduced)  


# ------ TOGETHER ------ #

# Model 1
(fit1_plot <- ggplot(oncdata, aes(x = Months, y = TumourSize)) +
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(oncdata, pred = predict(fit1_model)),
              aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

# Model 2
(fit2_plot <- ggplot(oncdata, aes(x = Months, y = TumourSize)) +
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(oncdata, pred = predict(fit2_model)),
              aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

# Model 2 reduced
(fit2_reduced_plot <- ggplot(oncdata, aes(x = Months, y = TumourSize)) +
        geom_point(alpha = 0.5) +
        theme_classic() +
        geom_line(data = cbind(oncdata, pred = predict(fit2_model_reduced)),
                  aes(y = pred), size = 1) +  # adding predicted line from mixed model 
        theme(legend.position = "none",
              panel.spacing = unit(2, "lines"))  # adding space between panels
)

# ---- FACET_WRAP ---- #

# Model 1
(fit1_plot <- ggplot(oncdata, aes(x = Months, y = TumourSize, colour = Stage)) +
    facet_wrap(~Treatment, nrow=2) +   # a panel for Stage of Cancer
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(oncdata, pred = predict(fit1_model)),
              aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

# Model 2
(fit2_plot <- ggplot(oncdata, aes(x = Months, y = TumourSize, colour = Stage)) +
    facet_wrap(~Treatment, nrow=2) +   # a panel for Stage of Cancer
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(oncdata, pred = predict(fit2_model)),
              aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

# Model 2 reduced
(fit2_plot <- ggplot(oncdata, aes(x = Months, y = TumourSize, colour = Stage)) +
        facet_wrap(~Treatment, nrow=2) +   # a panel for Stage of Cancer
        geom_point(alpha = 0.5) +
        theme_classic() +
        geom_line(data = cbind(oncdata, pred = predict(fit2_model_reduced)),
                  aes(y = pred), size = 1) +  # adding predicted line from mixed model 
        theme(legend.position = "none",
              panel.spacing = unit(2, "lines"))  # adding space between panels
)


# --- PLotting predictions --- #

# Extract the prediction data frame
pred_fit2 = ggpredict(fit2_model, terms = c("Months"))  # this gives overall predictions for the model


# Plot the predictions 

(ggplot(pred_fit2) + 
        geom_line(aes(x = x, y = predicted)) +          # slope
        geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                    fill = "lightgrey", alpha = 0.5) +  # error band
        geom_point(data = oncdata,                      # adding the raw data 
                   aes(x = Months, y = TumourSize, colour = Stage)) + 
        labs(x = "Months passed", y = "Tumours Size", 
             title = "lorem ipsum...") +
        theme()
)
summary(pred_fit2)
summary(fit2_model)
