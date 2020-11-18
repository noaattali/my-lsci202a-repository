
## Exercise 2

# Clear the environment
rm(list=ls())

#getwd() # this is the project address where the files are saved with ggsave
# to set the file path for saving these files, open this file as part of a project,
# use setwd(), or specify the full path in each ggsave

# load libraries
library(tidyverse)
library(modelr)

####### 1.1 Linear Regression ############

# load data
d <- read_tsv("http://socsci.uci.edu/~rfutrell/teaching/MWPND_by_picture.txt")

# 1. Similar to last time, create a visualization showing 
# the relationship between RT_M (mean RT) and name_H (the H-statistic) 
# within each language. You may use scatterplots, 2D densities, etc., 
# as you see fit. You should have one facet per language. Save the 
# visualization in a file called H_stat.pdf

d %>% 
  ggplot(aes(x = name_H, y = RT_M)) +
  geom_density_2d() +
  facet_wrap(~lang, nrow = 2) +
  labs(x="H statistic", 
       y = "Mean RT",
       title="Mean RT and H-Statistic per Language")+
  theme_bw() +
  ggsave("H_stat.pdf")


# 2. Fit a linear regression to predict RT_M as a function of name_H. 
# Call the model RT_model

RT_model <- lm(RT_M ~ name_H, data=d)

# Plot the regression line from RT model on top of a scatterplot of RT_M 
# as a function of name_H for each language. Note that your model only 
# gives you one regression line, even though the best fit to each individual 
# language might be different. Does it look like there are some languages 
# where the RTs are systematically over- or under-estimated by the regression? 
# Save this visualization in a file called H_stat_regression.pdf

d %>%
  add_predictions(RT_model) %>%
  ggplot(aes(x = name_H, y = RT_M)) +
  geom_density_2d() +
  labs(x="H statistic", 
       y = "Mean RT",
       title="RT model predictions")+
  facet_wrap(~lang, nrow = 2) +
  theme_bw() +
  geom_line(aes(y=pred), 
            color="red",
            size = 1) +
  ggsave("H_stat_regression.pdf")

# Bulgarian, Russian, and German are underestimated
# DE is very badly overestimated and Dutch, English and Finnish 
# are also overestimated.
# For the other languages, just by looking at it, it seems like slopes
# might be slightly off but the overall fit seems good

# 4. Plot the model residuals in one facet per language. 
# Do you notice any patterns comparing languages? Do these 
# line up with what you noticed in the previous question? 
# Save this visualization in a file called H_stat_residuals.pdf

d %>%
  add_residuals(RT_model) %>%
  ggplot(aes(x = name_H, resid)) +
  geom_point(alpha=1/8) +
  facet_wrap(~lang, nrow=2) +
  labs(x="H statistic", 
       y = "Mean RT",
       title="RT model residuals")+
  theme_bw() +
  ggsave("H_stat_residuals.pdf")

# The relationships between name_H and mean RT still appear in the
# residual plot for the languages that were badly over- or underestimated 
# by the RT_model; for example, German still shows a strong relationship in
# the residual plot, with nearly all the points above zero, 
# and German is a language that is underestimated by the model. This indicates
# that the model fails to account for a lot of the variance between these
# variables for nearly half the languages.


# 5. Make another regression model where the intercept of the 
# regression line depends on the language, but the slope of the 
# line is the same across languages. Plot the predictions and 
# residuals of the model. Save the visualizations in files 
# called H_stat_regression_by_lang.pdf and H_stat_residuals_by_lang.pdf.

RT_by_lang_model <- lm(RT_M ~ name_H + lang, data=d)

d %>%
  add_predictions(RT_by_lang_model) %>%
  ggplot(aes(x = name_H, y = RT_M)) +
  geom_density_2d() +
  facet_wrap(~lang, nrow=2) +
  theme_bw() +
  labs(x="H statistic", 
       y = "Mean RT",
       title="RT by Lang predictions") +
  geom_line(aes(y=pred), 
            color="red",
            size = 1) +
  ggsave("H_stat_regression_by_lang.pdf")

d %>%
  add_residuals(RT_by_lang_model) %>%
  ggplot(aes(x = name_H, resid)) +
  geom_point(alpha=1/8) +
  labs(x="H statistic", 
       y = "Mean RT",
       title="RT by Lang residuals") +
  facet_wrap(~lang, nrow=2) +
  theme_bw()+
  ggsave("H_stat_residuals_by_lang.pdf")


# 6. Make another regression model where both the intercept and the slope
# of the line depend on the language. Save visualizations in files called
# H_stat_regression_by_lang_slope.pdf and H_stat_residuals_by_lang_slope.pdf.

RT_by_lang_slope_model <- lm(RT_M ~ name_H*lang, data=d)

d %>%
  add_predictions(RT_by_lang_slope_model) %>%
  ggplot(aes(x = name_H, y = RT_M)) +
  geom_density_2d() +
  facet_wrap(~lang, nrow=2) +
  labs(x="H statistic", 
       y = "Mean RT",
       title="RT by Lang slope predictions") +
  theme_bw() +
  geom_line(aes(y=pred), 
            color="red",
            size = 1) +
  ggsave("H_stat_regression_by_lang_slope.pdf")

d %>%
  add_residuals(RT_by_lang_slope_model) %>%
  ggplot(aes(x = name_H, resid)) +
  geom_point(alpha=1/8) +
  facet_wrap(~lang, nrow=2) +
  labs(x="H statistic", 
       y = "Mean RT",
       title="RT by Lang slope residuals") +
  theme_bw() +
  ggsave("H_stat_residuals_by_lang_slope.pdf")


# 7. Save a dataframe including predictions and residuals from the part above
# in a file called full_predictions.csv.

d %>%
  add_predictions(RT_by_lang_slope_model) %>%
  add_residuals(RT_by_lang_slope_model) %>%
  write_csv("full_predictions.csv")


######## 1.2 Logistic Regression ##########

# Clear the environment
rm(list=ls())

# load data
url <- "https://tinyurl.com/y5fgh9mk"
d <- read_csv(url)

# 1. Based on the original dataframe d, create a new dataframe where
# (a) The variables Theme.animacy and Recipient.animacy are replaced
# with dichotomous variables, with value animate if the original value
# is A, and inanimate otherwise.
# (b) The variables Theme.definiteness and Recipient.definiteness
# are replaced with dichotomous variables, with value D if the original
# value is Definite or Definite-pn, and I otherwise

d2 <- d %>%
  mutate(Theme.animacy = if_else(Theme.animacy == "A",
                                "animate","inanimate"),
         Recipient.animacy = if_else(Recipient.animacy == "A",
                                    "animate","inanimate"),
         Theme.definiteness = if_else(Theme.definiteness == "Definite" | Theme.definiteness == "Definite-pn",
                                     "D", "I"),
         Recipient.definiteness = if_else(Recipient.definiteness == "Definite" | Recipient.definiteness == "Definite-pn",
                                         "D", "I"))

# 2. Create a bar plot showing the proportion of Response.variable 
# realized as D or P as a function of animacy. Use facet wrap so that one
# facet has Recipient.animacy and the other facet has Theme.animacy.
# Hint: You will have to use gather. Save this bar plot in a file called
# animacy_bars.pdf

d2 %>%
  gather(key = "Role", value = "Animacy", 
         "Theme.animacy","Recipient.animacy") -> d3

d3 %>%
  mutate(Role = if_else(Role == "Theme.animacy","Theme","Recipient")) -> d4

d4 %>%
  ggplot(aes(fill = Response.variable, 
             x = Animacy)) +
  geom_bar(position="fill") +
  facet_wrap(~Role) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(legend.position="bottom") +
  labs(x="Animacy", 
       y = "Proportion",
       title="Animacy by Role") +
  ggsave("animacy_bars.pdf")


# 3. Do the same for Recipient.definiteness and Theme.definiteness.
# Save the result in a file called definiteness_bars.pdf

d2 %>%
  gather(key = "Role", value = "Definiteness", 
         "Theme.definiteness","Recipient.definiteness") %>%
  mutate(Role = if_else(Role == "Theme.definiteness","Theme","Recipient")) %>%
  ggplot(aes(fill = Response.variable, x = Definiteness)) +
  geom_bar(position="fill") +
  facet_wrap(~Role) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(legend.position="bottom") +
  labs(x="Definiteness", 
       y = "Proportion",
       title="Definiteness by Role") +
  ggsave("definiteness_bars.pdf")

# 4. What generalizations do you notice about the effects 
# of animacy and definiteness on the dative alternation?

# The girl gave the snack to the cat (P - both definite, animate recipient)
# The girl gave a snack to a cat (P - both indefinite)
# The girl gave a pet to a cat (P - both animate)
# The girl gave the cat the snack (D)

# Double object constructions are preferred overall, but the strength
# of preference is opposite for the definiteness of recipient v. theme.
# If the recipient is definite ("the cat"), double object is more preferred than if
# the recipient is indefinite ("a cat"), and vice versa for theme: double object
# constructions are more preferred for indefinite ("a snack") than definite ("the snack") themes.

# there could even be an interaction between definiteness, the dative alternation,
# and role, because people are nearly at chance in producing the preposition vs.
# the double object construction when the recipient is indefinite, which is
# a greater likelihood of producing P than in any other combination of variables.

# We see the same relationship, but even stronger, with animacy.
# Double object constructions are more preferred for inanimate rather than
# animate themes and for animate rather than inanimate recipients.
# However, the nearly at chance preference for producing P has gone away
# for inanimate recipients.





# 5. Fit a logistic regression to predict whether people will use 
# the doubleobject dative (Response.variable = D) as a function of 
# Recipient.animacy, Recipient.definiteness, Theme.animacy, and Theme.definiteness.


logistic = function(x) {
  1 / (1 + exp(-x))
}


d2 %>%
  gather(key = "Role", value = "Definiteness", 
         "Theme.definiteness","Recipient.definiteness") %>%
  mutate(Role = if_else(Role == "Theme.definiteness","Theme","Recipient")) %>%
  gather(key = "Role", value = "Animacy", 
         "Theme.animacy","Recipient.animacy") %>%
  mutate(Role = if_else(Role == "Theme.animacy","Theme","Recipient"), 
         Response = if_else(Response.variable == "D", 1,0)) -> d5

#Three way interaction model
log_model_int <- glm(Response ~ Role*Definiteness*Animacy,
                     data = d5,
                     family = "binomial")
summary(log_model_int)

#Additive model - Main effects
log_model_add <- glm(Response ~ Role+Definiteness+Animacy,
                     data = d5,
                     family = "binomial")
summary(log_model_add)

#Combination model - Interaction of Role and Animacy with the additive effect of Definitiveness
log_model_int_add <- glm(Response ~ Role*Animacy+Definiteness,
                         data = d5,
                         family = "binomial")
summary(log_model_int_add)

#Model Comparison
model_comparison <- anova(log_model_add,log_model_int,log_model_int_add,test = "LR")

#Given model_comparison identifies log_model_int as the best fitting model
#this is the model we will use for all plots.

d5 %>%
  add_predictions(log_model_int) %>%
  add_residuals(log_model_int) %>%
  mutate(predicted = logistic(pred))-> d6

# 6. Generate plots as in parts 2 and 3, except showing the 
# predictions of the logistic regression instead of the empirical 
# proportions. You don't need to save these plots.

#Plots for the Three way interaction model
#Empricial probabilites calculated according to model specification: 
#Role*Animacy*Definiteness


d6 %>%
  group_by(Definiteness, Role, Animacy) %>%
  summarize(response_prop = sum(Response)/n()) %>%
  ungroup() -> d66

d6 %>%
  left_join(d66, by = c("Definiteness","Role","Animacy")) -> d7
  
# Response.variable, pred -> prediction/human, value of that
d7 %>%
  gather(key = "Type", value = "Response", 
         "response_prop","predicted") -> d8

#Plot the Predicted values next to the empirical values for comparison

#Role/Animacy/Definiteness
d8 %>%
  ggplot(aes(y = Response, x= Animacy,fill=Type)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(Definiteness ~ Role)+
  labs(y="Proportion of Double Object Responses", 
       fill="Predicted and Empirical \n           proportions") +
  theme_bw()

#Role/Animacy
d8 %>%
  #filter(Type == "response_prop_animacy"| Type =="predicted")%>%
  ggplot(aes(y = Response, x= Animacy,fill=Type)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(. ~ Role)+
  labs(y="Proportion of Double Object Responses", 
       fill="Predicted and Empirical \n           proportions") +
  theme_bw()

#Role/Definiteness
d8 %>%
  #filter(Type == "response_prop_def"| Type =="predicted")%>%
  ggplot(aes(y = Response, x= Definiteness, fill=Type)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(. ~ Role)+
  labs(y="Proportion of Double Object Responses", fill="Predicted and Empirical \n           proportions") +
  theme_bw()
  

# 7. How do the logistic regression fits compare to the empirical
# proportions? Does it look like there is something missing
# from the regression?

#Here, since we calculated the empirical proportions the same way as the model does
#the model predictions are exactly the same as the empirical proportions. The logistic
#regression predictions fit the empirical proportions very well. There doesn't appear
#to be anything missing.








#### Appendix ####

# 6. Generate plots as in parts 2 and 3, except showing the 
# predictions of the logistic regression instead of the empirical 
# proportions. You don't need to save these plots.

#### Calculating empirical probabilites per pair: Role/Animacy, Role/Definiteness ####


d6 %>%
  group_by(Role, Animacy) %>%
  summarize(response_prop_animacy = sum(Response)/n()) %>%
  ungroup() -> d_r_a

d6 %>%
  group_by(Role, Definiteness) %>%
  summarize(response_prop_def = sum(Response)/n()) %>%
  ungroup() -> d_r_d

d_r_a %>%
  left_join(d_r_d, by="Role") -> d_r_a_d

d6 %>%
  left_join(d_r_a_d, by = c("Definiteness","Role","Animacy")) -> d7_r_a_d


# Response.variable, pred -> prediction/human, value of that
d7_r_a_d %>%
  gather(key = "Type", value = "Response", 
         "response_prop_def","response_prop_animacy","predicted") -> d8_r_a_d

#Role/Animacy/Definiteness
d8_r_a_d %>%
  ggplot(aes(y = Response, x= Animacy, fill=Type)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(Definiteness ~ Role)+
  labs(y="Proportion of Double Object Responses", fill="Predicted and Empirical \n           proportions") +
  theme_bw()

#Role/Animacy
d8_r_a_d %>%
  filter(Type == "response_prop_animacy"| Type =="predicted")%>%
  ggplot(aes(y = Response, x= Animacy, fill=Type)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(. ~ Role)+
  labs(y="Proportion of Double Object Responses", fill="Predicted and Empirical \n           proportions") +
  theme_bw()

#Role/Definiteness
d8_r_a_d %>%
  filter(Type == "response_prop_def"| Type =="predicted")%>%
  ggplot(aes(y = Response, x= Definiteness, fill=Type)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(. ~ Role)+
  labs(y="Proportion of Double Object Responses", fill="Predicted and Empirical \n           proportions") +
  theme_bw()

# 7. How do the logistic regression fits compare to the empirical 
# proportions? Does it look like there is something missing 
# from the regression?

#### Calculating probabilites per pair: Role/Animacy, Role/Definiteness version 2 ####

#Role/Animacy
d6 %>%
  group_by(Role, Animacy) %>% #Group by Role and Animacy
  summarize(response_prop_animacy = sum(Response)/n()) %>% #Calculate empirical prop
  ungroup() -> d_R_A

d6 %>%
  left_join(d_R_A, by = c("Role","Animacy")) -> d7_R_A #Merge the two data frames

# Response.variable, pred -> prediction/human, value of that
d7_R_A %>%
  gather(key = "Type", value = "Response", 
         "response_prop_animacy","predicted") -> d8_R_A # Merge the pred and emp values into 1 column


#Plot the Predicited values next to the emprical values for comparison
d8_R_A %>%
  ggplot(aes(y = Response, x= Animacy, fill=Type)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(. ~ Role) +
  labs(y="Proportion of Double Object Responses", fill="Predicted and Empirical \n           proportions") +
  theme_bw()


#Role/Definiteness

d6 %>%
  group_by(Role, Definiteness) %>% #Group by Role and Definiteness
  summarize(response_prop_def = sum(Response)/n()) %>% # Calculate empirical prop
  ungroup() -> d_R_D

d6 %>%
  left_join(d_R_D, by = c("Definiteness","Role")) -> d7_R_D #Merge the two data frames

# Response.variable, pred -> prediction/human, value of that
d7_R_D %>%
  gather(key = "Type", value = "Response", 
         "response_prop_def","predicted") -> d8_R_D # Merge the pred and emp values into 1 column

#Plot the Predicited values next to the emprical values for comparison
d8_R_D %>%
  ggplot(aes(y = Response, x= Definiteness, fill=Type)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(. ~ Role) +
  labs(y="Proportion of Double Object Responses", fill="Predicted and Empirical \n           proportions") +
  theme_bw()

# 7. How do the logistic regression fits compare to the empirical 
# proportions? Does it look like there is something missing 
# from the regression?


##### Plots for the Combination model - Interaction of Role and Animacy with the additive effect of Definitiveness ####
d5 %>%
  add_predictions(log_model_int_add) %>%
  add_residuals(log_model_int_add) -> d_int_add

d_int_add %>%
  ggplot(aes(y = logistic(pred), x=Animacy)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(Definiteness ~ Role)+
  #facet_grid(. ~ Role)+
  #scale_y_continuous(labels = scales::percent) +
  theme_bw()
#  theme(legend.position = "right")

d_int_add %>%
  ggplot(aes(y = logistic(pred), x= Definiteness)) +
  geom_bar(stat="identity", position = "dodge") +
  #facet_grid(Definiteness ~ Role)+
  facet_grid(. ~ Role)+
  #scale_y_continuous(labels = scales::percent) +
  #theme_bw()+
  theme(legend.position = "right")

d_int_add %>%
  ggplot(aes(y = logistic(pred), x= Animacy)) +
  geom_bar(stat="identity", position = "dodge") +
  #facet_grid(Definiteness ~ Role)+
  facet_grid(. ~ Role)+
  #scale_y_continuous(labels = scales::percent) +
  #theme_bw()+
  theme(legend.position = "right")