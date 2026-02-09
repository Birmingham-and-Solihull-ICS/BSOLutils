library(MASS)
library(tidyverse)
library(DBI)
library(glue)

# Utility functions
disp_ratio <- function(model, ...){
  sum(residuals(model, type="pearson")^2) / df.residual(model)
}


exact_CI <- function(o, n, ci=0.95){
  
  z <- qnorm(ci + ((1-ci)/2))
  
  olower <- (qchisq(ci + ((1-ci)/2), (2*o), lower.tail = FALSE)/2)
  oupper <- (qchisq(1-(ci + ((1-ci)/2)), 2*(o+1), lower.tail = FALSE)/2)
  
  return(data.frame(Rate=o/n, LowerCI=olower/n, UpperCI=oupper/n))
  
  
}


# SQL connection to OF and data pull
sql_connection <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "MLCSU-BI-SQL", 
                            Database = "EAT_Reporting_BSOL", Trusted_Connection = "True")

sql2 <- "Select *
  from [EAT_Reporting_BSOL].[OF].[OF2_Indicator_SQL_Data] T1
  LEFT JOIN [EAT_Reporting_BSOL].[OF].[OF2_Reference_Population] T2
ON T1.aggregation_id = T2.aggregation_id
AND T1.age_group_code = T2.age_code
AND T1.ethnicity_code = T2.ethnicity_code
AND T1.imd_code = T2.imd_code
AND T1.sex_code = T2.sex_code
WHERE T1.indicator_id = 50
and T1.denominator IS NULL 
and aggregation_type = 'ICB (Resident)'
and start_date < convert(datetime, '20250401', 112)
and (age_group_code <15 OR age_group_code = '999')"


stroke <- dbGetQuery(sql_connection, sql2)

stroke <- repair_names(stroke)



stroke2 <- 
  stroke %>% 
  mutate(imd_grp = ifelse(imd_code == 1, 1, 0),
         rt = numerator / denominator) %>% 
  dplyr::select(numerator, denominator = observation, age_group_code, sex_code, imd_grp, rt, start_date)


# manual expected
stroke2_ref <- stroke2 %>% 
  group_by(start_date, age_group_code) %>% 
  summarise(grp_num = sum(numerator),
            grp_denom = sum(denominator),
            grp_rt = sum(numerator)/sum(denominator)) %>% 
  as.data.frame()
# Join back on 
stroke2 <- 
  stroke2 %>% 
  left_join(stroke2_ref) %>% 
  mutate(manual_expected = grp_rt * denominator)

# manual plot


stroke2 %>%
  group_by(imd_grp, start_date) %>%
  summarise(
    num = sum(numerator),
    denom = sum(manual_expected),
    exact_CI(o = num, n = denom)[1],
    exact_CI(o = num, n = denom)[2],
    exact_CI(o = num, n = denom)[3]
  ) %>%
  mutate(
    fiscal_year = glue(
      "{ifelse(month(start_date) >= 4, year(start_date), year(start_date) - 1)}/",
      "{ifelse(month(start_date) >= 4, substr(year(start_date) + 1, 3, 4), substr(year(start_date), 3, 4))}"),
    imd_grp2 = factor(imd_grp, levels = c(1,0), labels = c("Q1", "Q2-Q5"))
  ) %>%
  ggplot(aes(
    y = Rate,
    x = fiscal_year,
    colour = factor(imd_grp2),
    group = factor(imd_grp2)
  )) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  #geom_line(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, size = 2, linetype = "dashed", colour = "#7570B3") +
  scale_colour_brewer(palette = "Dark2") +
  labs(colour = "Deprivation Qunitile",
       x = "Fiscal Year",
       y = "Indirectly Standardised Ratio",
       title = "Age-standardised Admission Ratio: Stroke (Under 75yrs)",
       subtitle = "Birmingham and Solihull ICS") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45),
        plot.subtitle = element_text(face = "italic"),
        legend.position = "inside", legend.position.inside = c(0.85,0.95),
        legend.background = element_rect(colour = "#FFF", color = "#000"))



############################################################################
# regression-based

stroke2$age_group_code <- factor(stroke2$age_group_code)
stroke2$start_date <- factor(stroke2$start_date)

stroke_model1 <- glm.nb(numerator ~ 1 +
                      #imd_grp +
                      age_group_code +
                      start_date +
                      offset(log(denominator))
                    , data=stroke2
                    , control = glm.control(maxit = 50)
)
#                , family="poisson")

summary(stroke_model1)
exp(coef(stroke_model1))
exp(confint(stroke_model1))

disp_ratio(stroke_model1)


models <- stroke2 %>%
  group_by(start_date) %>%
  do(model = glm(numerator ~ 1 + age_group_code+ offset(log(denominator)) , data=.
                 , family = "poisson"   
                 , control = glm.control(maxit = 200)))

# Predict using the corresponding model for each group
predictions <- stroke2 %>%
  group_by(start_date) %>%
  nest() %>%
  left_join(models, by = "start_date") %>%
  mutate(pred = map2(data, model, ~predict(.y, newdata = .x, type = "response"))) %>%
  unnest(cols = c(data, pred))



stroke2$preds <- predict(stroke_model1, newdata = stroke2, type = "response")

stroke2 %>% 
  summarise(sum(numerator) / sum(denominator))


stroke2 %>% 
  group_by(start_date, imd_grp) %>% 
  summarise(sum(numerator) / sum(preds))


stroke2 %>%
  group_by(imd_grp, start_date) %>%
  summarise(
    num = sum(numerator),
    denom = sum(preds),
    exact_CI(o = num, n = denom)[1],
    exact_CI(o = num, n = denom)[2],
    exact_CI(o = num, n = denom)[3]
  ) %>%
  mutate(
    fiscal_year = glue(
      "{ifelse(month(start_date) >= 4, year(start_date), year(start_date) - 1)}/",
      "{ifelse(month(start_date) >= 4, substr(year(start_date) + 1, 3, 4), substr(year(start_date), 3, 4))}"),
    imd_grp2 = factor(imd_grp, levels = c(1,0), labels = c("Q1", "Q2-Q5"))
  ) %>%
  ggplot(aes(
    y = Rate,
    x = fiscal_year,
    colour = factor(imd_grp2),
    group = factor(imd_grp2)
  )) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  #geom_line(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, size = 2, linetype = "dashed", colour = "#7570B3") +
  scale_colour_brewer(palette = "Accent") +
  labs(colour = "Deprivation Qunitile",
       x = "Fiscal Year",
       y = "Indirectly Standardised Ratio (regression)",
       title = "Age-standardised Admission Ratio: Stroke (Under 75yrs)",
       subtitle = "Birmingham and Solihull ICS") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45),
        plot.subtitle = element_text(face = "italic"),
        legend.position = "inside", legend.position.inside = c(0.85,0.95),
        legend.background = element_rect(colour = "#FFF", color = "#000"))


# and with grouped data


predictions %>%
  group_by(imd_grp, start_date) %>%
  summarise(
    num = sum(numerator),
    denom = sum(pred),
    exact_CI(o = num, n = denom)[1],
    exact_CI(o = num, n = denom)[2],
    exact_CI(o = num, n = denom)[3]
  ) %>%
  mutate(
    fiscal_year = glue(
      "{ifelse(month(start_date) >= 4, year(start_date), year(start_date) - 1)}/",
      "{ifelse(month(start_date) >= 4, substr(year(start_date) + 1, 3, 4), substr(year(start_date), 3, 4))}"),
    imd_grp2 = factor(imd_grp, levels = c(1,0), labels = c("Q1", "Q2-Q5"))
  ) %>%
  ggplot(aes(
    y = Rate,
    x = fiscal_year,
    colour = factor(imd_grp2),
    group = factor(imd_grp2)
  )) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  #geom_line(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, size = 2, linetype = "dashed", colour = "#7570B3") +
  scale_colour_brewer(palette = "Accent") +
  labs(colour = "Deprivation Qunitile",
       x = "Fiscal Year",
       y = "Indirectly Standardised Ratio (regression)",
       title = "Age-standardised Admission Ratio: Stroke (Under 75yrs)",
       subtitle = "Birmingham and Solihull ICS") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45),
        plot.subtitle = element_text(face = "italic"),
        legend.position = "inside", legend.position.inside = c(0.85,0.95),
        legend.background = element_rect(colour = "#FFF", color = "#000"))



stroke2 %>% 
  group_by(start_date, age_group_code) %>% 
  summarise(sum(numerator),
            sum(denominator)) %>% 
  as.data.frame()

############################################################
# PHEIndicatormethods

library(PHEindicatormethods)

phe <- calculate_ISRatio(stroke2, x=numerator, n=denominator, x_ref = grp_num, n_ref  = grp_denom,
                         refpoptype = "field")

phe


# Create grouped summary
a <- stroke2 %>% 
  group_by(start_date, imd_grp) %>% 
  summarise(n = sum(denominator), x = sum(numerator), 
            x_ref = sum(grp_num), n_ref = sum(grp_denom),  .groups = "drop")



b <- cbind(a[0,], na.omit(calculate_ISRatio(a[0,], x, n = n
                                            , x_ref = x_ref
                                            , n_ref = n_ref
                                            , refpoptype = "field"))
)
stroke2$x <- stroke2$numerator
stroke2$n <- stroke2$denominator
stroke2$x_ref <- stroke2$grp_num
stroke2$n_ref <- stroke2$grp_denom

for (i in 1:nrow(a)) {
  dt_sub <- a[i,]
  mi_sub <- subset(stroke2, start_date == dt_sub$start_date & imd_grp == dt_sub$imd_grp)
  tmp <-calculate_ISRatio(mi_sub, x = x, n =n, x_ref = x_ref
                          , n_ref = n_ref, refpoptype = "field")
  
  b<-rbind(b, cbind(dt_sub, tmp))
}



b %>%
  # group_by(imd_grp, start_date) %>%
  # summarise(
  #   Rate = sum(x)/sum(n),
  #   num = sum(x),
  #   denom = sum(n),
  #   exact_CI(o = x, n = n)[1],
  #   exact_CI(o = x, n = n)[2],
  #   exact_CI(o = x, n = n)[3]
  # ) %>%
  mutate(
    fiscal_year = glue(
      "{ifelse(month(start_date) >= 4, year(start_date), year(start_date) - 1)}/",
      "{ifelse(month(start_date) >= 4, substr(year(start_date) + 1, 3, 4), substr(year(start_date), 3, 4))}"),
    imd_grp2 = factor(imd_grp, levels = c(1,0), labels = c("Q1", "Q2-Q5"))
  ) %>%
  ggplot(aes(
    y = value,
    x = fiscal_year,
    colour = factor(imd_grp2),
    group = factor(imd_grp2)
  )) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  #geom_line(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lowercl, ymax = uppercl), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, size = 2, linetype = "dashed", colour = "#7570B3") +
  scale_colour_brewer(palette = "Dark2") +
  labs(colour = "Deprivation Qunitile",
       x = "Fiscal Year",
       y = "Indirectly Standardised Ratio (PHE)",
       title = "Age-standardised Admission Ratio: Stroke (Under 75yrs)",
       subtitle = "Birmingham and Solihull ICS") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45),
        plot.subtitle = element_text(face = "italic"),
        legend.position = "inside", legend.position.inside = c(0.85,0.95),
        legend.background = element_rect(colour = "#FFF", color = "#000"))


#######################################################################

# Using latest data, split IMD out from 1

max(stroke$start_date)

stroke3 <- 
  stroke %>% 
  filter(start_date == max(stroke$start_date)) %>% 
  mutate(imd_grp = ifelse(imd_code == 1, 1, 0),
         rt = numerator / denominator) %>% 
  dplyr::select(numerator, denominator = observation, age_group_code, sex_code
                , imd_code, imd_grp, rt, start_date)


stroke3$age_group_code_f <- factor(stroke3$age_group_code)

stroke3$imd_code_f <- factor(stroke3$imd_code, levels= c("1","2","3","4","5","999")
                         , labels = c("1","2","3","4","5","999"))

unique(stroke3$imd_code)

stroke_model3 <- glm.nb(numerator ~ 1 +
                   imd_code_f +
                   age_group_code_f +
                   #start_date +
                   offset(log(denominator))
                 , data = stroke3
                 #, family = "poisson"
                 , control = glm.control(maxit = 200)
)
#                , family="poisson")

summary(stroke_model3)
exp(coef(stroke_model3))
exp(confint(stroke_model3))

disp_ratio(stroke_model3)

out_coefs <-
  data.frame(
    imd_quintile = c("2","3","4","5","999"),
    ratio = exp((coef(stroke_model3)[2:6])),
    lowerCI = exp(confint(stroke_model3)[2:6,1]),
    upperCI = exp(confint(stroke_model3)[2:6,2])
  )


ggplot(out_coefs, aes(x=imd_quintile, y = ratio, fill=imd_quintile))+
  geom_col(show.legend = FALSE, alpha = 0.8) +
  geom_errorbar(aes(ymin=lowerCI, ymax=upperCI, width = 0.5))+
  geom_hline(yintercept = 1, linetype = "dashed", col = "red", size = 1) +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(breaks=seq(0,1.1,0.1))+
  labs(colour = "Deprivation Qunitile",
       x = "IMD Quintile (999 = 'Unknown')",
       y = "Indirectly Standardised Ratio",
       title = "Age-standardised Admission Ratio: Stroke (Under 75yrs) - 2024/25",
       subtitle = "Birmingham and Solihull ICS \n A ratio of 1 represents the most deprivated: Quintile 1") +
  theme_minimal() +
  theme(plot.subtitle = element_text(face = "italic")
  )
