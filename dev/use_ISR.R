source("utils.R")
library(MASS)

MI_data

stroke_data


ISR_dperivation <-
  function(.dt, age=TRUE, sex=FALSE, )
MI3 <- 
  MI %>% 
  filter(start_date == max(MI$start_date)) %>% 
  mutate(imd_grp = ifelse(imd_code == 1, 1, 0),
         rt = numerator / denominator) %>% 
  dplyr::select(numerator, denominator = observation, age_group_code, sex_code
                , imd_code, imd_grp, rt, start_date)


MI3$age_group_code_f <- factor(MI3$age_group_code)

MI3$imd_code_f <- factor(MI3$imd_code, levels= c("1","2","3","4","5","999")
                         , labels = c("1","2","3","4","5","999"))

unique(MI3$imd_code)

MI_model3 <- glm.nb(numerator ~ 1 +
                      imd_code_f +
                      age_group_code_f +
                      #start_date +
                      offset(log(denominator))
                    , data = MI3
                    #, family = "poisson"
                    #, control = glm.control(maxit = 50)
)
#                , family="poisson")

summary(MI_model3)
exp(coef(MI_model3))
exp(confint(MI_model3))

disp_ratio(MI_model3)

out_coefs <-
  data.frame(
    imd_quintile = c("2","3","4","5","999"),
    ratio = exp((coef(MI_model3)[2:6])),
    lowerCI = exp(confint(MI_model3)[2:6,1]),
    upperCI = exp(confint(MI_model3)[2:6,2])
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
       title = "Age-standardised Admission Ratio Ratios: Myocardial Infarction (Under 75yrs) - 2024/25",
       subtitle = "A ratio of 1 means the rate = the rate in Quintile 1") +
  theme_minimal() +
  theme(plot.subtitle = element_text(face = "italic")
  )
