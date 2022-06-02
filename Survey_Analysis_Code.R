


setwd("C:/Users/Shumaila Bhatti/Dropbox/TempProjects/COVID-SLR/data analysis")

d <- read.csv("data.csv")

library("psych")
library("tidyverse")
library("survey")
library("boot")
library("ggplot2")
library("rstatix")
library("ggpubr")

########################################### RECODED ##################################################################

encode_specialization <- function (x) {
  case_when (x %in% c(" ", "Non-Environmental", "Health", "Non-Environmental; Health", "Unknown") ~ 0,
             x %in% c("Environmental") ~ 1,
             x %in% c("Coastal or Hydrologic") ~ 2)}

d$domain <- sapply(d$specialization_coded, encode_specialization)


encode_specialization1 <- function (x) {
  case_when (x %in% c(0) ~ c("Unspecialized"),
             x %in% c(1) ~ c("Generalist"),
             x %in% c(2) ~ c("Specialist"))}

d$domain_codes <- sapply(d$domain, encode_specialization1)

encode_risk_prob <- function (x) {
  case_when (x %in% c("Neither") ~ 2,
             x %in% c("Syracuse, NY", "Both") ~ 1,
             x %in% c("Kingston, RI") ~ 0)}

d$riskprob.coded <- sapply(d$risk.prob1, encode_risk_prob)





factorise_impct <- function (x) {
  case_when (x %in% c("No") ~ 0,
             x %in% c("Someone I know has been impacted") ~ 1,
             x %in% c("A little bit") ~ 2,
             x %in% c("Yes") ~ 3)}

d$impact1.coded <- sapply(d$impact1, factorise_impct)
d$impact2.coded <- sapply(d$impact2, factorise_impct)

d$impact_sum <- rowSums(cbind.data.frame(d$impact1.coded,d$impact2.coded), na.rm = T)

factorise_impct2 <- function (x) {
  case_when (x %in% 0 ~ c("No"),
             x %in% 1 ~ c("Someone I know has been impacted"),
             x %in% 2 ~ c("A little bit"),
             x %in% 3 ~ c("Yes"))}

d$impactsum.coded <- sapply(d$impact_sum, factorise_impct2)



########### knowledge

factorise_slrnow <- function (x) {
  case_when (x %in% c("Between 1-5 feet.","I do not know","Over 5 feet.","Sea level hasn't risen") ~ 0,
             x %in% c("Under 1 foot.") ~1)
}

d$k.cons1.coded <- sapply(d$K.consequence1, factorise_slrnow)

# R4: SLR in next 50years

factorise_slr50yr <- function (x) {
  case_when (x %in% c("Under 1 foot.","I do not know","Sea level won't rise") ~ 0,
             x %in% c("Over 5 feet.","Between 1-5 feet.") ~1)
}

d$k.cons2.coded <- sapply(d$K.consequence2, factorise_slr50yr)

# R5: CC knowledge 

factorise_CC1 <- function (x) {
  case_when (x %in% c("Climate change is occurring and is mostly manmade.","Climate change is occurring and is mostly natural.","Climate change is occurring and is completely natural.","Climate change is occurring and is completely manmade.") ~ 1,
             x %in% c("") ~ 0)
}

d$kcause1.coded <- sapply(d$K.cause1, factorise_CC1)

# R6: SLR_CC

factorise_CC2 <- function (x) {
  case_when (x %in% c("Yes; Climate change is the main reason there is sea level rise.","Yes; Climate change is a contributing factor to sea level rise.")~0,
             x %in% c("Yes; Climate change is the sole reason there is sea level rise.")~1)}

d$kcause2.coded <- sapply(d$K.cause2, factorise_CC2)




# The reason I have agender, gender fluid and others with the females because there were some studies
# where they had a higher risk perception than the males... so maybe we need to take that into 
# consideration and talk about that in our paper in how we are still defining gender in binary terms when
# doing data analysis.
factorise_gender <- function (x) {
  case_when (x %in% c("Male") ~ 0,
             x %in% c("Female","Agender","Gender fluid","Prefer not to say")~ 1 )}

d$gender.coded <- sapply(d$gender, factorise_gender)

refactorise_gender <- function (x) {
  case_when (x %in% 0 ~ c("Male"),
             x %in% 1 ~ c("Female and Other Genders"))}

d$gender.recoded <- sapply(d$gender.coded, refactorise_gender)


encode_region <- function (x) {
  case_when (x %in% c("Non coastal") ~ 0,
             x %in% c("Coastal") ~ 1)}  

d$region_coded <- sapply(d$region, encode_region)


# When we code politics, we always code it as republican vs democrats but there is so much diversity in the
# other categories too. I feel like having politics as a scale rather than a binary duality. Maybe that is how
# scientist may also be contributing to the polarization as we are viewing the political space that way ourselves.
factorise_Politic <- function (x) {
  case_when (x %in% c("Progressive I wish I could say I was a democrat but they're really making it hard (Joe Biden's 1 billion dollar climate change plan is more than Trump but absolutely nothing compared to the Green New Deal we NEED)","Progressive","Socialist","Socialist ","Strong liberal","Communist","Green Party","Progressive leaning democrat ","Strongly progressive (preparing to leave Democratic party after the primary election for various reasons).", "Strong liberal","Democratic socialist ","democratic socialist ","Not so strong Democrat.","democratic socialist","democratic socialist   ", "Independent leaning Democrat."," Not so strong Democrat.","Strong Democrat.","Progressive leaning democrat ","Leftist, registered as a Democrat for convenience ") ~ c("Left-Leaning"),
             x %in% c("Independent leaning Republican.","Not so strong Republican.","Strong Republican","Not so strong Republican.") ~ c("Right-Leaning"),
             x %in% c("libertarian ","Libertarian ","Independent.") ~c("Centrists"),
             x %in% c("Not party affiliated","I do not know.","","I cannot vote") ~ c("Others"))}

d$politics.coded <- sapply(d$politics, factorise_Politic)

d$riskindex <- rowMeans(cbind.data.frame(d$riskprob.coded, d$risk2, d$risk3, d$risk4, d$risk5))

d$wtaindex <- rowMeans(cbind.data.frame(d$wta1, d$wta2, d$wta3, d$wta4, d$wta5, d$wta6))


# 0.65 and some were negatively correlated.
psych::alpha(cbind.data.frame(d$riskprob.coded, d$risk2, d$risk3, d$risk4, d$risk5))

# 0.75
psych::alpha(cbind.data.frame(d$wta1, d$wta2, d$wta3, d$wta4, d$wta5, d$wta6))

encode_concerns <- function (x) {
  case_when (x %in% c("climate change", " climate change ","climate change ", "sea level rise ","Coral bleaching ") ~ 0,
             x %in% c("general environmental issues","natural disaster","pollution", "animal welfare","Unsustainable living",
                      " pollution ","pollution ", "loss of biodiversity","loss of biodiversity ", "natural resource depletion ","natural resource depletion") ~ 1,
             x %in% c("COVID-19","COVID-19 ", "health/disease ", "health/disease") ~ 2,
             x %in% c("economy","economy "," economy ") ~ 3,
             x %in% c("misleading information", " misleading information ", "misleading information ") ~ 4,
             x %in% c("overpopulation ","overpopulation") ~ 5,
             x %in% c("hunger ","hunger") ~ 6,
             x %in% c("unemployment","unemployment ") ~ 7,
             x %in% c("war","war ") ~ 8,
             x %in% c("personal concern") ~ 9,
             x %in% c("politics ","politics","weakening of democracy ") ~ 10,
             x %in% c("social inequality", "social inequality ", "social inequality  ","equality") ~ 11,
             x %in% c("n/a","other","drugs","Immigration","Overconsumption ","Support for minorities ") ~ 12
  )}

d$concern_code1 <- sapply(d$concern1, encode_concerns)
d$concern_code2 <- sapply(d$concern2, encode_concerns)
d$concern_code3 <- sapply(d$concern3, encode_concerns)

d$concernsum <- rowSums(cbind.data.frame(d$concern_code1, d$concern_code2, d$concern_code3))

reencode_concerns <- function (x) {
  case_when (x %in% c("climate change", " climate change ","climate change ", "sea level rise ","Coral bleaching ") ~ c("Climate Change"),
             x %in% c("general environmental issues","natural disaster","pollution", "animal welfare","Unsustainable living",
                      " pollution ","pollution ", "loss of biodiversity","loss of biodiversity ", "natural resource depletion ","natural resource depletion") ~ c("Environment"),
             x %in% c("COVID-19","COVID-19 ", "health/disease ", "health/disease") ~ c("COVID-19"),
             x %in% c("economy","economy "," economy ") ~ c("Economy"),
             x %in% c("misleading information", " misleading information ", "misleading information ") ~ c("Misleading Information"),
             x %in% c("overpopulation ","overpopulation") ~ c("Overpopulation"),
             x %in% c("hunger ","hunger") ~ c("Hunger"),
             x %in% c("unemployment","unemployment ") ~ c("Unemployment"),
             x %in% c("war","war ") ~ c("War"),
             x %in% c("personal concern") ~ c("Personal Concern"),
             x %in% c("politics ","politics","weakening of democracy ") ~ c("Personal Concern"),
             x %in% c("social inequality", "social inequality ", "social inequality  ","equality") ~ c("Social Equality"),
             x %in% c("n/a","other","drugs","Immigration","Overconsumption ","Support for minorities ") ~ c("Others")
  )}

d$concern_recode1 <- sapply(d$concern1, reencode_concerns)
d$concern_recode2 <- sapply(d$concern2, reencode_concerns)
d$concern_recode3 <- sapply(d$concern3, reencode_concerns)

write.csv(d, file =  "recoded_data.csv")



##############################################################################################

d <- read.csv("recoded_data.csv")


# H1: Boxplot domains x risk perception index (others)
#             domains x wta

ggplot(d, aes(y = riskindex, x = domain_codes)) +
  geom_boxplot(notch = FALSE) + 
  scale_fill_brewer(palette="Greys") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", colour = "grey34") + 
  stat_summary(fun = mean, geom = "point", colour = "grey34") +theme_grey()+
  labs(y = "Risk Perception Index", x = "Domain")

stat.test1 <- d %>%
  t_test(riskindex ~ domain) %>%
  adjust_pvalue() %>%
  add_significance()


ggplot(d, aes(y = wtaindex, x = domain_codes)) +
  geom_boxplot(notch = FALSE) + 
  scale_fill_brewer(palette="Greys") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", colour = "grey34") + 
  stat_summary(fun = mean, geom = "point", colour = "grey34") +theme_grey()+
  labs(y = "Wilingness to Act Index", x = "Domain")

stat.test <- d %>%
  t_test(wtaindex ~ domain) %>%
  adjust_pvalue() %>%
  add_significance()

# H2: Boxplot Regions x risk perception index

ggplot(d, aes(y = riskindex, x = region)) +
  geom_boxplot(notch = FALSE) + 
  scale_fill_brewer(palette="Greys") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", colour = "grey34") + 
  stat_summary(fun = mean, geom = "point", colour = "grey34") +theme_grey()+
  labs(y = "Risk Perception Index", x = "Region")

ggplot(d, aes(y = wtaindex, x = region)) +
  geom_boxplot(notch = FALSE) + 
  scale_fill_brewer(palette="Greys") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", colour = "grey34") + 
  stat_summary(fun = mean, geom = "point", colour = "grey34") +theme_grey()+
  labs(y = "Wilingness to Act Index", x = "Region")

# H3: Boxplot Impact x risk perception index

d$impactsum.coded <- factor(d$impactsum.coded, 
                            levels = c("Yes",
                                       "A little bit",
                                       "Someone I know has been impacted",
                                       "No"), 
                            ordered = TRUE)

ggplot(d, aes(y = riskindex, x = impactsum.coded)) +
  geom_boxplot(notch = FALSE) + 
  scale_fill_brewer(palette="Greys") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", colour = "grey34") + 
  stat_summary(fun = mean, geom = "point", colour = "grey34") +theme_grey()+
  labs(y = "Risk Perception Index", x = "Impact Level")

# H4: Boxplot Gender x risk perception index

ggplot(d, aes(y = riskindex, x = gender.recoded)) +
  geom_boxplot(notch = FALSE) + 
  scale_fill_brewer(palette="Greys") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", colour = "grey34") + 
  stat_summary(fun = mean, geom = "point", colour = "grey34") +theme_grey()+
  labs(y = "Risk Perception Index", x = "Gender")


ggplot(d, aes(y = wtaindex, x = gender.recoded)) +
  geom_boxplot(notch = FALSE) + 
  scale_fill_brewer(palette="Greys") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", colour = "grey34") + 
  stat_summary(fun = mean, geom = "point", colour = "grey34") +theme_grey()+
  labs(y = "Wilingness to Act Index", x = "Gender")

# H5: Boxplot Politics x risk perception index

d$politics.coded <- factor(d$politics.coded, 
                           levels = c("Left-Leaning",
                                      "Centrists",
                                      "Right-Leaning",
                                      "Others"), 
                           ordered = TRUE)

ggplot(d, aes(y = riskindex, x = politics.coded)) +
  geom_boxplot(notch = FALSE) + 
  scale_fill_brewer(palette="Greys") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", colour = "grey34") + 
  stat_summary(fun = mean, geom = "point", colour = "grey34") +theme_grey()+
  labs(y = "Risk Perception Index", x = "Political Affiliation")

# H6: Correlation Risk perception index x wta

ggplot(d, aes(y = riskindex, x = wtaindex)) +
  geom_jitter(notch = FALSE) + 
  scale_fill_brewer(palette="Greys") + theme_grey() #+
#stat_summary(fun.data= mean_cl_normal) + 
#geom_smooth(method='lm') +
#labs(y = "Risk Perception Index", x = "Wilingness to Act Index")

model1 <- lm(d$wtaindex ~ d$riskindex)
summary(model1)

# Factor analysis of the concerns

mat <- as.matrix(d[,74:76])

d1 <- d %>% select(domain_codes,concern_recode1,concern_recode2,concern_recode3) %>% gather("concerns","n",-domain_codes)

table(d1$n)

d1$n <- factor(d1$n,levels = c("Climate Change",
                               "COVID-19",
                               "Social Equality",
                               "Environment",
                               "Personal Concern",
                               "Economy",
                               "Others",
                               "Hunger",
                               "War","Unemployment","Misleading Information","Overpopulation"), 
               ordered = TRUE)

ggplot(d1, aes(y = n, fill = domain_codes)) +
  geom_bar(position = "dodge") + 
  labs(y = "Concerns", x = "", )

d2 <- d %>% select(domain,concern_code1,concern_code2,concern_code3) %>% gather("concerns","n",-domain)

CALC_CI <- function(df){
  
  tab.df <- d2 %>% group_by(domain_codes) %>% count(n)
  
  # Selecting the portion of the table where the frames are present
  
  count.df <- as.data.frame(tab.df)
  
  # n (would be an array with four stages/values) which is equal to the total number of articles in that stage
  
  n <- count.df %>% group_by(Var1) %>% summarise(total= sum(Freq))
  
  Gen <- count.df %>%
    filter(Var1 == "Generalist")
  
  Uns <- count.df %>%
    filter(Var1 == "Unspecialized")
  
  Spe <- count.df %>%
    filter(Var1 == "Specialist")
  
  Gen$percent <- Gen$Freq/n$total[1]
  Uns$percent <- Uns$Freq/n$total[3]
  Spe$percent <- Spe$Freq/n$total[2]
  
  out_ci <- function(x, y){
    
    d <- dim(x)
    
    for(i in seq(1:d[1])){
      
      test <- list()
      
      test[[i]] <- prop.test(x$Freq[i], y)
      
      x$LLCI[i] <- test[[i]]$conf.int[1]
      x$ULCI[i] <- test[[i]]$conf.int[2]
      
      
    }
    return(x)}
  
  Gen_ci <- out_ci(Gen, n$total[1])
  Uns_ci <- out_ci(Uns, n$total[3])
  Spe_ci <- out_ci(Spe, n$total[2])
  
  finaloutput <- rbind.data.frame(Gen_ci, Uns_ci, Spe_ci)
  
  return(finaloutput)
}

d1ci <- CALC_CI(d2)

CIplot <- function(df){
  p <-
    ggplot(df, aes(x = Var2, y = percent, fill = Var1)) +
    geom_bar(stat = "identity", position = "dodge") + 
    geom_errorbar(aes(ymin = LLCI, ymax = ULCI), position = position_dodge()) +
    labs(x = "", y = "") +
    scale_y_continuous(labels = scales::percent, limits = c(0,0.55)) +
    theme_classic() +
    scale_fill_brewer(palette ="Spectral", direction = -1) +
    theme() 
  return(p)}

CIplot(d1ci)+ coord_flip()




# Correlation Matrix

mat <- as.matrix(read.csv("mat2.csv"))

mat<- mat[,-c(4:5,7:8,16:20)]

df <- as.data.frame(mat) 

df2 <- df %>% gather("variable", "response")

summary_results <- df2 %>% 
  group_by(variable) %>% 
  summarise(avg = mean(response), 
            sd = sqrt(var(response)), 
            mx = max(response), 
            mn = min(response))

df1 <- df %>% gather("variable", "response", -domain)

summary_results_domain <- df1 %>% 
  group_by(variable, domain) %>% 
  summarise(avg = mean(response), 
            sd = sqrt(var(response)), 
            mx = max(response), 
            mn = min(response))

write.csv(summary_results_domain, file = "domain_results.csv")

chart.Correlation(mat, histogram = TRUE, method = "pearson")

pairs.panels(mat,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "pearson", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

library(corrplot)

# Make a correlation analysis using the mat matrix
fa.cor <- round(cor(mat), digits = 3)
p_mat <- cor.mtest(mat)

# tl.cex changes the label size
# number.cex changes the number size within the cells

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

# One approach would be to color the boxes black and then edit it outside in photoshop
corrplot(fa.cor, method="color", col=col(200),
         type="upper", addCoef.col = "black", addCoefasPercent = TRUE, tl.cex = 0.4, tl.col = "black", number.cex = 0.5,
         p.mat = p_mat$p, sig.level= 0.05, pch = 20, pch.col = "black") # Add coefficient of correlation

# Remove outliers from the riskindex total of 6 removed
outliers <- boxplot(df$riskindex, plot = FALSE)$out
No_out_risk <- df[!(df$riskindex %in% outliers), ]

# using the no_out dataset now remove outliers in wta and there were 3
outliers1 <- boxplot(No_out_risk$wtaindex, plot = FALSE)$out
No_out_wta <- No_out_risk[!(No_out_risk$wtaindex %in% outliers1), ]

#create a regression plot again without outliers
ggplot(No_out_wta, aes(y = riskindex, x = wtaindex)) +
  geom_jitter(notch = FALSE) + 
  scale_fill_brewer(palette="Greys") + theme_grey() +
  stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm') +
  labs(y = "Risk Perception Index", x = "Wilingness to Act Index")

corr.test(No_out_wta$riskindex,No_out_wta$wtaindex)

model1 <- lm(wtaindex ~ riskindex + domain + gender.coded, data = No_out_wta)
summary(model1)
plot(model1)

model2 <- lm(riskindex ~ knowledge + impact_sum + domain + gender.coded, data = No_out_wta)
summary(model2)
plot(model2)

model3 <- lm(riskindex ~ cause_k + consqnc_k + impact_sum + domain + gender.coded, data = No_out_wta)
summary(model3)

model4 <- lm(wtaindex ~ riskindex + cause_k + consqnc_k + impact_sum + domain + gender.coded, data = No_out_wta)
summary(model4)
plot(model4)


# SEM

library(lavaan)
m1 <- '
  # regressions
    wtaindex ~ riskindex + cause_k + consqnc_k + impact_sum + domain + gender.coded
'
fit1 <- sem(m1, data=No_out_wta)
summary(fit1, fit.measure = T)

