library(tidyverse)
library(wesanderson)
library(ggforce)
library(ggdist)
library(gghalves)
library(ggbeeswarm)
library(rstatix)
library(corrplot)


#set working directory
setwd("/Users/ferenchonbolygo/Downloads/spmf_dys_con/")

# DATA MANIPULATIONS ------------------------------------------------------------

#read file
df <- read.table('mmn_area.txt')
mmn <- df

#read file with cons2 that has a different time window
df2 <- read.table('earlymmn_area.txt')
mmn2 <- df2

#select only FCz chanel
mmn <-df %>% select (FCz.std_avg, FCz.cons1_avg, FCz.cons2_avg, FCz.vowel1_avg, FCz.vowel2_avg, FCz.stress_avg)

#select single variable from df2 and rename
mmn2 <-df2 %>% select (FCz.std_avg, FCz.cons2_avg)
lookup <- c(FCz.std_avg2 = "FCz.std_avg", FCz.cons2_avg2 = "FCz.cons2_avg")
mmn2 <- rename (mmn2, all_of(lookup))

#combine the two dfs
mmn3 <- merge(mmn,mmn2, by=0)

#add grouping variable

mmn_contr <- mmn3 %>% slice (1:28)
mmn_contr <- mmn_contr %>% mutate(Group = 'contr')

mmn_dys <- mmn3 %>% slice (29:69)
mmn_dys <- mmn_dys %>% mutate(Group = 'dys')

mmn_full <- bind_rows(mmn_contr, mmn_dys)

#calculate diff values
mmn_full <- mmn_full %>% mutate(cons1_diff = FCz.cons1_avg - FCz.std_avg)
mmn_full <- mmn_full %>% mutate(cons2_diff = FCz.cons2_avg2 - FCz.std_avg2)
mmn_full <- mmn_full %>% mutate(vowel1_diff = FCz.vowel1_avg - FCz.std_avg)
mmn_full <- mmn_full %>% mutate(vowel2_diff = FCz.vowel2_avg - FCz.std_avg)
mmn_full <- mmn_full %>% mutate(stress_diff = FCz.stress_avg - FCz.std_avg)

#outliers
Q <- quantile(mmn_full$vowel1_diff, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(mmn_full$vowel1_diff)
mmn_elim<- subset(mmn_full, mmn_full$vowel1_diff > (Q[1] - 1.5*iqr) & mmn_full$vowel1_diff < (Q[2]+1.5*iqr))

Q <- quantile(mmn_elim$vowel2_diff, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(mmn_elim$vowel2_diff)
mmn_elim<- subset(mmn_elim, mmn_elim$vowel2_diff > (Q[1] - 1.5*iqr) & mmn_elim$vowel2_diff < (Q[2]+1.5*iqr))

# DATA ANALYSIS ------------------------------------------------------------

#summarize data
group_by(mmn_full, Group) %>%
  summarise(
    count = n(),
    mean = mean(cons1_diff, na.rm = TRUE),
    sd = sd(cons1_diff, na.rm = TRUE)
  )

#visualize the data
p <- ggplot(mmn_elim, aes(x=Group, y=vowel2_diff, fill = Group)) + 
  geom_boxplot()+
  labs(title="Consonant 1", x="Group", y = "MMN amplitude (\u03BCV)")+
  scale_fill_manual(values=wes_palette(2, name="Royal1"))+
  theme_classic()
p


#raincloud plots with stats

# Compute the t-test with rstatix
ttest <- t_test(stress_diff ~ Group, data = mmn_full)

# Summary of the analysis
#summary(ttest)
get_test_label(ttest, detailed = TRUE, type = "text")

myplot1<- ggplot(mmn_full, aes(x=Group, y=stress_diff, fill = Group)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = .1, outlier.shape = NA) +
  gghalves::geom_half_point(aes(color=Group), side = "l", range_scale = .4, alpha = .5)+
  scale_y_reverse()+
  #coord_flip()
  scale_color_manual(values=wes_palette(name="GrandBudapest1"))+
  scale_fill_manual(values=wes_palette(name="GrandBudapest1"))+
  labs(title="Stress", x="Group", y = "MMN amplitude (\u03BCV)", subtitle = get_test_label(ttest, detailed = TRUE))+
  theme_classic()+
  theme(legend.position = "none")

jpeg("plot_stress.jpg", width=2000, height= 2000, res=300)
print(myplot1)  
dev.off() 





#export data
write.csv(mmn_full, "/Users/ferenchonbolygo/Downloads/spmf_dys_con/mmn_full.csv", row.names=FALSE)


# CORRELATIONS ------------------------------------------------------------

#read file
df <- read.csv('mmn_dys_con_with_behav2.csv')
mmn_corr <- df
mmn_corr <- rename (mmn_corr, std_PA_scale = St_FonTud_Sk)

corr_data %>% cor_test(std_reading_scale, cons1_diff, method = "pearson")
corr_results <- corr_data %>% cor_test(std_reading_scale, cons1, method = "pearson")
report_label <- paste("r = ", corr_results$cor, ", p = ", corr_results$p, sep="")

myplot2 <- ggplot(mmn_corr, aes(x = std_reading_scale , y = cons1_diff, color = Group)) +
  geom_point()+
  scale_y_reverse() +
  geom_smooth(method = lm,  aes (fill=Group)) + 
  labs(title="Consonant1", x="Reading", y = "MMN amplitude (\u03BCV)", subtitle = report_label)+
  scale_color_manual(values=wes_palette(name="GrandBudapest1"))+
  scale_fill_manual(values=wes_palette(name="GrandBudapest1"))+
  theme_classic()


jpeg("corr_plot_reading_vow2.jpg", width=2000, height= 2000, res=300)
print(myplot2)  
dev.off() 

ggplot(mmn_corr, aes(x = vowel2_diff, y = cons1_diff)) +
  geom_point()+
  scale_y_reverse() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) + 
  labs(title="Stress", x="WM", y = "MMN amplitude (\u03BCV)")+
  scale_color_manual(values=wes_palette(name="GrandBudapest1"))+
  scale_fill_manual(values=wes_palette(name="GrandBudapest1"))+
  theme_classic()

ggplot(mmn_corr, aes(x = St_FonTud_Sk, y = stress_diff, color = stress_diff < -1.5))+
  geom_point()+
  scale_y_reverse() +
  stat_ellipse()

#correlation analysis with rstatix
corr_data <- mmn_corr %>% 
  select(cons1_diff, cons2_diff, vowel1_diff, vowel2_diff, stress_diff, std_reading_scale, std_spelling_scale, std_RAN_scale, St_FonTud_Sk, std_lettersound_scale, std_RAN_scale, WM_standard_score)
corr_data <- rename (corr_data, std_PA_scale = St_FonTud_Sk)
corr_data %>% cor_test(method = "pearson")  

cor.mat <- corr_data %>% cor_mat()  
cor.mat %>% cor_get_pval()
cor.mat %>%
  cor_as_symbols() %>%
  pull_lower_triangle()
cor.mat %>%
  cor_mark_significant()  
cor.mat %>%
  #cor_reorder() %>%
  pull_lower_triangle() %>% 
  cor_plot()

#correlation analysis with corrplot
M = cor(corr_data, use="pairwise.complete.obs")
testRes = cor.mtest(corr_data, conf.level = 0.95)
corrplot(M, method = 'number')
corrplot(M, method = 'color', order = 'alphabet')
corrplot(M, method = 'square', order = 'hclust', type = 'lower', diag = FALSE)
corrplot(M, p.mat = testRes$p, method = 'square', type = 'upper', insig='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'AOE', diag=FALSE, tl.col = 'black')
