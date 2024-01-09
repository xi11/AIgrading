###To reproduce figures
setwd('/Users/xpan/Documents/manuscript/NatureCanRebuttal/reproduceFig')
library("survival")
library("survminer")
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(ggplot2)
library(irr)
library('car')
library("caret") #confusionMatrix
library(tidyr)
library(survC1) #computing c-index

####TRACERx 421#####
#all cases: 206
tx421_LUAD <- read.csv('/Users/xpan/Documents/manuscript/NatureCanRebuttal/tx421LUADAIclinical.csv')
#fig2c
cm <- confusionMatrix(factor(tx421_LUAD$predom_ai), factor(tx421_LUAD$predom_path), dnn = c("Prediction", "Reference"))
plt<-as.data.frame(round(prop.table(cm$table, margin=2)*100,1))
plt$Prediction <- factor(plt$Prediction, levels=rev(c('lep', 'pap', 'aci', 'cri', 'mic', 'sol')))
plt$Reference <- factor(plt$Reference, levels=c('lep', 'pap', 'aci', 'cri', 'mic', 'sol'))
pdf(file="./fig2c_tx421.pdf", height = 4, width = 5.6, pointsize = 5)
ggplot(plt, aes(x=Reference, y=Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(x=Reference, y=Prediction, label=Freq)) +
  scale_fill_gradient(low="white", high="#7570B3") +
  theme_minimal()+
  theme(axis.ticks.y=element_blank(),
        #text=element_text(family="sans", size = 5),
        #axis.text.x = element_text(size = 5),
        #axis.text.y = element_text(size = 5)
  )+
  labs(x = "Reference-Path",y = "Prediction-Automated") +
  scale_x_discrete(labels=c("lep","pap","aci","cri","mic","sol")) +
  scale_y_discrete(labels=rev(c("lep","pap","aci","cri","mic","sol")))
dev.off()

#fig2e
cm <- confusionMatrix(factor(tx421_LUAD$IASLC_grade_ai), factor(tx421_LUAD$IASLC_grade_path), dnn = c("Prediction", "Reference"))
plt<-as.data.frame(round(prop.table(cm$table, margin=2)*100,1))
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
pdf(file="./fig2e_tx421.pdf", height = 4, width = 5.6, pointsize = 5)
ggplot(plt, aes(x=Reference, y=Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(x=Reference, y=Prediction, label=Freq)) +
  scale_fill_gradient(low="white", high="#7570B3") +
  theme_minimal()+
  theme(axis.ticks.y=element_blank(),
        text=element_text(family="sans", size = 5),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5))+
  labs(x = "Reference-Path",y = "Prediction-Automated") +
  scale_x_discrete(labels=c("Grade1","Grade2","Grade3")) +
  scale_y_discrete(labels=c("Grade3","Grade2","Grade1")) 
dev.off()

#fig2d
tx421_LUAD$agreement <- NULL
tx421_LUAD$agreement[tx421_LUAD$predom_ai == tx421_LUAD$predom_path] <- 'in Agreement'
tx421_LUAD$agreement[tx421_LUAD$predom_ai != tx421_LUAD$predom_path] <- 'Discrepant'
comList <- list( c("in Agreement", "Discrepant"))
pdf(file="./fig2dtx421.pdf", height = 4, width = 3)
  
dev.off()
wilcox.test(shannon_path ~ agreement, data=tx421_LUAD) #8.467e-07

#fig3a
fit <- survfit(Surv(time_DFS, DFS)~IASLC_grade_ai, data = tx421_LUAD)
pdf(file="./fig3a_tx421.pdf", height = 5, width = 6, onefile = FALSE)
ggsurvplot(fit, data = tx421_LUAD, conf.int = FALSE, fun='event',
           pval = TRUE, pval.size = 5, pval.coord = c(0.2, 0.1),
           linetype = "solid",
           surv.plot.height = 0.7, palette = c("#1B9E77", "#D95F02",  "#7570B3"),
           risk.table = TRUE,
           risk.table.col = "black", break.time.by = 500,
           tables.height = 0.25, 
           tables.theme = theme_cleantable(),
           risk.table.title = "Number at Risk",
           tables.x.text = "none", xlim = c(0, 2500), ylim=c(0,1),
           xlab = "Days to Recurrence", ylab = "DFS event")
dev.off()
tx421_LUAD$IASLC_grade_ai_merge <- NULL
tx421_LUAD$IASLC_grade_ai_merge[tx421_LUAD$IASLC_grade_ai=='Grade3'] <- 'Grade3'
tx421_LUAD$IASLC_grade_ai_merge[tx421_LUAD$IASLC_grade_ai=='Grade2'] <- 'Grade1&2'
tx421_LUAD$IASLC_grade_ai_merge[tx421_LUAD$IASLC_grade_ai=='Grade1'] <- 'Grade1&2'
tx421_LUAD$IASLC_grade_ai_merge <- factor(tx421_LUAD$IASLC_grade_ai_merge, levels = c('Grade3', 'Grade1&2'))
fit <- coxph(Surv(time_DFS, DFS)~IASLC_grade_ai_merge, data = tx421_LUAD)
summary(fit)
Anova(fit, type='III', test.statistic = 'Wald')
#IASLC_grade_ai_mergeGrade1&2    0.4831       2.07    0.2989    0.7807, p=0.002972 **

#fig3b
fit <- coxph(Surv(time_DFS, DFS)~Age+Sex+stage+Pack.years+
               Adjuvant.treatment+Surgery.type+IASLC_grade_ai_merge, data = tx421_LUAD)
pdf('./figure3b_tx421.pdf', height = 7, width = 7, onefile = FALSE)
ggforest(fit, data=tx421_LUAD, fontsize = 0.8) 
dev.off()
summary(fit)
cox_zph <- cox.zph(fit)
ggcoxzph(cox_zph )
Anova(fit, type='III', test.statistic = 'Wald')
#IASLC_grade_ai_mergeGrade1&2      0.5137     1.9467    0.3107    0.8493, p=0.009408

fit <- survfit(Surv(time_DFS, DFS)~IASLC_grade_path, data = tx421_LUAD)
pdf(file="./fig3a_tx421.pdf", height = 5, width = 6, onefile = FALSE)
ggsurvplot(fit, data = tx421_LUAD, conf.int = FALSE, fun='event',
           pval = TRUE, pval.size = 5, pval.coord = c(0.2, 0.1),
           linetype = "solid",
           surv.plot.height = 0.7, palette = c("#1B9E77", "#D95F02",  "#7570B3"),
           risk.table = TRUE,
           risk.table.col = "black", break.time.by = 500,
           tables.height = 0.25, 
           tables.theme = theme_cleantable(),
           risk.table.title = "Number at Risk",
           tables.x.text = "none", xlim = c(0, 2500), ylim=c(0,1),
           xlab = "Days to Recurrence", ylab = "DFS event")
dev.off()
tx421_LUAD$IASLC_grade_path_merge <- NULL
tx421_LUAD$IASLC_grade_path_merge[tx421_LUAD$IASLC_grade_path=='Grade3'] <- 'Grade3'
tx421_LUAD$IASLC_grade_path_merge[tx421_LUAD$IASLC_grade_path=='Grade2'] <- 'Grade1&2'
tx421_LUAD$IASLC_grade_path_merge[tx421_LUAD$IASLC_gradepathi=='Grade1'] <- 'Grade1&2'
tx421_LUAD$IASLC_grade_path_merge <- factor(tx421_LUAD$IASLC_grade_path_merge, levels = c('Grade3', 'Grade1&2'))
fit <- coxph(Surv(time_DFS, DFS)~IASLC_grade_path_merge, data = tx421_LUAD)
summary(fit)
Anova(fit, type='III', test.statistic = 'Wald')

#fig5h-tx421
tx_scattering <- tx421_LUAD[which(tx421_LUAD$scattering_continuous>0),]
tx_scattering$scattering_cutoff <- factor(tx_scattering$scattering_cutoff, levels = c('low','high'))
fit <- survfit(Surv(time_DFS, DFS)~scattering_cutoff, data = tx_scattering)
pdf(file="./fig5g_tx421.pdf", height = 5, width = 6, onefile = FALSE)
ggsurvplot(fit, data = tx_scattering, conf.int = FALSE, fun='event',
           pval = TRUE, pval.size = 5, pval.coord = c(0.2, 0.1),
           linetype = "solid",
           surv.plot.height = 0.7, palette = c("#1B9E77", "#D95F02"),
           risk.table = TRUE,
           risk.table.col = "black", break.time.by = 500,
           tables.height = 0.25, 
           tables.theme = theme_cleantable(),
           risk.table.title = "Number at Risk",
           tables.x.text = "none", xlim = c(0, 2500), ylim=c(0,1),
           xlab = "Days to Recurrence", ylab = "DFS event")
dev.off()
fit <- coxph(Surv(time_DFS, DFS)~scattering_cutoff, data = tx_scattering)
ggforest(fit, data=tx_scattering, fontsize = 0.8) 
Anova(fit, type='III', test.statistic = 'Wald')
#p=0.002954 **





#fig5i-tx421
tx_scattering$IASLC_grade_ai <- factor(tx_scattering$IASLC_grade_ai, levels = c('Grade3', 'Grade2', 'Grade1'))
tx_scattering$scattering_cutoff <- factor(tx_scattering$scattering_cutoff, levels = c('low', 'high'))
fit <- coxph(Surv(time_DFS, DFS)~IASLC_grade_ai+scattering_cutoff, data = tx_scattering)
pdf(file="./fig5i_tx.pdf", height = 3, width = 6, onefile = FALSE)
ggforest(fit, data=tx_scattering, fontsize = 0.8) 
dev.off()
summary(fit)
cox_zph <- cox.zph(fit)
ggcoxzph(cox_zph )
Anova(fit, type='III', test.statistic = 'Wald')
#IASLC_grade_ai     2 9.0826   0.010660 * 
#scattering_cutoff  1 8.1912   0.004209 **

#extended data fig8-lattice
data_grade1 <- tx_scattering[which(tx_scattering$IASLC_grade_ai=='Grade1'),]
data_grade2 <- tx_scattering[which(tx_scattering$IASLC_grade_ai=='Grade2'),]
data_grade3 <- tx_scattering[which(tx_scattering$IASLC_grade_ai=='Grade3'),]
data_grade12 <- tx_scattering[which(tx_scattering$IASLC_grade_ai=='Grade1'| tx_scattering$IASLC_grade_ai=='Grade2'),]
pdf(file="./extendDataFig8.pdf", height = 3, width = 6, onefile = FALSE)
fit <- coxph(Surv(time_DFS, DFS)~scattering_cutoff, data = data_grade3)
ggforest(fit, data=data_grade3, fontsize = 0.8) 
#dev.off()
Anova(fit, type='III', test.statistic = 'Wald')
summary(fit)
#grade3:  0.04235, n=137
#grade2: 0.0533, n=56
#grade1: 0.5112, n=12
#grade12: 0.02517 , n=68

#fig5b
lep <- read_csv('/Users/xpan/Documents/code_R/tx1184noSmooth_individual_lep_pix.csv')
pap <- read_csv('/Users/xpan/Documents/code_R/tx1184noSmooth_individual_pap_pix.csv')
aci <- read_csv('/Users/xpan/Documents/code_R/tx1184noSmooth_individual_aci_pix.csv')
cri <- read_csv('/Users/xpan/Documents/code_R/tx1184noSmooth_individual_cri_pix.csv')
sol <- read_csv('/Users/xpan/Documents/code_R/tx1184noSmooth_individual_sol_pix.csv')
mic <- read_csv('/Users/xpan/Documents/code_R/tx1184noSmooth_individual_mic_pix.csv')

lep <- lep[which(lep$lep_pix>0),]
pap <- pap[which(pap$pap_pix>0),]
aci <- aci[which(aci$aci_pix>0),] 
cri <- cri[which(cri$cri_pix>0),]
sol <- sol[which(sol$sol_pix>0),]
mic <- mic[which(mic$mic_pix>0),]

combined_df <- bind_rows(lep, aci, pap, cri, mic, sol)
write.table(combined_df, file = '/Users/xpan/Documents/code_R/tx_individual_pix.txt', sep = "\t", row.names = FALSE)

lep_sub <- lep[which(lep$lep_pix>=quantile(lep$lep_pix, probs = seq(0.0, 0.95, 0.1))[1] & lep$lep_pix<=2000),]
pap_sub <- pap[which(pap$pap_pix>=quantile(pap$pap_pix, probs = seq(0.0, 0.95, 0.1))[1] & pap$pap_pix<=2000),]
aci_sub <- aci[which(aci$aci_pix>=quantile(aci$aci_pix, probs = seq(0.0, 0.95, 0.1))[1] & aci$aci_pix<=2000),]  
cri_sub <- cri[which(cri$cri_pix>=quantile(cri$cri_pix, probs = seq(0.0, 0.95, 0.1))[1] & cri$cri_pix<=2000),]
mic_sub <- mic[which(mic$mic_pix>=quantile(mic$mic_pix, probs = seq(0.0, 0.95, 0.1))[1] & mic$mic_pix<=2000),]
sol_sub <- sol[which(sol$sol_pix>=quantile(sol$sol_pix, probs = seq(0.0, 0.95, 0.1))[1] & sol$sol_pix<=2000),]
pattern_pix <- data.frame(pattern=c(rep('lep', length(lep_sub$lep_pix)), rep('pap', length(pap_sub$pap_pix)), rep('aci', length(aci_sub$aci_pix)),
                                    rep('cri', length(cri_sub$cri_pix)), rep('sol', length(sol_sub$sol_pix)), rep('mic', length(mic_sub$mic_pix))),
                          pix = c(lep_sub$lep_pix, pap_sub$pap_pix, aci_sub$aci_pix, cri_sub$cri_pix, sol_sub$sol_pix, mic_sub$mic_pix))
pattern_pix$pattern<-factor(pattern_pix$pattern, levels=c('lep', 'pap', 'aci', 'cri', 'mic', 'sol'))
pdf(file="./fig5b_pix_tx.pdf", height = 4, width = 4)
ggplot(pattern_pix, aes(x=pix, group=pattern, color=pattern)) +
  geom_density() +
  scale_colour_manual(values = c('lep'="#0000ff",'pap'="#ffff00", 'aci'="#ff0000", 'cri'="#00ffff", 'mic'='#ff00ff', 'sol'='#880000'))+
  theme_classic()
dev.off()

lep_sub <- lep[which(lep$lep_pix<=quantile(lep$lep_pix, probs = seq(0.05, 0.95, 0.1))[10] & lep$lep_pix>2000),]
pap_sub <- pap[which(pap$pap_pix<=quantile(pap$pap_pix, probs = seq(0.05, 0.95, 0.1))[10] & pap$pap_pix>2000),]
aci_sub <- aci[which(aci$aci_pix<=quantile(aci$aci_pix, probs = seq(0.05, 0.95, 0.1))[10] & aci$aci_pix>2000),]  
cri_sub <- cri[which(cri$cri_pix<=quantile(cri$cri_pix, probs = seq(0.05, 0.95, 0.1))[10] & cri$cri_pix>2000),]
mic_sub <- mic[which(mic$mic_pix<=quantile(mic$mic_pix, probs = seq(0.05, 0.95, 0.1))[10] & mic$mic_pix>2000),]
sol_sub <- sol[which(sol$sol_pix<=quantile(sol$sol_pix, probs = seq(0.05, 0.95, 0.1))[10] & sol$sol_pix>2000),]
pattern_pix <- data.frame(pattern=c(rep('lep', length(lep_sub$lep_pix)), rep('pap', length(pap_sub$pap_pix)), rep('aci', length(aci_sub$aci_pix)),
                                    rep('cri', length(cri_sub$cri_pix)), rep('sol', length(sol_sub$sol_pix)), rep('mic', length(mic_sub$mic_pix))),
                          pix = c(lep_sub$lep_pix, pap_sub$pap_pix, aci_sub$aci_pix, cri_sub$cri_pix, sol_sub$sol_pix, mic_sub$mic_pix))
pattern_pix$pattern<-factor(pattern_pix$pattern, levels=c('lep', 'pap', 'aci', 'cri', 'mic', 'sol'))
pdf(file="./fig5b_pix_tx2000.pdf", height = 4, width = 4)
ggplot(pattern_pix, aes(x=pix, group=pattern, color=pattern)) +
  geom_density() +
  scale_colour_manual(values = c('lep'="#0000ff",'pap'="#ffff00", 'aci'="#ff0000", 'cri'="#00ffff", 'mic'='#ff00ff', 'sol'='#880000'))+
  theme_classic()
dev.off()

#fig5c, 5d, 5f, extended data fig7b, 7c, 7d
data_aci <- tx421_LUAD[which(tx421_LUAD$aci_per>=0.05),] #n=173
data_aci$predom_ai <- factor(data_aci$predom_ai, levels=c('lep', 'pap', 'aci', 'cri', 'mic', 'sol'))
pdf(file="./extendDataFig7b_area_tx.pdf", height = 4, width = 3)
ggplot(data_aci, aes(x=predom_ai, y=area_mean, fill=predom_ai)) +
  geom_boxplot(width=0.6,outlier.shape=NA) +
  geom_jitter(color="black", size=0.3, width=0.2) +
  scale_fill_manual(values = c('lep'="#0000ff",'pap'="#ffff00", 'aci'="#ff0000", 'cri'="#00ffff", 'mic'='#ff00ff', 'sol'='#880000'))+
  theme_classic() +
  theme(legend.position="none",plot.title = element_text(size=11)) +
  stat_compare_means()+
  xlab("")
dev.off()
kruskal.test(area_mean ~ predom_ai, data = data_aci) # n=173, p= 1.493e-09, Kruskal-Wallis rank sum test, one-way


pdf(file="./extendDataFig7b_soli_tx.pdf", height = 4, width = 3)
ggplot(data_aci, aes(x=predom_ai, y=soli_mean, fill=predom_ai)) +
  geom_boxplot(width=0.6,outlier.shape=NA) +
  geom_jitter(color="black", size=0.3, width=0.2) +
  scale_fill_manual(values = c('lep'="#0000ff",'pap'="#ffff00", 'aci'="#ff0000", 'cri'="#00ffff", 'mic'='#ff00ff', 'sol'='#880000'))+
  theme_classic() +
  theme(legend.position="none",plot.title = element_text(size=11)) +
  stat_compare_means()+
  xlab("")
dev.off()
kruskal.test(soli_mean ~ predom_ai, data = data_aci) # n=173, p= 0.0005932, Kruskal-Wallis rank sum test, one-way
pairwise.wilcox.test(data_aci$soli_mean, data_aci$predom_ai,p.adjust.method = "BH")

data_aci$predom_merge <- NULL
data_aci$predom_merge[data_aci$predom_ai=='lep'] <- 'Lep'
data_aci$predom_merge[data_aci$predom_ai=='aci' | data_aci$predom_ai=='pap'] <- 'Aci/Pap'
data_aci$predom_merge[data_aci$predom_ai=='cri' | data_aci$predom_ai=='mic'| data_aci$predom_ai=='sol'] <- 'Cri/Mic/Sol'
data_aci$predom_merge <- factor(data_aci$predom_merge, levels=c('Lep', 'Aci/Pap', 'Cri/Mic/Sol'))
my_comparison1 <- list( c("Lep", "Aci/Pap"), c("Aci/Pap", "Cri/Mic/Sol"), c("Lep", "Cri/Mic/Sol") )
pdf(file="./fig5c_area_mean_tx.pdf", height = 4, width = 3)
ggplot(data_aci, aes(x=predom_merge, y=area_mean, fill=predom_merge)) +
  geom_boxplot(width=0.6, outlier.shape=NA) +
  geom_jitter(color="black", size=0.3, width=0.2) +
  scale_fill_brewer(palette='Pastel1')+
  theme_classic() +
  theme(legend.position="none",plot.title = element_text(size=11)) +
  stat_compare_means(comparison=my_comparison1,method = "wilcox.test")+
  xlab("")
dev.off()
pdf(file="./fig5c_area_soli_tx.pdf", height = 4, width = 3)
ggplot(data_aci, aes(x=predom_merge, y=area_sd, fill=predom_merge)) +
  geom_boxplot(width=0.6, outlier.shape=NA) +
  geom_jitter(color="black", size=0.3, width=0.2) +
  scale_fill_brewer(palette='Pastel1')+
  theme_classic() +
  theme(legend.position="none",plot.title = element_text(size=11)) +
  stat_compare_means(comparison=my_comparison1,method = "wilcox.test")+
  xlab("")
dev.off()
#non-adjusted
data_aci1 <- data_aci[which(data_aci$predom_merge=='Lep' | data_aci$predom_merge=='Aci/Pap'),]
wilcox.test(area_mean ~ predom_merge, data_aci1) #n=108,p=0.0005161
wilcox.test(area_sd ~ predom_merge, data_aci1) #n=108,p=0.002889

data_aci2 <- data_aci[which(data_aci$predom_merge=='Aci/Pap' | data_aci$predom_merge=='Cri/Mic/Sol'),]
wilcox.test(area_mean ~ predom_merge, data_aci2) #n=157,p=9.797e-11
wilcox.test(area_sd ~ predom_merge, data_aci2) #n=157,p=7.617e-08
pairwise.wilcox.test(data_aci$area_mean, data_aci$predom_merge,p.adjust.method = "BH")
#BH adjusted
#            Lep     Aci/Pap
#Aci/Pap     0.00077 -      
#Cri/Mic/Sol 0.20224 2.9e-10

pdf(file="./fig5f_soli_mean_tx.pdf", height = 4, width = 3)
ggplot(data_aci, aes(x=predom_merge, y=soli_mean, fill=predom_merge)) +
  geom_boxplot(width=0.6, outlier.shape=NA) +
  geom_jitter(color="black", size=0.3, width=0.2) +
  scale_fill_brewer(palette='Set3')+
  theme_classic() +
  theme(legend.position="none",plot.title = element_text(size=11)) +
  stat_compare_means(comparison=my_comparison1,method = "wilcox.test")+
  xlab("")
dev.off()
data_aci1 <- data_aci[which(data_aci$predom_merge=='Lep' | data_aci$predom_merge=='Aci/Pap'),]
wilcox.test(soli_mean ~ predom_merge, data_aci1) #n=108,p=0.4492

data_aci2 <- data_aci[which(data_aci$predom_merge=='Aci/Pap' | data_aci$predom_merge=='Cri/Mic/Sol'),]
wilcox.test(soli_mean ~ predom_merge, data_aci2) #n=157,p=0.0001839

data_aci3 <- data_aci[which(data_aci$predom_merge=='Lep' | data_aci$predom_merge=='Cri/Mic/Sol'),]
wilcox.test(soli_mean ~ predom_merge, data_aci3) #n=81,p=0.002439
wilcox.test(soli_sd ~ predom_merge, data_aci3) #n=81, p=6.374e-06

data_pattern <- data_aci[which(data_aci$predom_ai=='aci'|data_aci$predom_ai=='cri'|data_aci$predom_ai=='sol'),]
my_comparison2 <- list( c("aci", "cri"), c("aci", "sol"), c("cri", "sol"))
pdf(file="./fig5d_aciCrisol_area_mean_tx.pdf", height = 4, width = 3)
ggplot(data_pattern, aes(x=predom_ai, y=area_mean, fill=predom_ai)) +
  geom_boxplot(width=0.6,outlier.shape=NA) +
  geom_jitter(color="black", size=0.3, width=0.2) +
  scale_fill_manual(values = c('aci'="#ff0000", 'cri'="#00ffff", 'sol'='#880000'))+
  theme_classic() +
  theme(legend.position="none",plot.title = element_text(size=11)) +
  stat_compare_means(comparison=my_comparison2,method = "wilcox.test")+
  xlab("")
dev.off()
pairwise.wilcox.test(data_pattern$area_mean, data_pattern$predom_ai,p.adjust.method = "none")
#BH non-adjusted, n=141
#      aci     cri 
#cri 0.0007 -   
#sol 1.9e-09 0.3705
data_pattern1 <- data_aci[which(data_aci$predom_ai=='aci' | data_aci$predom_ai=='cri'),]
wilcox.test(area_mean ~ predom_ai, data_pattern1) #n=95,p= 0.0006956

#fig5g
tx_scattering$predom_ai <- factor(tx_scattering$predom_ai, levels=c('lep', 'pap', 'aci', 'cri', 'mic', 'sol'))
pdf(file="./fig5f_predom_pattern_scattering_tx.pdf", height = 4, width = 3)
ggplot(tx_scattering, aes(x=factor(predom_ai), fill=factor(scattering_cutoff)))+
  geom_bar(color='black',  position="fill")+
  geom_text(aes(label=after_stat(count)),stat='count',position=position_fill(vjust=0.5))+
  scale_fill_brewer(palette='Set2')+
  theme_classic()+labs(fill = "Pattern")+xlab("Predominant subtype")+ylab("Proportion")
dev.off()
#fisher exact test
df <- data.frame('Lep'=c(13, 3), 'Other'= c(7+39+7+37, 13+37+14+35), row.names = c('low', 'high'))
fisher.test(df) 
#n=205, p=0.01663

#fig3c, e.g.stageI
#compute c-inedex
data_stage <-tx421_LUAD[which(tx421_LUAD$stage=='I'), ] #n=108
data_ci = data_stage[c('time_DFS', 'DFS', 'Age', 'Sex','stage',
                       'Pack.years', 'Adjuvant.treatment', 'Surgery.type',
                       'IASLC_grade_ai', 'IASLC_grade_path')]
names(data_ci)[names(data_ci)=='time_DFS'] <- 'time'
names(data_ci)[names(data_ci)=='DFS'] <- 'status'
D=CompCase(data.matrix(data_ci[, c(1:4)])) 
tau=1825
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)
#0.6651 0.0480 0.5712 0.7591
D=CompCase(data.matrix(data_ci[, c(1:4,9)])) #since stage will be the same, thus excluded
tau=1825#quantile(data_ci$time)[3]
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)
#AI grading: 0.7004 0.0420 0.6182 0.7827

D=CompCase(data.matrix(data_ci[, c(1:4,10)])) #since stage will be the same, thus excluded
tau=1825#quantile(data_ci$time)[3]
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)
#Path grading: 0.6953 0.0449 0.6074 0.7833

D=CompCase(data.matrix(data_ci[, c(1:4, 9, 10)])) #since stage will be the same, thus excluded
tau=1825#quantile(data_ci$time)[3]
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)
#AI + Path grading: 0.7080 0.0449 0.6200 0.7960
#check multicollinearity
model_all <- lm(DFS ~ Age + Sex+IASLC_grade_ai+IASLC_grade_path, data=data_stage)  # with all the independent variables in the dataframe
vif(model_all)
summary(model_all)
#                     GVIF Df GVIF^(1/(2*Df))
#Age              1.042996  1        1.021272
#Sex              1.079627  1        1.039051
#IASLC_grade_ai   1.820075  2        1.161508
#IASLC_grade_path 1.799247  2        1.158171

#plot c-index
summaryCsAP <- data.frame(
  Measure = c('Baseline', 'AI', 'Path', 'AI + Path'),
  lower95s1 = c(0.5712, 0.6182, 0.6074, 0.6200),
  upper95s1 = c(0.7591, 0.7827, 0.7833, 0.7960), 
  Cinds1 = c(0.6651, 0.7004, 0.6953, 0.7080),
  lower95s13 = c(0.6076, 0.6247, 0.6253, 0.6303),
  upper95s13 = c(0.7326, 0.7524, 0.7519, 0.7567), 
  Cinds13 = c(0.6701, 0.6885, 0.6886, 0.6935))
summaryCsAP$Measure <- factor(summaryCsAP$Measure, levels=c('Baseline', 'AI', 'Path', 'AI + Path'))
pdf(file="./fig3c_stage1_cindexCI_ai_path_tx.pdf", height = 3, width = 2)
ggplot(data=summaryCsAP,aes(x=Measure,y=Cinds1, ymin = lower95s1, ymax = upper95s1, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c('#000000', "#E41A1C", "#7570B3", "#BEBADA"), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.48,0.80)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()

pdf(file="./fig3c_stage13_cindexCI_ai_path_tx.pdf", height = 3, width = 2)
ggplot(data=summaryCsAP,aes(x=Measure,y=Cinds13, ymin = lower95s13, ymax = upper95s13, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c('#000000', "#E41A1C", "#7570B3", "#BEBADA"), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.48,0.80)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()


#supplementary fig1-7
aci_morph_rand1 <- read.csv('/Users/xpan/Documents/code_R/tx1184_solidity_area_allAcinar_random2Rebuttal.csv')
aci_morph_rand2 <- read.csv('/Users/xpan/Documents/code_R/tx1184_solidity_area_allAcinar_random3Rebuttal.csv')
aci_morph_rand3 <- merge(aci_morph_rand2, aci_morph_rand1, by='ID')
aci_morph_rand3 <- merge(aci_morph_rand3, LUAD_ai_patient, by='ID')
write.csv(aci_morph_rand3, '/Users/xpan/Documents/code_R/tx1184_solidity_area_allAcinar_random10.csv')

tx421_sup <- merge(tx421_LUAD, aci_morph_rand2, by='ID')
tx421_sup <- merge(tx421_sup, aci_morph_rand1, by='ID')
data_sup <- tx421_sup[which(tx421_sup$aci_per>=0.05),]
data_sup$predom_ai <- factor(data_sup$predom_ai, levels=c('lep', 'pap', 'aci', 'cri', 'mic', 'sol'))

#figS4-tx421
#for area mean: 
var_list1 <- c("area_meanQ1", "area_meanQ2", "area_meanQ3", "area_meanQ4", "area_meanQ5", 
               "area_meanQ6", "area_meanQ7", "area_meanQ8", "area_meanQ9", "area_meanQ10")
p_figs4a <- numeric(length(var_list1))
for (i in 1:length(var_list1)) {
  formula <- as.formula(paste(var_list1[i], "~ predom_ai"))
  result <- kruskal.test(formula, data = data_sup)
  p_figs4a[i] <- result$p.value
}
print(p_figs4a)
#[1] 8.858616e-09 2.912074e-09 2.914101e-09 1.604693e-09 1.040075e-08 4.568240e-10 2.954117e-09
#[8] 2.419632e-09 1.432690e-09 2.870097e-09

#for solidity mean
var_list2 <- c("soli_meanQ1", "soli_meanQ2", "soli_meanQ3", "soli_meanQ4", "soli_meanQ5", 
               "soli_meanQ6", "soli_meanQ7", "soli_meanQ8", "soli_meanQ9", "soli_meanQ10")

p_figs4b <- numeric(length(var_list2))
for (i in 1:length(var_list2)) {
  formula <- as.formula(paste(var_list2[i], "~ predom_ai"))
  result <- kruskal.test(formula, data = data_sup)
  p_figs4b[i] <- result$p.value
}
print(p_figs4b)
#[1] 0.0002813199 0.0016378670 0.0003854758 0.0007675337 0.0038669754 0.0001224812 0.0010425651
#[8] 0.0002189155 0.0012869787 0.0003278952

#figS1-tx
#for area eman:
data_sup$predom_merge <- NULL
data_sup$predom_merge[data_sup$predom_ai=='lep'] <- 'Lep'
data_sup$predom_merge[data_sup$predom_ai=='aci' | data_sup$predom_ai=='pap'] <- 'Aci/Pap'
data_sup$predom_merge[data_sup$predom_ai=='cri' | data_sup$predom_ai=='mic'| data_sup$predom_ai=='sol'] <- 'Cri/Mic/Sol'
data_sup$predom_merge <- factor(data_sup$predom_merge, levels=c('Lep', 'Aci/Pap', 'Cri/Mic/Sol'))

data_sup1 <- data_sup[which(data_sup$predom_merge=='Lep' | data_sup$predom_merge=='Aci/Pap'),]
p_figs1a <- numeric(length(var_list1))
for (i in 1:length(var_list1)) {
  formula <- as.formula(paste(var_list1[i], "~ predom_merge"))
  result <- wilcox.test(formula, data = data_sup1)
  p_figs1a[i] <- result$p.value
}
print(p_figs1a)
#[1] 0.0007559035 0.0006877893 0.0011298166 0.0004389680 0.0004389680 0.0005868037 0.0009693622
#[8] 0.0004112280 0.0002583417 0.0013145254 #n=108

data_sup2 <- data_sup[which(data_sup$predom_merge=='Aci/Pap' | data_sup$predom_merge=='Cri/Mic/Sol'),]
p_figs1b <- numeric(length(var_list1))
for (i in 1:length(var_list1)) {
  formula <- as.formula(paste(var_list1[i], "~ predom_merge"))
  result <- wilcox.test(formula, data = data_sup2)
  p_figs1b[i] <- result$p.value
}
print(p_figs1b)
#n=157
#[1] 5.674948e-10 1.801276e-10 1.210760e-10 1.679765e-10 9.956295e-10 2.627624e-11 1.299022e-10
#[8] 1.976729e-10 1.239508e-10 1.329817e-10

#figS2-tx
data_sup3 <- data_sup[which(data_sup$predom_ai=='aci' | data_sup$predom_ai=='cri'),]
p_figs2a <- numeric(length(var_list1))
for (i in 1:length(var_list1)) {
  formula <- as.formula(paste(var_list1[i], "~ predom_merge"))
  result <- wilcox.test(formula, data = data_sup3)
  p_figs2a[i] <- result$p.value
}
print(p_figs2a)
#[1] 0.0005473416 0.0013065160 0.0002510971 0.0022403011 0.0015347861 0.0003998452 0.0005863708
#[8] 0.0007443142 0.0007698615 0.0006068427 #n=95

data_sup4 <- data_sup[which(data_sup$predom_ai=='aci' | data_sup$predom_ai=='sol'),]
p_figs2b <- numeric(length(var_list1))
for (i in 1:length(var_list1)) {
  formula <- as.formula(paste(var_list1[i], "~ predom_merge"))
  result <- wilcox.test(formula, data = data_sup4)
  p_figs2b[i] <- result$p.value
}
print(p_figs2b)
#[1] 1.294574e-08 3.340323e-09 2.268306e-09 2.057850e-09 7.866135e-09 6.937757e-10 2.420105e-09
#[8] 4.597911e-09 2.268306e-09 3.033377e-09, n=122

#figS3-tx
data_sup5 <- data_sup[which(data_sup$predom_merge=='Lep' | data_sup$predom_merge=='Cri/Mic/Sol'),]
p_figs3 <- numeric(length(var_list2))
for (i in 1:length(var_list2)) {
  formula <- as.formula(paste(var_list2[i], "~ predom_merge"))
  result <- wilcox.test(formula, data = data_sup5)
  p_figs3[i] <- result$p.value
}
print(p_figs3)
#[1] 0.0015707241 0.0050250768 0.0021665957 0.0060306432 0.0046672354 0.0027424893 0.0026377028
#[8] 0.0009543957 0.0064812373 0.0010383955 #n=81

#figS6-tx
#for area sd
var_list3 <- c("area_sdQ1", "area_sdQ2", "area_sdQ3", "area_sdQ4", "area_sdQ5", 
               "area_sdQ6", "area_sdQ7", "area_sdQ8", "area_sdQ9", "area_sdQ10")
#for solidity sd
var_list4 <- c("soli_sdQ1", "soli_sdQ2", "soli_sdQ3", "soli_sdQ4", "soli_sdQ5", 
               "soli_sdQ6", "soli_sdQ7", "soli_sdQ8", "soli_sdQ9", "soli_sdQ10")

p_figs6a <- numeric(length(var_list3))
for (i in 1:length(var_list3)) {
  formula <- as.formula(paste(var_list3[i], "~ predom_merge"))
  result <- wilcox.test(formula, data = data_sup1)
  p_figs6a[i] <- result$p.value
}
print(p_figs6a)
#[1] 0.005010992 0.002048788 0.010869028 0.001718798 0.002048788 0.005879314 0.007437248 0.001770160
#[9] 0.001237530 0.009355827, n=108

p_figs6b <- numeric(length(var_list3))
for (i in 1:length(var_list3)) {
  formula <- as.formula(paste(var_list3[i], "~ predom_merge"))
  result <- wilcox.test(formula, data = data_sup2)
  p_figs6b[i] <- result$p.value
}
print(p_figs6b)
#[1] 6.229814e-07 3.782820e-08 6.499673e-08 1.481847e-07 1.808808e-06 3.336929e-09 2.536108e-07
#[8] 4.356487e-08 6.763151e-08 1.150880e-07, n=157

#figS7-tx
p_figs7 <- numeric(length(var_list4))
for (i in 1:length(var_list4)) {
  formula <- as.formula(paste(var_list4[i], "~ predom_merge"))
  result <- wilcox.test(formula, data = data_sup5)
  p_figs7[i] <- result$p.value
}
print(p_figs7)
#[1] 9.922079e-07 2.348454e-05 2.277422e-06 9.930203e-06 2.749195e-05 2.414214e-06 6.561608e-05
#[8] 1.343802e-07 3.951981e-05 8.792449e-07, n=81


