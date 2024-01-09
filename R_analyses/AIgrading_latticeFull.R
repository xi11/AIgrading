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


####LATTICe-A#####
#all cases: 845
latticeAll <- read.csv('/Users/xpan/Documents/manuscript/NatureCanRebuttal/latticeAIclinicalPath.csv')
#fig2c
cm <- confusionMatrix(factor(latticeAll$predom_ai), factor(latticeAll$predom_path1),dnn = c("Prediction", "Reference"))
plt<-as.data.frame(round(prop.table(cm$table, margin=2)*100,1))
plt$Prediction <- factor(plt$Prediction, levels=rev(c('lep', 'pap', 'aci', 'cri', 'mic', 'sol')))
plt$Reference <- factor(plt$Reference, levels=c('lep', 'pap', 'aci', 'cri', 'mic', 'sol'))
pdf(file="./fig2c_latticeP1.pdf", height = 4, width = 5.6, pointsize = 5)
ggplot(plt, aes(x=Reference, y=Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(x=Reference, y=Prediction, label=Freq)) +
  scale_fill_gradient(low="white", high="#0094B9") +
  theme_minimal()+
  theme(axis.ticks.y=element_blank())+
  labs(x = "Reference-Path 1",y = "Prediction-Automated") +
  scale_x_discrete(labels=c("lep","pap","aci","cri","mic","sol")) +
  scale_y_discrete(labels=rev(c("lep","pap","aci","cri","mic","sol")))
dev.off()

cm <- confusionMatrix(factor(latticeAll$predom_ai), factor(latticeAll$predom_path2),dnn = c("Prediction", "Reference"))
plt<-as.data.frame(round(prop.table(cm$table, margin=2)*100,1))
plt$Prediction <- factor(plt$Prediction, levels=rev(c('lep', 'pap', 'aci', 'cri', 'mic', 'sol')))
plt$Reference <- factor(plt$Reference, levels=c('lep', 'pap', 'aci', 'cri', 'mic', 'sol'))
pdf(file="./fig2c_latticeP2.pdf", height = 4, width = 5.6, pointsize = 5)
ggplot(plt, aes(x=Reference, y=Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(x=Reference, y=Prediction, label=Freq)) +
  scale_fill_gradient(low="white", high="#F9A100") +
  theme_minimal()+
  theme(axis.ticks.y=element_blank())+
  labs(x = "Reference-Path 2",y = "Prediction-Automated") +
  scale_x_discrete(labels=c("lep","pap","aci","cri","mic","sol")) +
  scale_y_discrete(labels=rev(c("lep","pap","aci","cri","mic","sol")))
dev.off()

cm <- confusionMatrix(factor(latticeAll$predom_ai), factor(latticeAll$predom_path3),dnn = c("Prediction", "Reference"))
plt<-as.data.frame(round(prop.table(cm$table, margin=2)*100,1))
plt$Prediction <- factor(plt$Prediction, levels=rev(c('lep', 'pap', 'aci', 'cri', 'mic', 'sol')))
plt$Reference <- factor(plt$Reference, levels=c('lep', 'pap', 'aci', 'cri', 'mic', 'sol'))
pdf(file="./fig2c_latticeP3.pdf", height = 4, width = 5.6, pointsize = 5)
ggplot(plt, aes(x=Reference, y=Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(x=Reference, y=Prediction, label=Freq)) +
  scale_fill_gradient(low="white", high="#38761D") +
  theme_minimal()+
  theme(axis.ticks.y=element_blank())+
  labs(x = "Reference-Path 3",y = "Prediction-Automated") +
  scale_x_discrete(labels=c("lep","pap","aci","cri","mic","sol")) +
  scale_y_discrete(labels=rev(c("lep","pap","aci","cri","mic","sol")))
dev.off()

#fig2d
latticeAll$agreementP1 <- NULL
latticeAll$agreementP1[latticeAll$predom_ai == latticeAll$predom_path1] <- 'in Agreement'
latticeAll$agreementP1[latticeAll$predom_ai != latticeAll$predom_path1] <- 'Discrepant'
comList <- list( c('in Agreement', 'Discrepant'))
pdf('./fig2d_latticeP1.pdf', height = 4, width = 3)
ggplot(latticeAll, aes(x=agreementP1, y=shannon_path1, color=agreementP1)) +
  geom_boxplot(width=0.7) +
  geom_jitter(aes(colour=agreementP1), width=0.2) +
  scale_colour_manual(values= c('Discrepant'="#0094B9", 'in Agreement'="#888888"))+
  theme_classic() +
  theme(legend.position="none",plot.title = element_text(size=12)) +
  stat_compare_means(comparison=comList,method = "wilcox.test")+
  xlab('')+
  ylab("Growth pattern intra-tumour heterogeneity")
dev.off()

latticeAll$agreementP2 <- NULL
latticeAll$agreementP2[latticeAll$predom_ai == latticeAll$predom_path2] <- 'in Agreement'
latticeAll$agreementP2[latticeAll$predom_ai != latticeAll$predom_path2] <- 'Discrepant'
comList <- list( c('in Agreement', 'Discrepant'))
pdf('./fig2d_latticeP2.pdf', height = 4, width = 3)
ggplot(latticeAll, aes(x=agreementP2, y=shannon_path2, color=agreementP2)) +
  geom_boxplot(width=0.7) +
  geom_jitter(aes(colour=agreementP2), width=0.2) +
  scale_colour_manual(values= c('Discrepant'="#F9A100", 'in Agreement'="#888888"))+
  theme_classic() +
  theme(legend.position="none",plot.title = element_text(size=12)) +
  stat_compare_means(comparison=comList,method = "wilcox.test")+
  xlab('')+
  ylab("Growth pattern intra-tumour heterogeneity")
dev.off()
latticeAll$agreementP2num <- NULL
latticeAll$agreementP2num[latticeAll$predom_ai == latticeAll$predom_path2] <- 1
latticeAll$agreementP2num[latticeAll$predom_ai != latticeAll$predom_path2] <- 0
wilcox.test(shannon_path2 ~ agreementP2, data = latticeAll,exact = TRUE)

latticeAll$agreementP3 <- NULL
latticeAll$agreementP3[latticeAll$predom_ai == latticeAll$predom_path3] <- 'in Agreement'
latticeAll$agreementP3[latticeAll$predom_ai != latticeAll$predom_path3] <- 'Discrepant'
comList <- list( c('in Agreement', 'Discrepant'))
pdf('./fig2d_latticeP3.pdf', height = 4, width = 3)
ggplot(latticeAll, aes(x=agreementP3, y=shannon_path3, color=agreementP3)) +
  geom_boxplot(width=0.7) +
  geom_jitter(aes(colour=agreementP3), width=0.2) +
  scale_colour_manual(values= c('Discrepant'="#38761D", 'in Agreement'="#888888"))+
  theme_classic() +
  theme(legend.position="none",plot.title = element_text(size=12)) +
  stat_compare_means(comparison=comList,method = "wilcox.test")+
  xlab('')+
  ylab("Growth pattern intra-tumour heterogeneity")
dev.off()
wilcox.test(shannon_path3 ~ agreementP3, data = latticeAll)

#extended data fig4d
latticeAll$agreementP12 <- NULL
latticeAll$agreementP12[latticeAll$predom_path1 == latticeAll$predom_path2] <- 'in Agreement'
latticeAll$agreementP12[latticeAll$predom_path1 != latticeAll$predom_path2] <- 'Discrepant'
comList <- list(c('Discrepant', 'in Agreement'))
latticeAll$agreementP12 <- factor(latticeAll$agreementP12, levels = c('Discrepant', 'in Agreement'))
pdf(file="./figS4disconP12.pdf", height = 4, width = 3)
ggboxplot(latticeAll, x = "agreementP12", y = "shannon_path1", xlab = "Path 1 vs. Path 2", ylab = "Growth pattern intra-tumour heterogeneity-Path1", color = "agreementP12", palette = c("#1B9E77", "#888888"), 
          add = "jitter", border = "white")+
  stat_compare_means(comparisons = comList, method = "wilcox")+
  theme(legend.position="") + theme(text = element_text(size=12))
dev.off()
wilcox.test(shannon_path1 ~ agreementP12, data = latticeAll)

latticeAll$agreementP13 <- NULL
latticeAll$agreementP13[latticeAll$predom_path1 == latticeAll$predom_path3] <- 'in Agreement'
latticeAll$agreementP13[latticeAll$predom_path1 != latticeAll$predom_path3] <- 'Discrepant'
comList <- list(c('Discrepant', 'in Agreement'))
latticeAll$agreementP13 <- factor(latticeAll$agreementP13, levels = c('Discrepant', 'in Agreement'))
pdf(file="./figS4disconP12.pdf", height = 4, width = 3)
ggboxplot(latticeAll, x = "agreementP13", y = "shannon_path3", xlab = "Path 1 vs. Path 3", ylab = "Growth pattern intra-tumour heterogeneity-Path3", color = "agreementP13", palette = c("#1B9E77", "#888888"), 
          add = "jitter", border = "white")+
  stat_compare_means(comparisons = comList, method = "wilcox")+
  theme(legend.position="") + theme(text = element_text(size=12))
dev.off()
wilcox.test(shannon_path3 ~ agreementP13, data = latticeAll) # p=4.323e-13

latticeAll$agreementP23 <- NULL
latticeAll$agreementP23[latticeAll$predom_path2 == latticeAll$predom_path3] <- 'in Agreement'
latticeAll$agreementP23[latticeAll$predom_path2 != latticeAll$predom_path3] <- 'Discrepant'
comList <- list(c('Discrepant', 'in Agreement'))
latticeAll$agreementP23 <- factor(latticeAll$agreementP23, levels = c('Discrepant', 'in Agreement'))
pdf(file="./figS4disconP23.pdf", height = 4, width = 3)
ggboxplot(latticeAll, x = "agreementP23", y = "shannon_path2", xlab = "Path 2 vs. Path 3", ylab = "Growth pattern intra-tumour heterogeneity-Path2", color = "agreementP23", palette = c("#1B9E77", "#888888"), 
          add = "jitter", border = "white")+
  stat_compare_means(comparisons = comList, method = "wilcox")+
  theme(legend.position="") + theme(text = element_text(size=12))
dev.off()
wilcox.test(shannon_path2 ~ agreementP23, data = latticeAll) # n=845, p=1.589e-15


#fig2e
cm <- confusionMatrix(factor(latticeAll$IASLC_grade_ai), factor(latticeAll$IASLC_grade_path1),dnn = c("Prediction", "Reference"))
plt<-as.data.frame(round(prop.table(cm$table, margin=2)*100,1))
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
pdf(file="./fig2e_grade_path1.pdf", height = 4, width = 5.6, pointsize = 5)
ggplot(plt, aes(x=Reference, y=Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(x=Reference, y=Prediction, label=Freq)) +
  scale_fill_gradient(low="white", high="#0094B9") +
  theme_minimal()+
  theme(axis.ticks.y=element_blank(),
        text=element_text(family="sans", size = 5),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5))+
  labs(x = "Reference-Path 1",y = "Prediction-AI") +
  scale_x_discrete(labels=c("Grade1","Grade2","Grade3")) +
  scale_y_discrete(labels=c("Grade3","Grade2","Grade1")) 
dev.off()

cm <- confusionMatrix(factor(latticeAll$IASLC_grade_ai), factor(latticeAll$IASLC_grade_path2), dnn = c("Prediction", "Reference"))
plt<-as.data.frame(round(prop.table(cm$table, margin=2)*100,1))
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
pdf(file="./fig2e_grade_path2.pdf", height = 4, width = 5.6, pointsize = 5)
ggplot(plt, aes(x=Reference, y=Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(x=Reference, y=Prediction, label=Freq)) +
  scale_fill_gradient(low="white", high="#F9A100") +
  theme_minimal()+
  theme(axis.ticks.y=element_blank(),
        text=element_text(family="sans", size = 5),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5))+
  labs(x = "Reference-Path 2",y = "Prediction-AI") +
  scale_x_discrete(labels=c("Grade1","Grade2","Grade3")) +
  scale_y_discrete(labels=c("Grade3","Grade2","Grade1")) 
dev.off()

cm <- confusionMatrix(factor(latticeAll$IASLC_grade_ai), factor(latticeAll$IASLC_grade_path3), dnn = c("Prediction", "Reference"))
plt<-as.data.frame(round(prop.table(cm$table, margin=2)*100,1))
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))
pdf(file="./fig2e_grade_path3.pdf", height = 4, width = 5.6, pointsize = 5)
ggplot(plt, aes(x=Reference, y=Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(x=Reference, y=Prediction, label=Freq)) +
  scale_fill_gradient(low="white", high="#38761D") +
  theme_minimal()+
  theme(axis.ticks.y=element_blank(),
        text=element_text(family="sans", size = 5),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5))+
  labs(x = "Reference-Path 3",y = "Prediction-AI") +
  scale_x_discrete(labels=c("Grade1","Grade2","Grade3")) +
  scale_y_discrete(labels=c("Grade3","Grade2","Grade1")) 
dev.off()

#fig3a
data_stage <- latticeAll[which(latticeAll$stage=='I'|latticeAll$stage=='II'|latticeAll$stage=='III'),]
fit <- survfit(Surv(time_to_rfs, rfs_status)~IASLC_grade_ai, data = data_stage)
pdf(file="./fig3a_lattice.pdf", height = 5, width = 6, onefile = FALSE)
ggsurvplot(fit, data = data_stage, conf.int = FALSE, fun='event',
           pval = TRUE, pval.size = 5, pval.coord = c(0.2, 0.1),
           linetype = "solid",
           surv.plot.height = 0.7, palette = c("#1B9E77", "#D95F02",  "#7570B3"),
           risk.table = TRUE,
           risk.table.col = "black", break.time.by = 1000,
           tables.height = 0.25, 
           tables.theme = theme_cleantable(),
           risk.table.title = "Number at Risk",
           tables.x.text = "none", xlim = c(0, 6500), ylim=c(0,1),
           xlab = "Days to Recurrence", ylab = "DFS event")
dev.off()
#fig3a p-val and HRs
data_stage$IASLC_grade_ai_merge <- NULL
data_stage$IASLC_grade_ai_merge[data_stage$IASLC_grade_ai=='Grade3'] <- 'Grade3'
data_stage$IASLC_grade_ai_merge[data_stage$IASLC_grade_ai=='Grade2'] <- 'Grade1&2'
data_stage$IASLC_grade_ai_merge[data_stage$IASLC_grade_ai=='Grade1'] <- 'Grade1&2'
fit <- coxph(Surv(time_to_rfs, rfs_status)~IASLC_grade_ai_merge, data = data_stage)
summary(fit)
Anova(fit, type='III', test.statistic = 'Wald')

#fig3b
data_stage$IASLC_grade_ai_merge <- factor(data_stage$IASLC_grade_ai_merge, levels = c('Grade3','Grade1&2'))
fit <- coxph(Surv(time_to_rfs, rfs_status)~age_surgery+sex+stage+pack_years+
               Adjuvant.therapy+type_surgery+IASLC_grade_ai_merge, data = data_stage)
pdf('./figure3b_lattice.pdf', height = 7, width = 7, onefile = FALSE)
ggforest(fit, data=data_stage, fontsize = 0.8) 
dev.off()
summary(fit)
cox_zph <- cox.zph(fit)
ggcoxzph(cox_zph )
#IASLC_grade_ai_mergeGrade1&2   -0.4448282  0.6409344  0.1371505 -3.243  0.00118 ** 
#IASLC_grade_ai_mergeGrade1&2    0.4899    0.8386 HRs
#stageII                         0.9381490  2.5552473  0.1447255  6.482 9.04e-11 ***
#stageIII                        1.2004376  3.3215703  0.1566720  7.662 1.83e-14 ***

#extended data figure 5b,c,d
#path1
data_stage$IASLC_grade_path1_merge <- NULL
data_stage$IASLC_grade_path1_merge[data_stage$IASLC_grade_path1=='Grade3'] <- 'Grade3'
data_stage$IASLC_grade_path1_merge[data_stage$IASLC_grade_path1=='Grade2'] <- 'Grade1&2'
data_stage$IASLC_grade_path1_merge[data_stage$IASLC_grade_path1=='Grade1'] <- 'Grade1&2'
data_stage$IASLC_grade_path1_merge <- factor(data_stage$IASLC_grade_path1_merge, levels = c('Grade3','Grade1&2'))
fit <- coxph(Surv(time_to_rfs, rfs_status)~age_surgery+sex+stage+pack_years+
               Adjuvant.therapy+type_surgery+IASLC_grade_path1_merge, data = data_stage)
pdf('./figure3b_path1_lattice.pdf', height = 7, width = 7, onefile = FALSE)
ggforest(fit, data=data_stage, fontsize = 0.8) 
dev.off()
summary(fit)
cox_zph <- cox.zph(fit)
ggcoxzph(cox_zph )

#path2
data_stage$IASLC_grade_path2_merge <- NULL
data_stage$IASLC_grade_path2_merge[data_stage$IASLC_grade_path2=='Grade3'] <- 'Grade3'
data_stage$IASLC_grade_path2_merge[data_stage$IASLC_grade_path2=='Grade2'] <- 'Grade1&2'
data_stage$IASLC_grade_path2_merge[data_stage$IASLC_grade_path2=='Grade1'] <- 'Grade1&2'
data_stage$IASLC_grade_path2_merge <- factor(data_stage$IASLC_grade_path2_merge, levels = c('Grade3','Grade1&2'))
fit <- coxph(Surv(time_to_rfs, rfs_status)~age_surgery+sex+stage+pack_years+
               Adjuvant.therapy+type_surgery+IASLC_grade_path2_merge, data = data_stage)
pdf('./figure3b_path2_lattice.pdf', height = 7, width = 7, onefile = FALSE)
ggforest(fit, data=data_stage, fontsize = 0.8) 
dev.off()
summary(fit)
cox_zph <- cox.zph(fit)
ggcoxzph(cox_zph )

#path3
data_stage$IASLC_grade_path3_merge <- NULL
data_stage$IASLC_grade_path3_merge[data_stage$IASLC_grade_path3=='Grade3'] <- 'Grade3'
data_stage$IASLC_grade_path3_merge[data_stage$IASLC_grade_path3=='Grade2'] <- 'Grade1&2'
data_stage$IASLC_grade_path3_merge[data_stage$IASLC_grade_path3=='Grade1'] <- 'Grade1&2'
data_stage$IASLC_grade_path3_merge <- factor(data_stage$IASLC_grade_path3_merge, levels = c('Grade3','Grade1&2'))
fit <- coxph(Surv(time_to_rfs, rfs_status)~age_surgery+sex+stage+pack_years+
               Adjuvant.therapy+type_surgery+IASLC_grade_path3_merge, data = data_stage)
pdf('./figure3b_path3_lattice.pdf', height = 7, width = 7, onefile = FALSE)
ggforest(fit, data=data_stage, fontsize = 0.8) 
dev.off()
summary(fit)
cox_zph <- cox.zph(fit)
ggcoxzph(cox_zph )

#fig5g-LATTIECe-A
data_scattering <- latticeAll[which(latticeAll$scattering_continuous>0),]
data_scattering$scattering_cutoff <- factor(data_scattering$scattering_cutoff, levels = c('low','high'))
fit <- survfit(Surv(time_to_rfs, rfs_status)~scattering_cutoff, data = data_scattering)
pdf(file="./fig5g_lattice.pdf", height = 5, width = 6, onefile = FALSE)
ggsurvplot(fit, data = data_scattering, conf.int = FALSE, fun='event',
           pval = TRUE, pval.size = 5, pval.coord = c(0.2, 0.1),
           linetype = "solid",
           surv.plot.height = 0.7, palette = c("#1B9E77", "#D95F02"),
           risk.table = TRUE,
           risk.table.col = "black", break.time.by = 1000,
           tables.height = 0.25, 
           tables.theme = theme_cleantable(),
           risk.table.title = "Number at Risk",
           tables.x.text = "none", xlim = c(0, 6500), ylim=c(0,1),
           xlab = "Days to Recurrence", ylab = "DFS event")
dev.off()
fit <- coxph(Surv(time_to_rfs, rfs_status)~scattering_cutoff, data = data_scattering)
ggforest(fit, data=data_scattering, fontsize = 0.8) 
Anova(fit, type='III', test.statistic = 'Wald')

#fig5i-LATTIECe-A
data_scattering <- latticeAll[which(latticeAll$scattering_continuous>0),]
data_scattering$IASLC_grade_ai <- factor(data_scattering$IASLC_grade_ai, levels = c('Grade3', 'Grade2', 'Grade1'))
data_scattering$scattering_cutoff <- factor(data_scattering$scattering_cutoff, levels = c('low', 'high'))
fit <- coxph(Surv(time_to_rfs, rfs_status)~IASLC_grade_ai+scattering_cutoff, data = data_scattering)
pdf(file="./figS5i_lattice.pdf", height = 3, width = 6, onefile = FALSE)
ggforest(fit, data=data_scattering, fontsize = 0.8) 
dev.off()
summary(fit)
cox_zph <- cox.zph(fit)
ggcoxzph(cox_zph )
Anova(fit, type='III', test.statistic = 'Wald')
#scattering_cutoffhigh  0.41368   1.51238  0.09838  4.205 2.61e-05 ***

#extended data fig8-lattice
data_grade1 <- data_scattering[which(data_scattering$IASLC_grade_ai=='Grade1'),]
data_grade2 <- data_scattering[which(data_scattering$IASLC_grade_ai=='Grade2'),]
data_grade3 <- data_scattering[which(data_scattering$IASLC_grade_ai=='Grade3'),]
data_grade12 <- data_scattering[which(data_scattering$IASLC_grade_ai=='Grade1'| data_scattering$IASLC_grade_ai=='Grade2'),]
pdf(file="./extendDataFig8.pdf", height = 3, width = 6, onefile = FALSE)
fit <- coxph(Surv(time_to_rfs, rfs_status)~scattering_cutoff, data = data_grade12)
ggforest(fit, data=data_grade12, fontsize = 0.8) 
#dev.off()
Anova(fit, type='III', test.statistic = 'Wald')
summary(fit)
#grade3:  0.007446, n=570
#grade2: 1.947e-05, n=212
#grade1: 0.5397, n=55
#grade12: 1.387e-05, n=267

#fig5a
data_stage <- latticeAll[which(latticeAll$stage=='I'|
                                 latticeAll$stage=='II'|
                                 latticeAll$stage=='III'),]
data_stage$predom_ai <- factor(data_stage$predom_ai, levels=c('lep', 'pap', 'aci', 'cri', 'mic', 'sol'))
pdf(file="./fig5a_lattice.pdf", height = 4, width = 4)
ggplot(data_stage, aes(x=factor(stage), fill=factor(predom_ai)))+
  geom_bar(color='black',  position="fill")+
  geom_text(aes(label=..count..),stat='count',position=position_fill(vjust=0.5))+
  scale_fill_manual(values = c('lep'='#0000ff', 'pap'='#ffff00', 'aci'='#ff0000', 'cri'='#00ffff', 'mic'='#ff00ff','sol'='#880000'))+
  theme_classic()+labs(fill = "Pattern")
dev.off()

#fig5b
lep <- read_csv('/Users/xpan/Documents/code_R/lattice3979_individual_lep_pix.csv')
pap <- read_csv('/Users/xpan/Documents/code_R/lattice3979_individual_pap_pix.csv')
aci <- read_csv('/Users/xpan/Documents/code_R/lattice3979_individual_aci_pix.csv')
cri <- read_csv('/Users/xpan/Documents/code_R/lattice3979_individual_cri_pix.csv')
sol <- read_csv('/Users/xpan/Documents/code_R/lattice3979_individual_sol_pix.csv')
mic <- read_csv('/Users/xpan/Documents/code_R/lattice3979_individual_mic_pix.csv')
#to remove data with 0 pixel
lep <- lep[which(lep$lep_pix>0),]
pap <- pap[which(pap$pap_pix>0),]
aci <- aci[which(aci$aci_pix>0),] 
cri <- cri[which(cri$cri_pix>0),]
sol <- sol[which(sol$sol_pix>0),]
mic <- mic[which(mic$mic_pix>0),]

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
pdf(file="./fig5bpattern_pix_lattice.pdf", height = 4, width = 4)
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
pdf(file="./fig5bpattern_pix_lattice_remain.pdf", height = 4, width = 4)
ggplot(pattern_pix, aes(x=pix, group=pattern, color=pattern)) +
  geom_density() +
  scale_colour_manual(values = c('lep'="#0000ff",'pap'="#ffff00", 'aci'="#ff0000", 'cri'="#00ffff", 'mic'='#ff00ff', 'sol'='#880000'))+
  theme_classic()
dev.off()


#fig5c, 5d, 5f, extended data fig7b, 7c, 7d
data_aci <- latticeAll[which(latticeAll$aci_per>=0.05),]
data_aci$predom_ai <- factor(data_aci$predom_ai, levels=c('lep', 'pap', 'aci', 'cri', 'mic', 'sol'))
pdf(file="./extendDataFig7b_area_lattice.pdf", height = 4, width = 3)
ggplot(data_aci, aes(x=predom_ai, y=area_mean, fill=predom_ai)) +
  geom_boxplot(width=0.6,outlier.shape=NA) +
  geom_jitter(color="black", size=0.3, width=0.2) +
  scale_fill_manual(values = c('lep'="#0000ff",'pap'="#ffff00", 'aci'="#ff0000", 'cri'="#00ffff", 'mic'='#ff00ff', 'sol'='#880000'))+
  theme_classic() +
  theme(legend.position="none",plot.title = element_text(size=11)) +
  stat_compare_means()+
  xlab("")
dev.off()

pdf(file="./extendDataFig7b_soli_lattice.pdf", height = 4, width = 3)
ggplot(data_aci, aes(x=predom_ai, y=soli_mean, fill=predom_ai)) +
  geom_boxplot(width=0.6,outlier.shape=NA) +
  geom_jitter(color="black", size=0.3, width=0.2) +
  scale_fill_manual(values = c('lep'="#0000ff",'pap'="#ffff00", 'aci'="#ff0000", 'cri'="#00ffff", 'mic'='#ff00ff', 'sol'='#880000'))+
  theme_classic() +
  theme(legend.position="none",plot.title = element_text(size=11)) +
  stat_compare_means()+
  xlab("")
dev.off()
kruskal.test(soli_mean ~ predom_ai, data = data_aci) # n=654, p= 2.626e-10, Kruskal-Wallis rank sum test, one-way
pairwise.wilcox.test(data_aci$soli_mean, data_aci$predom_ai,p.adjust.method = "BH")

data_aci$predom_merge <- NULL
data_aci$predom_merge[data_aci$predom_ai=='lep'] <- 'Lep'
data_aci$predom_merge[data_aci$predom_ai=='aci' | data_aci$predom_ai=='pap'] <- 'Aci/Pap'
data_aci$predom_merge[data_aci$predom_ai=='cri' | data_aci$predom_ai=='mic'| data_aci$predom_ai=='sol'] <- 'Cri/Mic/Sol'
data_aci$predom_merge <- factor(data_aci$predom_merge, levels=c('Lep', 'Aci/Pap', 'Cri/Mic/Sol'))
my_comparison1 <- list( c("Lep", "Aci/Pap"), c("Aci/Pap", "Cri/Mic/Sol"), c("Lep", "Cri/Mic/Sol") )
pdf(file="./fig5c_area_mean_lattice.pdf", height = 4, width = 3)
ggplot(data_aci, aes(x=predom_merge, y=area_mean, fill=predom_merge)) +
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
wilcox.test(area_mean ~ predom_merge, data_aci1) #n=420,p=5.413e-12
wilcox.test(area_sd ~ predom_merge, data_aci1) #n=420,p=7.743e-09

data_aci2 <- data_aci[which(data_aci$predom_merge=='Aci/Pap' | data_aci$predom_merge=='Cri/Mic/Sol'),]
wilcox.test(area_mean ~ predom_merge, data_aci2) #n=593,p<2.2e-16
wilcox.test(area_sd ~ predom_merge, data_aci2)
pairwise.wilcox.test(data_aci$area_mean, data_aci$predom_merge,p.adjust.method = "BH")
#BH adjusted
#            Lep     Aci/Pap
#Aci/Pap     8.1e-12 -      
#Cri/Mic/Sol 0.038   < 2e-16

pdf(file="./fig5f_soli_mean_lattice.pdf", height = 4, width = 3)
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
wilcox.test(soli_mean ~ predom_merge, data_aci1) #n=420,p=0.0001426

data_aci2 <- data_aci[which(data_aci$predom_merge=='Aci/Pap' | data_aci$predom_merge=='Cri/Mic/Sol'),]
wilcox.test(soli_mean ~ predom_merge, data_aci2) #n=593,p=0.005615

data_aci3 <- data_aci[which(data_aci$predom_merge=='Lep' | data_aci$predom_merge=='Cri/Mic/Sol'),]
wilcox.test(soli_mean ~ predom_merge, data_aci3) #n=295,p=4.118e-07
wilcox.test(soli_sd ~ predom_merge, data_aci3) #p = 8.184e-16

data_pattern <- data_aci[which(data_aci$predom_ai=='aci'|data_aci$predom_ai=='cri'|data_aci$predom_ai=='sol'),]
my_comparison2 <- list( c("aci", "cri"), c("aci", "sol"), c("cri", "sol"))
pdf(file="./fig5d_aciCrisol_area_mean_lattice.pdf", height = 4, width = 3)
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
#BH non-adjusted, n=445
#      aci     cri 
#cri 1.5e-07 -   
#sol < 2e-16 0.83
data_pattern1 <- data_aci[which(data_aci$predom_ai=='aci' | data_aci$predom_ai=='cri'),]
wilcox.test(area_mean ~ predom_ai, data_pattern1) #n=290,p=1.515e-07

#fig5f
data_scattering$predom_ai <- factor(data_scattering$predom_ai, levels=c('lep', 'pap', 'aci', 'cri', 'mic', 'sol'))
pdf(file="./fig5f_predom_pattern_scattering_lattice.pdf", height = 4, width = 3)
ggplot(data_scattering, aes(x=factor(predom_ai), fill=factor(scattering_cutoff)))+
  geom_bar(color='black',  position="fill")+
  geom_text(aes(label=after_stat(count)),stat='count',position=position_fill(vjust=0.5))+
  scale_fill_brewer(palette='Set2')+
  theme_classic()+labs(fill = "Pattern")+xlab("Predominant subtype")+ylab("Proportion")
dev.off()
#fisher exact test
df <- data.frame('Lep'=c(54,11), 'Other'= c(88+164+35+23+196, 52+75+26+16+97), row.names = c('low', 'high'))
fisher.test(df) 
#n=837, p=0.00367

#fig3c
#step1 to get c-index, e.g. stage I
data_stage <-latticeAll[which(latticeAll$stage=='I'), ] #n=337
data_ci = data_stage[, c('time_to_rfs', 'rfs_status', 'age_surgery', 'sex','stage',
                         'pack_years', 'Adjuvant.therapy', 'type_surgery',
                         'IASLC_grade_ai', 'IASLC_grade_path1', 'IASLC_grade_path2', 'IASLC_grade_path3')]
names(data_ci)[names(data_ci)=='time_to_rfs'] <- 'time'
names(data_ci)[names(data_ci)=='rfs_status'] <- 'status'
D=CompCase(data.matrix(data_ci[, c(1:4)])) #since stage will be the same, thus excluded
tau=1825#quantile(data_ci$time)[3]
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)
#baseline: 0.5599 0.0331 0.4950 0.6247

D=CompCase(data.matrix(data_ci[, c(1:4,9)])) #since stage will be the same, thus excluded
tau=1825#quantile(data_ci$time)[3]
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)
#AI grading: 0.6428 0.0303 0.5835 0.7021

D=CompCase(data.matrix(data_ci[, c(1:4,10)])) #since stage will be the same, thus excluded
tau=1825#quantile(data_ci$time)[3]
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)
#Path1 grading: 0.6302 0.0306 0.5701 0.6902

D=CompCase(data.matrix(data_ci[, c(1:4,11)])) #since stage will be the same, thus excluded
tau=1825#quantile(data_ci$time)[3]
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)
#Path2 grading: 0.6153 0.0343 0.5481 0.6826

D=CompCase(data.matrix(data_ci[, c(1:4,12)])) #since stage will be the same, thus excluded
tau=1825#quantile(data_ci$time)[3]
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)
#Path3 grading: 0.5996 0.0374 0.5263 0.6729

D=CompCase(data.matrix(data_ci[, c(1:4, 9, 10)])) #since stage will be the same, thus excluded
tau=1825#quantile(data_ci$time)[3]
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)
#AI + Path1 grading: 0.6529 0.0312 0.5917 0.7141
#check multicollinearity
model_all <- lm(rfs_status ~ age_surgery + sex +IASLC_grade_ai+IASLC_grade_path1, data=data_stage)  # with all the independent variables in the dataframe
vif(model_all)
#                      GVIF Df GVIF^(1/(2*Df))
#age_surgery       1.019225  1        1.009567
#sex               1.021340  1        1.010614
#IASLC_grade_ai    1.780805  2        1.155192
#IASLC_grade_path1 1.800874  2        1.158433

D=CompCase(data.matrix(data_ci[, c(1:4, 9, 11)])) #since stage will be the same, thus excluded
tau=1825#quantile(data_ci$time)[3]
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)
#AI + Path2 grading: 0.6429 0.0309 0.5823 0.7035
#check multicollinearity
model_all <- lm(rfs_status ~ age_surgery + sex +IASLC_grade_ai+IASLC_grade_path2, data=data_stage)  # with all the independent variables in the dataframe
vif(model_all)

D=CompCase(data.matrix(data_ci[, c(1:4, 9, 12)])) #since stage will be the same, thus excluded
tau=1825#quantile(data_ci$time)[3]
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)
#AI + Path3 grading: 0.6424 0.0296 0.5844 0.7004
#check multicollinearity
model_all <- lm(rfs_status ~ age_surgery + sex +IASLC_grade_ai+IASLC_grade_path3, data=data_stage)  # with all the independent variables in the dataframe
vif(model_all)

#similar process to get c-indes for stage I-III tumors
#...

#step2 to plot c-index
my_color <-brewer.pal(8, "Set1")
summaryCsAP <- data.frame(
  Measure = c('Baseline', 'AI', 'Path 1', 'AI + Path 1','Path 2', 'AI + Path 2', 'Path 3', 'AI + Path 3'),
  lower95s1 = c(0.4950, 0.5835, 0.5701, 0.5917, 0.5481, 0.5823, 0.5263, 0.5844),
  upper95s1 = c(0.6247, 0.7021, 0.6902, 0.7141, 0.6826, 0.7035, 0.6729, 0.7004), 
  Cinds1 = c(0.5599, 0.6428,  0.6302, 0.6529, 0.6153, 0.6429, 0.5996, 0.6424),
  lower95s13 = c(0.6334, 0.6502, 0.6449, 0.6511, 0.6465, 0.6505, 0.6437, 0.6500),
  upper95s13 = c(0.6969, 0.7133, 0.7127, 0.7170, 0.7128, 0.7177, 0.7067, 0.7134), 
  Cinds13 = c(0.6652, 0.6817, 0.6788, 0.6840, 0.6797, 0.6841, 0.6752, 0.6817))
summaryCsAP$Measure <- factor(summaryCsAP$Measure, levels=c('Baseline', 'AI', 'Path 1', 'AI + Path 1','Path 2', 'AI + Path 2', 'Path 3', 'AI + Path 3'))
pdf(file="./fig3c_stage1_cindexCI_ai_path_lattice.pdf", height = 3, width = 3)
ggplot(data=summaryCsAP,aes(x=Measure,y=Cinds1, ymin = lower95s1, ymax = upper95s1, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c('#000000', my_color[1],my_color[2], "#80B1D3", my_color[5], "#FDB462", "#7C0B78", "#EF44E8"), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.48,0.72)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()

pdf(file="./fig3c_stage13_cindexCI_ai_path_lattice.pdf", height = 3, width = 3)
ggplot(data=summaryCsAP,aes(x=Measure,y=Cinds13, ymin = lower95s13, ymax = upper95s13, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c('#000000', my_color[1],my_color[2], "#80B1D3", my_color[5], "#FDB462", "#7C0B78", "#EF44E8"), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.48,0.72)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()


#fig4a - Scenario 1, high shannon index
#data_stage <- latticeAll[which(latticeAll$p_stage=='I'),]
#shannon distribution
median(data_stage$shannon_path1) #1.054016
median(data_stage$shannon_path2) #1.295462
median(data_stage$shannon_path3) #0.9927745
shannon_div <- data.frame(meassure=c(rep('AI', length(data_stage$shannon_ai)), rep('Path1', length(data_stage$shannon_path1)), 
                                     rep('Path2', length(data_stage$shannon_path2)), rep('Path3', length(data_stage$shannon_path3))),
                          shannon_gp = c(data_stage$shannon_ai, data_stage$shannon_path1, data_stage$shannon_path2, data_stage$shannon_path3))
shannon_div$meassure <-factor(shannon_div$meassure, levels=c('AI', 'Path1', 'Path2', 'Path3'))
pdf(file="./fig4a_shannon_distribution_median_stage1.pdf", height = 4, width = 4.5)
ggplot(shannon_div, aes(x=shannon_gp,  color=meassure)) +
  geom_density() +
  scale_colour_manual(values = c('AI'=my_color[1],'Path1'=my_color[2], 'Path2'=my_color[5], 'Path3'="#7C0B78"))+
  #scale_colour_brewer(palette='Set1')+
  scale_x_continuous(breaks=seq(0, 2.6, 0.5))+
  geom_vline(xintercept = 1.05, linetype="dashed", color = my_color[2], linewidth=0.5)+
  geom_vline(xintercept = 1.30, linetype="dashed", color = my_color[5], linewidth=0.5)+
  geom_vline(xintercept = 1.0, linetype="dashed", color = "#7C0B78", linewidth=0.5)+
  theme_classic()
dev.off()

#c-index@Scenario 1
#subset based on path1
data_shannon <- data_stage[which(data_stage$shannon_path1>=1.05),]
data_ci = data_shannon[, c('time_to_rfs', 'rfs_status', 'age_surgery', 'sex','stage',
                           'IASLC_grade_ai', 'IASLC_grade_path1', 'IASLC_grade_path2', 'IASLC_grade_path3')]
names(data_ci)[names(data_ci)=='time_to_rfs'] <- 'time'
names(data_ci)[names(data_ci)=='rfs_status'] <- 'status'
D=CompCase(data.matrix(data_ci[, c(1:4)])) 
tau=1825
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)
#baseline:0.5375 0.0655 0.4093 0.6657

D=CompCase(data.matrix(data_ci[, c(1:4, 6)])) 
tau=1825
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)
#AI grading: 0.6024 0.0602 0.4845 0.7203

D=CompCase(data.matrix(data_ci[, c(1:4, 7)])) 
tau=1825
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)
#path1 grading:  0.5903 0.0606 0.4715 0.7091
#similar process for computing c-index for subsets based on path2 and path3

#to plot fig4a and extended data fig6a
summaryC1 <- data.frame(
  Measure = c('Baseline', 'Path x', 'AI'),
  lower95s1P1 = c(0.4093, 0.4715, 0.4845),
  upper95s1P1 = c(0.6657, 0.7091, 0.7203), 
  Cinds1P1 = c(0.5375, 0.5903, 0.6024),
  lower95s13P1 = c(0.6280, 0.6359, 0.6400),
  upper95s13P1 = c(0.6983, 0.7106, 0.7089), 
  Cinds13P1 = c(0.6631, 0.6732, 0.6745),
  
  lower95s1P2 = c(0.4344, 0.4528, 0.4968),
  upper95s1P2 = c(0.6784, 0.6919, 0.7064), 
  Cinds1P2 = c(0.5564, 0.5723, 0.6016),
  lower95s13P2 = c(0.6233, 0.6307, 0.6285),
  upper95s13P2 = c(0.6945, 0.7028, 0.7051), 
  Cinds13P2 = c(0.6589, 0.6667, 0.6668),
  
  lower95s1P3 = c(0.4616, 0.4936, 0.5369),
  upper95s1P3 = c(0.6750, 0.6631, 0.7039), 
  Cinds1P3 = c(0.5683, 0.5783, 0.6204),
  lower95s13P3 = c(0.6184, 0.6262, 0.6315),
  upper95s13P3 = c(0.6895, 0.6990, 0.7007), 
  Cinds13P3 = c(0.6540, 0.6626, 0.6661))
summaryC1$Measure <- factor(summaryC1$Measure, levels=c('Baseline', 'Path x', 'AI'))
#Path1
pdf(file="./fig4a_stage1_cindexCI_Scene1P1_lattice.pdf", height = 3, width = 2)
ggplot(data=summaryC1,aes(x=Measure,y=Cinds1P1, ymin = lower95s1P1, ymax = upper95s1P1, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c("#000000",my_color[2],my_color[1]), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.4,0.73)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()

pdf(file="./figS6a_stage13_cindexCI_Scene1P1_lattice.pdf", height = 3, width = 2)
ggplot(data=summaryC1,aes(x=Measure,y=Cinds13P1, ymin = lower95s13P1, ymax = upper95s13P1, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c("#000000",my_color[2],my_color[1]), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.48,0.72)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()


#Path2
pdf(file="./fig4a_stage1_cindexCI_Scene1P2_lattice.pdf", height = 3, width = 2)
ggplot(data=summaryC1,aes(x=Measure,y=Cinds1P2, ymin = lower95s1P2, ymax = upper95s1P2, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c("#000000",my_color[5],my_color[1]), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.4,0.73)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()

pdf(file="./figS6a_stage13_cindexCI_Scene1P2_lattice.pdf", height = 3, width = 2)
ggplot(data=summaryC1,aes(x=Measure,y=Cinds13P2, ymin = lower95s13P2, ymax = upper95s13P2, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c("#000000",my_color[5],my_color[1]), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.48,0.72)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()


#Path3
pdf(file="./fig4a_stage1_cindexCI_Scene1P3_lattice.pdf", height = 3, width = 2)
ggplot(data=summaryC1,aes(x=Measure,y=Cinds1P3, ymin = lower95s1P3, ymax = upper95s1P3, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c("#000000","#7C0B78",my_color[1]), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.4,0.73)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()

pdf(file="./figS6a_stage13_cindexCI_Scene1P3_lattice.pdf", height = 3, width = 2)
ggplot(data=summaryC1,aes(x=Measure,y=Cinds13P3, ymin = lower95s13P3, ymax = upper95s13P3, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c("#000000","#7C0B78",my_color[1]), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.48,0.72)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()

#fig4a - Scenario 2, lep vs. aci and lep vs. pap
data_stage <- latticeAll[which(latticeAll$stage=='I'),]
data_aci <- data_stage[which((data_stage$predom_path1=='lep'& data_stage$lep_path1<0.9) | 
                                (data_stage$predom_path1=='aci' & data_stage$aci_path1<0.9)),]
data_ci = data_aci[, c('time_to_rfs', 'rfs_status', 'age_surgery', 'sex', 'stage',
                        'IASLC_grade_ai', 'IASLC_grade_path1','IASLC_grade_path2', 'IASLC_grade_path3')]
names(data_ci)[names(data_ci)=='time_to_rfs'] <- 'time'
names(data_ci)[names(data_ci)=='rfs_status'] <- 'status'
D=CompCase(data.matrix(data_ci[, c(1:4)])) 
tau=1825#quantile(data_ci$time)[3]
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)

D=CompCase(data.matrix(data_ci[, c(1:4, 6)])) 
tau=1825#quantile(data_ci$time)[3]
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)

D=CompCase(data.matrix(data_ci[, c(1:4, 7)])) 
tau=1825#quantile(data_ci$time)[3]
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)
#similar process for computing c-index for subsets based on path2 and path3
#baseline:  0.5834 0.0757 0.4351 0.7316
#path1: 0.6156 0.0524 0.5130 0.7183
#ai: 0.6577 0.0571 0.5458 0.7696

#baseline: 0.5783 0.0434 0.4933 0.6633
#path2:  0.5865 0.0553 0.4782 0.6948
#ai: 0.6207 0.0462 0.5302 0.7113

#baseline:  0.5918 0.0430 0.5076 0.6760
#path3:  0.5994 0.0448 0.5117 0.6872
#ai: 0.7031 0.0398 0.6252 0.7810
summaryC2A <- data.frame(
  Measure = c('Baseline', 'Path x', 'AI'),
  lower95s1P1 = c(0.4351, 0.5130, 0.5458),
  upper95s1P1 = c(0.7316, 0.7183, 0.7696), 
  Cinds1P1 = c(0.5834, 0.6156, 0.6577),
  lower95s13P1 = c(0.6196, 0.6256, 0.6456),
  upper95s13P1 = c(0.7299, 0.7430, 0.7543), 
  Cinds13P1 = c(0.6748, 0.6843, 0.6999),
  
  lower95s1P2 = c(0.4933, 0.4782, 0.5302),
  upper95s1P2 = c(0.6633, 0.6948, 0.7113), 
  Cinds1P2 = c(0.5783, 0.5865, 0.6207),
  lower95s13P2 = c(0.6139, 0.6165, 0.6271),
  upper95s13P2 = c(0.7193, 0.7307, 0.7343), 
  Cinds13P2 = c(0.6666, 0.6736, 0.6807),
  
  lower95s1P3 = c(0.5076, 0.5117, 0.6252),
  upper95s1P3 = c(0.6760, 0.6872, 0.7810), 
  Cinds1P3 = c(0.5918, 0.5994, 0.7031),
  lower95s13P3 = c(0.6185, 0.6215, 0.6448),
  upper95s13P3 = c(0.7165, 0.7289, 0.7388), 
  Cinds13P3 = c(0.6675, 0.6752, 0.6918))
summaryC2A$Measure <- factor(summaryC2A$Measure, levels=c('Baseline', 'Path x', 'AI'))


#to plot fig4b-increament between PathX and baseline, bewtween AI and baseline
cindexC2 <- data.frame(para=c('Path 1', 'AI1', 'Path 2', 'AI2', 'Path 3', 'AI3'),
                       Stage1=c(0.0322, 0.0743, 0.0082, 0.0424, 0.0076, 0.1113))
cindexC2$para <- factor(cindexC2$para, levels=c('AI3','Path 3',  'AI2', 'Path 2',  'AI1','Path 1'))
#plot-increament
pdf(file="./fig4b_stage1_aci.pdf", height = 2, width = 2.5)
ggplot(cindexC2, aes(x=para, y=Stage1)) +
  geom_bar(stat="identity", fill="#bebada", width=.3) +
  coord_flip() +
  xlab("") +
  theme_classic()
dev.off()

#extended data fig6b
pdf(file="./figs6b_stage13_cindexCI_AciP1_lattice.pdf", height = 3, width = 2)
ggplot(data=summaryC2A,aes(x=Measure,y=Cinds13P1, ymin = lower95s13P1, ymax = upper95s13P1, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c("#000000",my_color[2],my_color[1]), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.48,0.79)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()

pdf(file="./figs6b_stage13_cindexCI_AciP2_lattice.pdf", height = 3, width = 2)
ggplot(data=summaryC2A,aes(x=Measure,y=Cinds13P2, ymin = lower95s13P2, ymax = upper95s13P2, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c("#000000",my_color[5],my_color[1]), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.48,0.79)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()

pdf(file="./figs6b_stage13_cindexCI_AciP3_lattice.pdf", height = 3, width = 2)
ggplot(data=summaryC2A,aes(x=Measure,y=Cinds13P3, ymin = lower95s13P3, ymax = upper95s13P3, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c("#000000", "#7C0B78",my_color[1]), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.48,0.79)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()

#similar to get plots for fig4b lep vs. pap

#fig4c
#distribution
latticeAll$high_per <- latticeAll$cri_per + latticeAll$mic_per + latticeAll$sol_per
latticeAll$high_path1 <- latticeAll$cri_path1 + latticeAll$mic_path1 + latticeAll$sol_path1
latticeAll$high_path2 <- latticeAll$cri_path2 + latticeAll$mic_path2 + latticeAll$sol_path2
latticeAll$high_path3 <- latticeAll$cri_path3 + latticeAll$mic_path3 + latticeAll$sol_path3
data_stage <- latticeAll[which(latticeAll$stage=='I'),]
high_grade_per <- data.frame(meassure=c(rep('AI', length(data_stage$high_per)), rep('Path1', length(data_stage$high_path1)), 
                                        rep('Path2', length(data_stage$high_path2)), rep('Path3', length(data_stage$high_path3))),
                             high_per = c(data_stage$high_per, data_stage$high_path1, data_stage$high_path2, data_stage$high_path3),
                             mic_per = c(data_stage$mic_per, data_stage$mic_path1, data_stage$mic_path2, data_stage$mic_path3))


high_grade_per$meassure <-factor(high_grade_per$meassure, levels=c('AI', 'Path1', 'Path2', 'Path3'))
pdf(file="./fig4c_distribution_stage1.pdf", height = 4, width = 4)
ggplot(high_grade_per, aes(x=high_per,  color=meassure)) +
  geom_density() +
  scale_colour_manual(values = c('AI'=my_color[1],'Path1'=my_color[2], 'Path2'=my_color[5], 'Path3'="#7C0B78"))+
  #scale_colour_brewer(palette='Set1')+
  scale_x_continuous(breaks=seq(0, 1, 0.2))+
  geom_vline(xintercept = 0.05, linetype="dashed", color = "black", linewidth=0.5)+
  geom_vline(xintercept = 0.3, linetype="dashed", color = "black", linewidth=0.5)+
  theme_classic()
dev.off()

#compute c-index, e.g. path1
data_stage_high <- data_stage[which(data_stage$high_path1<=0.3 & data_stage$high_path1>=0.05),]
data_ci = data_stage_high[, c('time_to_rfs', 'rfs_status', 'age_surgery', 'sex','stage', 
                         'IASLC_grade_ai', 'IASLC_grade_path1', 'IASLC_grade_path2', 'IASLC_grade_path3')]
names(data_ci)[names(data_ci)=='time_to_rfs'] <- 'time'
names(data_ci)[names(data_ci)=='rfs_status'] <- 'status'
D=CompCase(data.matrix(data_ci[, c(1:4)])) 
tau=1825#quantile(data_ci$time)[3]
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)

D=CompCase(data.matrix(data_ci[, c(1:4, 6)])) 
tau=1825#quantile(data_ci$time)[3]
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)

D=CompCase(data.matrix(data_ci[, c(1:4, 7)])) 
tau=1825#quantile(data_ci$time)[3]
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)

#similar process to compute c-index for subsets based on Path2 and Path3
summaryC3 <- data.frame(
  Measure = c('Baseline', 'Path x', 'AI'),
  lower95s1P1 = c(0.3841, 0.3915, 0.4863),
  upper95s1P1 = c(0.7638, 0.7565, 0.7761), 
  Cinds1P1 = c(0.5739, 0.5740, 0.6312),
  lower95s13P1 = c(0.5661, 0.5792, 0.5669),
  upper95s13P1 = c(0.7243, 0.7293, 0.7315), 
  Cinds13P1 = c(0.6452, 0.6543, 0.6492),
  
  lower95s1P2 = c(0.3259, 0.3134, 0.3474),
  upper95s1P2 = c(0.6879, 0.6957, 0.7727), 
  Cinds1P2 = c(0.5069, 0.5045, 0.5601),
  lower95s13P2 = c(0.5487, 0.5484, 0.5798),
  upper95s13P2 = c(0.7705, 0.7704, 0.7947), 
  Cinds13P2 = c(0.6596, 0.6594, 0.6873),
  
  lower95s1P3 = c(0.4493, 0.4614, 0.5209),
  upper95s1P3 = c(0.7290, 0.7532, 0.7592), 
  Cinds1P3 = c(0.5892, 0.6073, 0.6400),
  lower95s13P3 = c(0.5926, 0.5932, 0.6041),
  upper95s13P3 = c(0.7190, 0.7189, 0.7216), 
  Cinds13P3 = c(0.6558, 0.6560, 0.6629))
summaryC3$Measure <- factor(summaryC3$Measure, levels=c('Baseline', 'Path x', 'AI'))

#plot fig4c and extended data fig6c
pdf(file="./fig4c_stage1_cindexCI_Scene3P1_lattice.pdf", height = 3, width = 2)
ggplot(data=summaryC3,aes(x=Measure,y=Cinds1P1, ymin = lower95s1P1, ymax = upper95s1P1, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c("#000000",my_color[2],my_color[1]), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.31,0.78)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()

pdf(file="./figS6c_stage13_cindexCI_Scene3P1_lattice.pdf", height = 3, width = 2)
ggplot(data=summaryC3,aes(x=Measure,y=Cinds13P1, ymin = lower95s13P1, ymax = upper95s13P1, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c("#000000",my_color[2],my_color[1]), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.48,0.80)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()

#Path2
pdf(file="./fig4c_stage1_cindexCI_Scene3P2_lattice.pdf", height = 3, width = 2)
ggplot(data=summaryC3,aes(x=Measure,y=Cinds1P2, ymin = lower95s1P2, ymax = upper95s1P2, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c("#000000",my_color[5],my_color[1]), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.31,0.78)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()

pdf(file="./figS6c_stage13_cindexCI_Scene3P2_lattice.pdf", height = 3, width = 2)
ggplot(data=summaryC3,aes(x=Measure,y=Cinds13P2, ymin = lower95s13P2, ymax = upper95s13P2, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c("#000000",my_color[5],my_color[1]), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.48,0.80)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()

#Path3
pdf(file="./fig4c_stage1_cindexCI_Scene3P3_lattice.pdf", height = 3, width = 2)
ggplot(data=summaryC3,aes(x=Measure,y=Cinds1P3, ymin = lower95s1P3, ymax = upper95s1P3, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c("#000000", "#7C0B78",my_color[1]), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.31,0.78)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()

pdf(file="./figS6c_stage13_cindexCI_Scene3P3_lattice.pdf", height = 3, width = 2)
ggplot(data=summaryC3,aes(x=Measure,y=Cinds13P3, ymin = lower95s13P3, ymax = upper95s13P3, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c("#000000", "#7C0B78",my_color[1]), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.48,0.80)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()

#fig4d, extended data fig6d, Scenario 4-number of slides
#distribution
data_slide1 <- data_stage[which(data_stage$num_slide>=1),]
kappa(data_slide1[c('predom_path1', 'predom_path3')])
kappa(data_slide1[c('predom_path1', 'predom_path2')])
kappa(data_slide1[c('predom_path2', 'predom_path3')])
data_slide2 <- data_stage[which(data_stage$num_slide>=2),]
kappa(data_slide2[c('predom_path1', 'predom_path3')])
kappa(data_slide2[c('predom_path1', 'predom_path2')])
kappa(data_slide2[c('predom_path2', 'predom_path3')])
data_slide3 <- data_stage[which(data_stage$num_slide>=3),]
kappa(data_slide3[c('predom_path1', 'predom_path3')])
kappa(data_slide3[c('predom_path1', 'predom_path2')])
kappa(data_slide3[c('predom_path2', 'predom_path3')])
ka_val <- data.frame(meassure=c('N>=1', 'N>=2', 'N>=3', 'N>=4', 'N>=5'), 
                     Path13 = c(0.6119, 0.6143, 0.6045, 0.584, 0.5353),
                     Path12 = c(0.4093,	0.4081,	0.4041,	0.3627,	0.3806),
                     Path23 = c(0.3948,	0.3871,	0.3829,	0.3594,	0.3312),
                     Path_avg = c(0.472,	0.4698,	0.4638,	0.4354,	0.4157))
ka_val_long <- pivot_longer(data = ka_val, cols = starts_with("Path"), names_to = "name") 
pdf(file="./fig4Kappa_num_slide_stage1.pdf", height = 4, width = 5)
ggplot(ka_val_long, aes(x=meassure, y=value)) +
  geom_line(aes(group=name),color="black") +
  geom_point(aes(fill=name),shape=21, size=4) +
  scale_fill_brewer(palette='Set3')+
  theme_classic() 
dev.off()

pdf(file="./fig4_distribution_stage1.pdf", height = 4, width = 3)
ggplot(data_stage, aes(x=num_slide)) +
  geom_histogram(binwidth=1, aes(y=after_stat(density)), fill="#e5a84b", color="black") +  
  scale_x_continuous(breaks=seq(0, 13, 1))+
  theme_classic()
dev.off()

#compute c-index
median(data_stage$num_slide)
data_slide <-data_stage[which(data_stage$num_slide>=4), ]
data_ci = data_slide[, c('time_to_rfs', 'rfs_status', 'age_surgery', 'sex', 'stage',
                         'IASLC_grade_ai', 'IASLC_grade_path1','IASLC_grade_path2', 'IASLC_grade_path3')]
names(data_ci)[names(data_ci)=='time_to_rfs'] <- 'time'
names(data_ci)[names(data_ci)=='rfs_status'] <- 'status'
D=CompCase(data.matrix(data_ci[, c(1:4)])) 
tau=1825
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)

D=CompCase(data.matrix(data_ci[, c(1:4, 6)])) 
tau=1825
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)

D=CompCase(data.matrix(data_ci[, c(1:4, 7)])) 
tau=1825
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)

D=CompCase(data.matrix(data_ci[, c(1:4, 8)])) 
tau=1825
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)

D=CompCase(data.matrix(data_ci[, c(1:4, 9)])) 
tau=1825
set.seed(2022)
C=Inf.Cval(D, tau, itr=100)
round(c(C$Dhat, C$se, C$low95, C$upp95), digits=4)

#plot c-index, fig4d, extended data fig6d
summaryC4 <- data.frame(
  Measure = c('Baseline', 'Path 1', 'Path 2','Path 3','AI'),
  lower95s1 = c(0.4886, 0.5814, 0.5510, 0.5229, 0.5771),
  upper95s1 = c(0.6339, 0.7088, 0.6797, 0.6906, 0.6988), 
  Cinds1 = c(0.5612, 0.6451, 0.6154, 0.6068, 0.6380),
  lower95s13 = c(0.6292, 0.6390, 0.6419, 0.6368, 0.6422),
  upper95s13 = c(0.6975, 0.7131, 0.7116, 0.7075, 0.7104), 
  Cinds13 = c(0.6633, 0.6761, 0.6768, 0.6721, 0.6763))
summaryC4$Measure <- factor(summaryC4$Measure, levels=c('Baseline', 'Path 1', 'Path 2','Path 3','AI'))

pdf(file="./fig4d_stage1_cindexCI_Scene4_lattice.pdf", height = 3, width = 2)
ggplot(data=summaryC4,aes(x=Measure,y=Cinds1, ymin = lower95s1, ymax = upper95s1, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c("#000000",my_color[2],my_color[5], "#7C0B78",my_color[1]), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.48,0.71)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()

pdf(file="./figS6d_stage13_cindexCI_Scene4_lattice.pdf", height = 3, width = 2)
ggplot(data=summaryC4,aes(x=Measure,y=Cinds13, ymin = lower95s13, ymax = upper95s13, colour=Measure))+
  geom_linerange(color="black")+
  geom_point(shape=21, color="black", fill=c("#000000",my_color[2],my_color[5], "#7C0B78",my_color[1]), size=3)+
  geom_hline(yintercept=0.5,linetype="longdash", colour="grey70")+
  theme_classic()+
  labs(y='C-index')+
  ylim(0.48,0.72)+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 6)) #adjust y text
dev.off()

#supplementary fig1-7
aci_morph_rand1 <- read_csv('/Users/xpan/Documents/code_R/lattice3979_solidity_area_allAcinar_random2Rebuttal.csv')
aci_morph_rand2 <- read_csv('/Users/xpan/Documents/code_R/lattice3979_solidity_area_allAcinar_random3Rebuttal.csv')
names(aci_morph_rand1)[names(aci_morph_rand1) == 'ID'] <- 'ACA_ID'
names(aci_morph_rand2)[names(aci_morph_rand2) == 'ID'] <- 'ACA_ID'
aci_morph_rand3 <- merge(aci_morph_rand1, aci_morph_rand2, by='ACA_ID')
write.csv(aci_morph_rand3, '/Users/xpan/Documents/code_R/lattice3979_solidity_area_allAcinar_random10.csv')
lattice_sup <- merge(latticeAll, aci_morph_rand1, by='ACA_ID')
lattice_sup <- merge(lattice_sup, aci_morph_rand2, by='ACA_ID')

data_sup <- lattice_sup[which(lattice_sup$aci_per>=0.05),]
data_sup$predom_ai <- factor(data_sup$predom_ai, levels=c('lep', 'pap', 'aci', 'cri', 'mic', 'sol'))

#figS5-LATTICe-A
#for area mean: P<2.2e10-16
var_list1 <- c("area_meanQ1", "area_meanQ2", "area_meanQ3", "area_meanQ4", "area_meanQ5", 
               "area_meanQ6", "area_meanQ7", "area_meanQ8", "area_meanQ9", "area_meanQ10")
#for solidity mean
var_list2 <- c("soli_meanQ1", "soli_meanQ2", "soli_meanQ3", "soli_meanQ4", "soli_meanQ5", 
              "soli_meanQ6", "soli_meanQ7", "soli_meanQ8", "soli_meanQ9", "soli_meanQ10")

p_figs5 <- numeric(length(var_list2))
for (i in 1:length(var_list2)) {
  formula <- as.formula(paste(var_list2[i], "~ predom_ai"))
  result <- kruskal.test(formula, data = data_sup)
  p_figs5[i] <- result$p.value
}
print(p_figs5)
#[1] 1.087018e-08 5.718833e-11 7.863343e-09 4.702437e-10 4.823926e-09 1.664403e-10 2.940408e-09
#[8] 3.137030e-10 6.110131e-08 2.951022e-11

#figS1-LATTICe-A
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
#[1] 8.202093e-12 8.267623e-12 2.606164e-11 2.430580e-12 2.295707e-12 3.139325e-11 4.084449e-12
#[8] 1.248321e-11 4.837327e-12 1.162686e-11 #n=420

data_sup2 <- data_sup[which(data_sup$predom_merge=='Aci/Pap' | data_sup$predom_merge=='Cri/Mic/Sol'),]
p_figs1b <- numeric(length(var_list1))
for (i in 1:length(var_list1)) {
  formula <- as.formula(paste(var_list1[i], "~ predom_merge"))
  result <- wilcox.test(formula, data = data_sup2)
  p_figs1b[i] <- result$p.value
}
print(p_figs1b)
#p<2.22e-16 #n=593
# [1] 9.803442e-20 1.364427e-18 1.340782e-18 1.048945e-19 7.080412e-20 1.255661e-18 1.586379e-19
# [8] 6.642250e-19 3.558821e-19 2.437477e-19

#figS2-LATTICe-A
data_sup3 <- data_sup[which(data_sup$predom_ai=='aci' | data_sup$predom_ai=='cri'),]
p_figs2a <- numeric(length(var_list1))
for (i in 1:length(var_list1)) {
  formula <- as.formula(paste(var_list1[i], "~ predom_merge"))
  result <- wilcox.test(formula, data = data_sup3)
  p_figs2a[i] <- result$p.value
}
print(p_figs2a)
#[1] 5.277080e-08 5.242959e-07 5.828643e-07 5.222431e-08 3.582194e-08 5.828643e-07 2.086572e-07
#[8] 1.341967e-07 1.152795e-07 2.281907e-07 #n=290,

data_sup4 <- data_sup[which(data_sup$predom_ai=='aci' | data_sup$predom_ai=='sol'),]
p_figs2b <- numeric(length(var_list1))
for (i in 1:length(var_list1)) {
  formula <- as.formula(paste(var_list1[i], "~ predom_merge"))
  result <- wilcox.test(formula, data = data_sup4)
  p_figs2b[i] <- result$p.value
}
print(p_figs2b)
#[1] 2.631157e-18 1.694659e-18 2.224947e-18 1.942010e-18 8.684552e-19 4.075246e-18 1.071408e-18
#[8] 4.737477e-18 1.420237e-18 1.911154e-18, n=395

#figS3-LATTICe-A
data_sup5 <- data_sup[which(data_sup$predom_merge=='Lep' | data_sup$predom_merge=='Cri/Mic/Sol'),]
p_figs3 <- numeric(length(var_list2))
for (i in 1:length(var_list2)) {
  formula <- as.formula(paste(var_list2[i], "~ predom_merge"))
  result <- wilcox.test(formula, data = data_sup5)
  p_figs3[i] <- result$p.value
}
print(p_figs3)
#[1] 1.004164e-05 7.231972e-08 1.306729e-05 2.454135e-07 1.314198e-06 8.070471e-07 6.411760e-06
#[8] 2.733602e-07 1.972597e-05 1.489452e-07 #n=295,p=4.118e-07

#figS6-LATTICe-A
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
#[1] 4.942281e-08 3.723435e-09 6.715819e-08 2.031233e-09 2.209578e-09 3.944803e-08 1.524957e-08
#[8] 7.484980e-09 1.014271e-08 2.040484e-08, n=420

p_figs6b <- numeric(length(var_list3))
for (i in 1:length(var_list3)) {
  formula <- as.formula(paste(var_list3[i], "~ predom_merge"))
  result <- wilcox.test(formula, data = data_sup2)
  p_figs6b[i] <- result$p.value
}
print(p_figs6b)
#[1] 1.035881e-16 1.675107e-14 8.158602e-15 3.507372e-16 2.325343e-16 5.159631e-15 3.960935e-16
#[8] 5.912792e-15 4.400443e-16 1.887863e-15, n=593

#figS7-LATTICe-A
p_figs7 <- numeric(length(var_list4))
for (i in 1:length(var_list4)) {
  formula <- as.formula(paste(var_list4[i], "~ predom_merge"))
  result <- wilcox.test(formula, data = data_sup5)
  p_figs7[i] <- result$p.value
}
print(p_figs7)
#[1] 1.248395e-14 1.037369e-14 4.600002e-15 4.919001e-15 1.079409e-14 1.860518e-15 4.671585e-14
#[8] 1.019181e-16 1.478174e-13 6.654206e-16, n=295

