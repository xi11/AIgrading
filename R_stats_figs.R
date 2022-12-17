####Artificial Intelligence-based histopathological grading of lung adenocarcinoma###
###statistical analyses and figures

library("survival")
library("survminer")
library(ggpubr)
library(RColorBrewer)
library(ggcorrplot)
library(ggstatsplot)
library(survC1) 
library('car')
library("irr")
set.seed(2022)

###load data
load('/Users/xpan/Documents/code_R/tx421GP_17June.RData')
load("/Users/xpan/Documents/code_R/lattice_10June.RData")
load('/Users/xpan/Documents/code_R/TCGA_9Aug.RData')
load('/Users/xpan/Documents/code_R/DHMC_19Aug.Rdata')

###fig2b
patternRhoTX <- function(pattern){
  pattern <- unlist(pattern)
  y<-as.numeric(data_tx421[,pattern[2]])
  x<-as.numeric(data_tx421[,pattern[1]])
  dd<-cor.test(x, y, method='spearman',  exact = FALSE)
  data.frame(path='PathTX', pattern=pattern[1], cor=dd$estimate, p.value=dd$p.value)
}

val_list = list(c('lep_per','lep_path'), c('pap_per','pap_path'), c('aci_per','aci_path'), 
                c('cri_per','cri_path'), c('sol_per','sol_path'), c('mic_per','mic_path'))
pathTXrho = do.call(rbind, lapply(val_list, patternRhoTX))
pathTXrho$Padjust = p.adjust(pathTXrho$p.value, method = "BH")


patternRho <- function(pattern){
  pattern <- unlist(pattern)
  y<-as.numeric(data_lattice[,pattern[2]])
  x<-as.numeric(data_lattice[,pattern[1]])
  dd<-cor.test(x, y, method='spearman',  exact = FALSE)
  data.frame(path='Path3', pattern=pattern[1], cor=dd$estimate, p.value=dd$p.value)
}

val_list1 = list(c('lep_per','lep_path1'), c('pap_per','pap_path1'), c('aci_per','aci_path1'), 
                 c('cri_per','cri_path1'), c('sol_per','sol_path1'), c('mic_per','mic_path1'))
path1rho = do.call(rbind, lapply(val_list1, patternRho))
path1rho$Padjust = p.adjust(path1rho$p.value, method = "BH")

val_list2 = list(c('lep_per','lep_path2'), c('pap_per','pap_path2'), c('aci_per','aci_path2'), 
                 c('cri_per','cri_path2'), c('sol_per','sol_path2'), c('mic_per','mic_path2'))
path2rho = do.call(rbind, lapply(val_list2, patternRho))
path2rho$Padjust = p.adjust(path2rho$p.value, method = "BH")

val_list3 = list(c('lep_per','lep_path3'), c('pap_per','pap_path3'), c('aci_per','aci_path3'), 
                 c('cri_per','cri_path3'), c('sol_per','sol_path3'), c('mic_per','mic_path3'))
path3rho = do.call(rbind, lapply(val_list3, patternRho))
path3rho$Padjust = p.adjust(path3rho$p.value, method = "BH")


patternRhoTCGA <- function(pattern){
  pattern <- unlist(pattern)
  y<-as.numeric(data_tcga[,pattern[2]])
  x<-as.numeric(data_tcga[,pattern[1]])
  dd<-cor.test(x, y, method='spearman',  exact = FALSE)
  data.frame(path='PathTCGA', pattern=pattern[1], cor=dd$estimate, p.value=dd$p.value)
}

val_list = list(c('lep_per','lep_path'), c('pap_per','pap_path'), c('aci_per','aci_path'), 
                c('cri_per','cri_path'), c('sol_per','sol_path'), c('mic_per','mic_path'))
pathTCGArho = do.call(rbind, lapply(val_list, patternRhoTCGA))
pathTCGArho$Padjust = p.adjust(pathTXrho$p.value, method = "BH")


pathrho <- rbind(pathTXrho, path1rho, path2rho, path3rho, pathTCGArho)
pathrho$Padjust_all = p.adjust(pathrho$p.value, method = "BH") #adjust p-val for all
pathrho$pstar <-  ifelse(pathrho$Padjust_all<0.001,
                         ifelse(pathrho$Padjust_all<0.0001, '****', '***'), '')


pathrho$pattern <- factor(pathrho$pattern,levels=rev(c("lep_per","pap_per","aci_per","cri_per","mic_per","sol_per")))
pathrho$path <- factor(pathrho$path,levels=rev(c("PathTCGA","Path3", "Path2", "Path1", "PathTX")))
ggplot(pathrho, aes(path, pattern)) +
  geom_tile(aes(fill = cor), colour = "white",size=0.2)+
  scale_fill_gradient(low = brewer.pal(9, "RdGy")[5],
                      #low = "white",
                      high = brewer.pal(9, "RdGy")[2],
                      #midpoint = 0.6
                      breaks=seq(0,0.8,0.2),
                      limits=c(0, 0.8))+
  geom_text(aes(label=pstar),col ="black",size = 5)+
  theme_minimal()+ #no background
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.title.y=element_blank(),#no y axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 8),
        text=element_text(family="sans"))+ #adjust y text
  labs(fill =paste0(" **** p < 0.0001","\n\n","Correlation"))


###figure2c, 2e, figureS3e, S3f, need to change the compared variables
pattern_acc <- sum(ifelse(data_lattice$predom_ai==data_lattice$predom_path1,1,0))
pattern_acc <- pattern_acc/nrow(data_lattice)
cm <- confusionMatrix(factor(data_lattice$predom_ai), factor(data_lattice$predom_path1),dnn = c("Prediction", "Reference"))
ggplot(plt, aes(x=Reference, y=Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(x=Reference, y=Prediction, label=Freq)) +
  scale_fill_gradient(low="white", high="#0094B9") +
  theme_minimal()+
  theme(axis.ticks.y=element_blank())+
  labs(x = "Reference-Path 1",y = "Prediction-Automated") +
  scale_x_discrete(labels=c("lep","pap","aci","cri","mic","sol")) +
  scale_y_discrete(labels=rev(c("lep","pap","aci","cri","mic","sol")))


###figure2d
data_lattice$agreementP1 <- NULL
data_lattice$agreementP1[data_lattice$predom_ai == data_lattice$predom_path1] <- 'in Agreement'
data_lattice$agreementP1[data_lattice$predom_ai != data_lattice$predom_path1] <- 'Discrepant'
comList <- list( c('in Agreement', 'Discrepant'))
ggplot(data_lattice, aes(x=agreementP1, y=shannon_path1, color=agreementP1)) +
  geom_boxplot(width=0.7) +
  geom_jitter(aes(colour=agreementP1), width=0.2) +
  scale_colour_manual(values= c('Discrepant'="#0094B9", 'in Agreement'="#888888"))+
  theme_classic() +
  theme(legend.position="none",plot.title = element_text(size=12)) +
  stat_compare_means(comparison=comList,method = "wilcox.test")+
  ylab("Growth pattern intra-tumour heterogeneity")

###figure3a, figure5i
fit <- survfit(Surv(time_to_rfs, rfs_status)~IASLC_grade_ai, data = data_lattice)
ggsurvplot(fit, data = data_lattice, conf.int = FALSE, fun='event',
           pval = TRUE, pval.size = 5, pval.coord = c(0.2, 0.1),
           linetype = "solid",
           surv.plot.height = 0.7, palette = brewer.pal(6, "Dark2"),
           risk.table = TRUE,
           risk.table.col = "black", break.time.by = 1000,
           tables.height = 0.25, 
           tables.theme = theme_cleantable(),
           risk.table.title = "Number at Risk",
           tables.x.text = "none", xlim = c(0, 6500), ylim=c(0,1),
           xlab = "Days to Recurrence", ylab = "Disease-free Survival")

###figure3b, figureS4a-c
fit <- coxph(Surv(time_to_rfs, rfs_status)~age+sex+stage+pack_years+Adjuvant.therapy+procedure+IASLC_grade_ai_merge,
             data = data_lattice)
ggforest(fit, data=data_lattice,  cpositions = c(0.01, 0.22, 0.4), fontsize = 0.8)
Anova(fit, type='III', test.statistic = 'Wald')

###figure3c-d, figure4a,c,d, figureS5
my_color <- brewer.pal(9, "Set1")
df_cindex <- data.frame(para=c('Baseline', 'AI', 'Path 1', 'Path 2','Path 3'),
                         Stage13=c(0.6652, 0.6817, 0.6788, 0.6797, 0.6752),
                         Stage1=c(0.5599, 0.6428, 0.6302, 0.6153, 0.5996))
df_cindex$para <- factor(df_cindex$para, levels=c('Baseline', 'AI', 'Path 1', 'Path 2','Path 3'))
ggplot(df_cindex, aes(x=para, y=Stage1)) +
  geom_segment( aes(x=para, xend=para, y=0.5, yend=Stage1)) +
  geom_point( shape=21, color="black", fill=c('#000000', my_color[1], my_color[2], my_color[5],my_color[3]), size=3)+
  labs(y='C-index')+
  theme_classic()+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 8)) #adjust y text

ggplot(df_cindex, aes(x=para, y=Stage13)) +
  geom_segment( aes(x=para, xend=para, y=0.5, yend=Stage13)) +
  geom_point( shape=21, color="black", fill=c('#000000', my_color[1], my_color[2], my_color[5],my_color[3]), size=3)+
  labs(y='C-index')+
  theme_classic()+
  theme(axis.title.x=element_blank(),#no title
        axis.ticks.x=element_blank(),#no x axis
        axis.text.x = element_text(angle = 45, hjust = 1),# adjust x text
        axis.text.y = element_text(size = 8)) #adjust y text


###figure5a,h
ggplot(data_lattice_stage, aes(x=factor(stage), fill=factor(predom_ai)))+
  geom_bar(color='black',  position="fill")+
  geom_text(aes(label=..count..),stat='count',position=position_fill(vjust=0.5))+
  scale_fill_manual(values = c('lep'='#0000ff', 'pap'='#ffff00', 'aci'='#ff0000', 'cri'='#00ff00', 'mic'='#ff00ff','sol'='#880000'))+
  theme_classic()+labs(fill = "Pattern")

###figure5c,d,f, figureS6b-d
my_comparison1 <- list( c("low", "mid"), c("mid", "high"), c("low", "high") )
ggplot(data_lattice, aes(x=grade_ai, y=area_mean, fill=grade_ai)) +
  geom_boxplot(width=0.6, outlier.shape=NA) +
  geom_jitter(color="black", size=0.3, width=0.2) +
  scale_fill_brewer(palette='Set3')+
  theme_classic() +
  theme(legend.position="none",plot.title = element_text(size=11)) +
  stat_compare_means(comparison=my_comparison1,method = "wilcox.test")+
  xlab("")







