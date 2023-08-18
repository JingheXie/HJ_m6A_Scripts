library(tidyr)
library(dplyr)
library(gplots)
library(ggplot2)
library(VennDiagram)
library(export)
setwd("C:/Users/CSIV149/PLC.lab.docs/04.hanjian/for.JingHe.July/Filtered.METTL3.eCLIP.peaks.in.both.or.unique.Scr.or.KD/")
#10:100:67 
bins_overlapping_peaks_with_both = read.delim("cds_bins_overlapped_Filtered.METTL3.eCLIP.peaks.in.both.or.unique.Scr.or.KD_METTL3_both_SCRandKD.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")
bins_overlapping_peaks_5utr_with_both = read.delim("5utr_bins_overlapped_Filtered.METTL3.eCLIP.peaks.in.both.or.unique.Scr.or.KD_METTL3_both_SCRandKD.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")
bins_overlapping_peaks_3utr_with_both = read.delim("3utr_bins_overlapped_Filtered.METTL3.eCLIP.peaks.in.both.or.unique.Scr.or.KD_METTL3_both_SCRandKD.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")

total_with_both = rbind(count(bins_overlapping_peaks_5utr_with_both,V4), 
                          count(bins_overlapping_peaks_with_both,V4), 
                          count(bins_overlapping_peaks_3utr_with_both,V4)) %>% subset(select= -c(V4))
total_with_both = total_with_both %>% mutate(num = 1:n()) %>% subset(select=c(num, n)) #%>% plot(type="l", col="green", lwd=5, ylab="Peak Coverage")
max(total_with_both$n)





bins_overlapping_peaks_with_KDonly = read.delim("cds_bins_overlapped_Filtered.METTL3.eCLIP.peaks.in.both.or.unique.Scr.or.KD_METTL3_KDonly.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")
bins_overlapping_peaks_5utr_with_KDonly = read.delim("5utr_bins_overlapped_Filtered.METTL3.eCLIP.peaks.in.both.or.unique.Scr.or.KD_METTL3_KDonly.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")
bins_overlapping_peaks_3utr_with_KDonly = read.delim("3utr_bins_overlapped_Filtered.METTL3.eCLIP.peaks.in.both.or.unique.Scr.or.KD_METTL3_KDonly.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")

total_with_KDonly = rbind(count(bins_overlapping_peaks_5utr_with_KDonly,V4), 
                          count(bins_overlapping_peaks_with_KDonly,V4), 
                          count(bins_overlapping_peaks_3utr_with_KDonly,V4)) %>% subset(select= -c(V4))
total_with_KDonly = total_with_KDonly %>% mutate(num = 1:n()) %>% subset(select=c(num, n)) #%>% plot(type="l", col="green", lwd=5, ylab="Peak Coverage")
max(total_with_KDonly$n)





bins_overlapping_peaks_with_SCRonly = read.delim("cds_bins_overlapped_Filtered.METTL3.eCLIP.peaks.in.both.or.unique.Scr.or.KD_METTL3_SCRonly.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")
bins_overlapping_peaks_5utr_with_SCRonly = read.delim("5utr_bins_overlapped_Filtered.METTL3.eCLIP.peaks.in.both.or.unique.Scr.or.KD_METTL3_SCRonly.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")
bins_overlapping_peaks_3utr_with_SCRonly = read.delim("3utr_bins_overlapped_Filtered.METTL3.eCLIP.peaks.in.both.or.unique.Scr.or.KD_METTL3_SCRonly.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")

total_with_SCRonly = rbind(count(bins_overlapping_peaks_5utr_with_SCRonly,V4), 
                          count(bins_overlapping_peaks_with_SCRonly,V4), 
                          count(bins_overlapping_peaks_3utr_with_SCRonly,V4)) %>% subset(select= -c(V4))
total_with_SCRonly = total_with_SCRonly %>% mutate(num = 1:n()) %>% subset(select=c(num, n)) #%>% plot(type="l", col="green", lwd=5, ylab="Peak Coverage")
max(total_with_SCRonly$n)



##第一种：除以4个的最大值。
all.max <- max(max(total_with_both$n), max(total_with_KDonly$n)
               , max(total_with_SCRonly$n))

total_with_both$n.rate <- total_with_both$n/all.max
total_with_KDonly$n.rate <- total_with_KDonly$n/all.max
total_with_SCRonly$n.rate <- total_with_SCRonly$n/all.max


##第二种：除以4个样本各自的最大值。
total_with_both$n.rate <- total_with_both$n/max(total_with_both$n)
total_with_KDonly$n.rate <- total_with_KDonly$n/max(total_with_KDonly$n)
total_with_SCRonly$n.rate <- total_with_SCRonly$n/max(total_with_SCRonly$n)




pt = ggplot() +
  geom_line(data=total_with_both, aes(x=num, y=n.rate, group=1),color="red", size = 2)+
  geom_line(data=total_with_KDonly, aes(x=num, y=n.rate, group=1),color="green4", size = 2)+
  geom_line(data=total_with_SCRonly, aes(x=num, y=n.rate, group=1),color="blue", size = 2)+
  ylim(0,1) +
  theme_minimal() +
  theme(axis.text.x = element_text(color = "black", size = 18, face = "plain"),
        axis.text.y = element_text(color = "black", size = 18, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 18, face = "plain"),
        axis.title.y = element_text(color = "black", size = 18, face = "plain"))+
  annotate('text',x=150,y=0.9,
           label=expression("both_SCRandKD"),
           size=6,color='red')+
  annotate('text',x=150,y=0.8,
           label=expression("KDonly"),
           size=6,color='green4')+
  annotate('text',x=150,y=0.7,
           label=expression("SCRonly"),
           size=6,color='blue')

pt + 
  geom_vline(xintercept = 12, linetype="dashed", color="grey18", size=1.2) + 
  geom_vline(xintercept = 113, linetype="dashed", color="grey18", size=1.2) +
  xlab("5UTR_CDS_3UTR") +
  ylab("the number of bin")


graph2ppt(file="MetaPlot_Filtered.METTL3.eCLIP.peaks.in.both.or.unique.Scr.or.KD.pptx", width=8, aspectr=1.5)
graph2ppt(file="MetaPlot_Filtered.METTL3.eCLIP.peaks.in.both.or.unique.Scr.or.KD_divide.by.max.of.each.sample.pptx", width=8, aspectr=1.5)



