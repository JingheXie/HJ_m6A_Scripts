library(gplots)
library(ggplot2)
library(VennDiagram)
library(export)
setwd("C:/Users/CSIV149/PLC.lab.docs/04.hanjian/for.JingHe.July/Filtered.METTL3.eCLIP.peaks")
#10:100:67 
bins_overlapping_peaks_with_IgG = read.delim("cds_bins_overlapped_Filtered.METTL3.eCLIP.peaks_IgG.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")
bins_overlapping_peaks_5utr_with_IgG = read.delim("5utr_bins_overlapped_Filtered.METTL3.eCLIP.peaks_IgG.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")
bins_overlapping_peaks_3utr_with_IgG = read.delim("3utr_bins_overlapped_Filtered.METTL3.eCLIP.peaks_IgG.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")

total_with_IgG = rbind(count(bins_overlapping_peaks_with_IgG,V4), 
                         count(bins_overlapping_peaks_5utr_with_IgG,V4), 
                         count(bins_overlapping_peaks_3utr_with_IgG,V4)) %>% subset(select= -c(V4))
total_with_IgG = total_with_IgG %>% mutate(num = 1:n()) %>% subset(select=c(num, n)) #%>% plot(type="l", col="green", lwd=5, ylab="Peak Coverage")
max(total_with_IgG$n)



bins_overlapping_peaks_with_Scr.1 = read.delim("cds_bins_overlapped_Filtered.METTL3.eCLIP.peaks_METTL3.SCR-1.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")
bins_overlapping_peaks_5utr_with_Scr.1 = read.delim("5utr_bins_overlapped_Filtered.METTL3.eCLIP.peaks_METTL3.SCR-1.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")
bins_overlapping_peaks_3utr_with_Scr.1 = read.delim("3utr_bins_overlapped_Filtered.METTL3.eCLIP.peaks_METTL3.SCR-1.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")

total_with_Scr.1 = rbind(count(bins_overlapping_peaks_5utr_with_Scr.1,V4), 
                            count(bins_overlapping_peaks_with_Scr.1,V4), 
                            count(bins_overlapping_peaks_3utr_with_Scr.1,V4)) %>% subset(select= -c(V4))
total_with_Scr.1 = total_with_Scr.1 %>% mutate(num = 1:n()) %>% subset(select=c(num, n)) #%>% plot(type="l", col="green", lwd=5, ylab="Peak Coverage")
max(total_with_Scr.1$n)




bins_overlapping_peaks_with_Scr.2 = read.delim("cds_bins_overlapped_Filtered.METTL3.eCLIP.peaks_METTL3.SCR-2.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")
bins_overlapping_peaks_5utr_with_Scr.2 = read.delim("5utr_bins_overlapped_Filtered.METTL3.eCLIP.peaks_METTL3.SCR-2.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")
bins_overlapping_peaks_3utr_with_Scr.2 = read.delim("3utr_bins_overlapped_Filtered.METTL3.eCLIP.peaks_METTL3.SCR-2.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")

total_with_Scr.2 = rbind(count(bins_overlapping_peaks_5utr_with_Scr.2,V4), 
                         count(bins_overlapping_peaks_with_Scr.2,V4), 
                         count(bins_overlapping_peaks_3utr_with_Scr.2,V4)) %>% subset(select= -c(V4))
total_with_Scr.2 = total_with_Scr.2 %>% mutate(num = 1:n()) %>% subset(select=c(num, n)) #%>% plot(type="l", col="green", lwd=5, ylab="Peak Coverage")
max(total_with_Scr.2$n)




bins_overlapping_peaks_with_sh.1.1 = read.delim("cds_bins_overlapped_Filtered.METTL3.eCLIP.peaks_METTL3.KD1-1.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")
bins_overlapping_peaks_5utr_with_sh.1.1 = read.delim("5utr_bins_overlapped_Filtered.METTL3.eCLIP.peaks_METTL3.KD1-1.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")
bins_overlapping_peaks_3utr_with_sh.1.1 = read.delim("3utr_bins_overlapped_Filtered.METTL3.eCLIP.peaks_METTL3.KD1-1.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")

total_with_sh.1.1 = rbind(count(bins_overlapping_peaks_5utr_with_sh.1.1,V4), 
                         count(bins_overlapping_peaks_with_sh.1.1,V4), 
                         count(bins_overlapping_peaks_3utr_with_sh.1.1,V4)) %>% subset(select= -c(V4))
total_with_sh.1.1 = total_with_sh.1.1 %>% mutate(num = 1:n()) %>% subset(select=c(num, n)) #%>% plot(type="l", col="green", lwd=5, ylab="Peak Coverage")
max(total_with_sh.1.1$n)





bins_overlapping_peaks_with_sh.1.2 = read.delim("cds_bins_overlapped_Filtered.METTL3.eCLIP.peaks_METTL3.KD1-2.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")
bins_overlapping_peaks_5utr_with_sh.1.2 = read.delim("5utr_bins_overlapped_Filtered.METTL3.eCLIP.peaks_METTL3.KD1-2.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")
bins_overlapping_peaks_3utr_with_sh.1.2 = read.delim("3utr_bins_overlapped_Filtered.METTL3.eCLIP.peaks_METTL3.KD1-2.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")

total_with_sh.1.2 = rbind(count(bins_overlapping_peaks_5utr_with_sh.1.2,V4), 
                          count(bins_overlapping_peaks_with_sh.1.2,V4), 
                          count(bins_overlapping_peaks_3utr_with_sh.1.2,V4)) %>% subset(select= -c(V4))
total_with_sh.1.2 = total_with_sh.1.2 %>% mutate(num = 1:n()) %>% subset(select=c(num, n)) #%>% plot(type="l", col="green", lwd=5, ylab="Peak Coverage")
max(total_with_sh.1.2$n)





bins_overlapping_peaks_with_sh.2.1 = read.delim("cds_bins_overlapped_Filtered.METTL3.eCLIP.peaks_METTL3.KD2-1.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")
bins_overlapping_peaks_5utr_with_sh.2.1 = read.delim("5utr_bins_overlapped_Filtered.METTL3.eCLIP.peaks_METTL3.KD2-1.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")
bins_overlapping_peaks_3utr_with_sh.2.1 = read.delim("3utr_bins_overlapped_Filtered.METTL3.eCLIP.peaks_METTL3.KD2-1.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")

total_with_sh.2.1 = rbind(count(bins_overlapping_peaks_5utr_with_sh.2.1,V4), 
                          count(bins_overlapping_peaks_with_sh.2.1,V4), 
                          count(bins_overlapping_peaks_3utr_with_sh.2.1,V4)) %>% subset(select= -c(V4))
total_with_sh.2.1 = total_with_sh.2.1 %>% mutate(num = 1:n()) %>% subset(select=c(num, n)) #%>% plot(type="l", col="green", lwd=5, ylab="Peak Coverage")
max(total_with_sh.2.1$n)





bins_overlapping_peaks_with_sh.2.2 = read.delim("cds_bins_overlapped_Filtered.METTL3.eCLIP.peaks_METTL3.KD2-2.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")
bins_overlapping_peaks_5utr_with_sh.2.2 = read.delim("5utr_bins_overlapped_Filtered.METTL3.eCLIP.peaks_METTL3.KD2-2.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")
bins_overlapping_peaks_3utr_with_sh.2.2 = read.delim("3utr_bins_overlapped_Filtered.METTL3.eCLIP.peaks_METTL3.KD2-2.P3FC4.geneAnno.out_with.TPM_0.1.txt", stringsAsFactors = F, header = F, check.names = F, sep = "\t")

total_with_sh.2.2 = rbind(count(bins_overlapping_peaks_5utr_with_sh.2.2,V4), 
                          count(bins_overlapping_peaks_with_sh.2.2,V4), 
                          count(bins_overlapping_peaks_3utr_with_sh.2.2,V4)) %>% subset(select= -c(V4))
total_with_sh.2.2 = total_with_sh.2.2 %>% mutate(num = 1:n()) %>% subset(select=c(num, n)) #%>% plot(type="l", col="green", lwd=5, ylab="Peak Coverage")
max(total_with_sh.2.2$n)






##第一种：除以4个的最大值。
all.max <- max(max(total_with_IgG$n), max(total_with_Scr.1$n), 
               max(total_with_Scr.2$n)
               , max(total_with_sh.1.1$n), max(total_with_sh.1.2$n)
               , max(total_with_sh.2.1$n), max(total_with_sh.2.2$n))

total_with_IgG$n.rate <- total_with_IgG$n/all.max
total_with_Scr.1$n.rate <- total_with_Scr.1$n/all.max
total_with_Scr.2$n.rate <- total_with_Scr.2$n/all.max
total_with_sh.1.1$n.rate <- total_with_sh.1.1$n/all.max
total_with_sh.1.2$n.rate <- total_with_sh.1.2$n/all.max
total_with_sh.2.1$n.rate <- total_with_sh.2.1$n/all.max
total_with_sh.2.2$n.rate <- total_with_sh.2.2$n/all.max


##第二种：除以4个样本各自的最大值。
#total_with_DAP3_XL1$n.rate <- total_with_DAP3_XL1$n/max(total_with_DAP3_XL1$n)
#total_with_DAP3_XL2$n.rate <- total_with_DAP3_XL2$n/max(total_with_DAP3_XL2$n)
#total_without_DAP3_XL1$n.rate <- total_without_DAP3_XL1$n/max(total_without_DAP3_XL1$n)
#total_without_DAP3_XL2$n.rate <- total_without_DAP3_XL2$n/max(total_without_DAP3_XL2$n)




pt = ggplot() +
  geom_line(data=total_with_IgG, aes(x=num, y=n.rate, group=1),color="black", size = 2)+
  geom_line(data=total_with_Scr.1, aes(x=num, y=n.rate, group=1),color="#BDB76B", size = 2)+
  geom_line(data=total_with_Scr.2, aes(x=num, y=n.rate, group=1),color="grey50", size = 2)+
  geom_line(data=total_with_sh.1.1, aes(x=num, y=n.rate, group=1),color="orange", size = 2)+
  geom_line(data=total_with_sh.1.2, aes(x=num, y=n.rate, group=1),color="red", size = 2)+
  geom_line(data=total_with_sh.2.1, aes(x=num, y=n.rate, group=1),color="green4", size = 2)+
  geom_line(data=total_with_sh.2.2, aes(x=num, y=n.rate, group=1),color="blue", size = 2)+
  ylim(0,1) +
  theme_minimal() +
  theme(axis.text.x = element_text(color = "black", size = 18, face = "plain"),
        axis.text.y = element_text(color = "black", size = 18, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 18, face = "plain"),
        axis.title.y = element_text(color = "black", size = 18, face = "plain"))+
  annotate('text',x=150,y=1.0,
           label=expression("IgG"),
           size=6,color='black')+
  annotate('text',x=150,y=0.9,
           label=expression("Scr-1"),
           size=6,color='#BDB76B')+
  annotate('text',x=150,y=0.8,
           label=expression("Scr-2"),
           size=6,color='grey50')+
  annotate('text',x=150,y=0.7,
           label=expression("KD1-1"),
           size=6,color='orange')+
  annotate('text',x=150,y=0.6,
           label=expression("KD1-2"),
           size=6,color='red')+
  annotate('text',x=150,y=0.5,
           label=expression("KD2-1"),
           size=6,color='green4')+
  annotate('text',x=150,y=0.4,
           label=expression("KD2-2"),
           size=6,color='blue')
pt + 
  geom_vline(xintercept = 12, linetype="dashed", color="grey18", size=1.2) + 
  geom_vline(xintercept = 113, linetype="dashed", color="grey18", size=1.2) +
  xlab("5UTR_CDS_3UTR") +
  ylab("the number of bin")


graph2ppt(file="MetaPlot_Filtered.METTL3.eCLIP.peaks.pptx", width=8, aspectr=1.5)

