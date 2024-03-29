library(ape)
library(ggplot2)
library(tidytree)
library(ggtree)
library(flextable)
library(tidyr)
library(dplyr)
library(stringr)
library(svglite)
library(ggplotify)
library(eoffice)

setwd("F:/Snail/longest_protO/Orthofinder/Results_May27/CAFE/r8sr_lambda1")
##cafe_tree_number
##Get one tree from Base_asr.tre and create a file "cafe.tre
tree.cafe<-read.tree("cafe.tre")
tip.order.cafe<-data.frame(node=1:Ntip(tree.cafe), cafe.label = tree.cafe$tip.label)
node.order.cafe<-data.frame(node=1:Nnode(tree.cafe) + Ntip(tree.cafe), cafe.label = tree.cafe$node.label)
nt.order.cafe<-rbind(tip.order.cafe, node.order.cafe)
nt.order.cafe<-separate(nt.order.cafe, cafe.label, into = c("cafe.label"), sep = "_\\d", remove = TRUE)
rm(tree.cafe, tip.order.cafe, node.order.cafe)
##r8s_tree_number
##This is the r8s.ultrametric.tre in CAFE input
tree.r8s<-read.tree("r8s.ultrametric.tre")
myTree<-tree.r8s
tip.order.r8s<-data.frame(node=1:Ntip(tree.r8s), r8s.label = tree.r8s$tip.label)
node.order.r8s<-data.frame(node=1:Nnode(tree.r8s) + Ntip(tree.r8s), r8s.label = tree.r8s$node.label)
nt.order.r8s<-rbind(tip.order.r8s, node.order.r8s)
rm(tree.r8s, tip.order.r8s, node.order.r8s)


##Merge the labels of cafe and orthofinder
nt.order<-merge(nt.order.r8s, nt.order.cafe, by = "node", all.x = TRUE)
rm(nt.order.cafe, nt.order.r8s)

##Gain and loss number
pvalue=0.01
p.table<-read.delim("Base_family_results.txt", header = TRUE)
p.table<-p.table[p.table$pvalue<pvalue,]
base.change.tab<-read.delim("Base_change.tab", header = FALSE)
names(base.change.tab)<-base.change.tab[1,]
base.change.tab<-base.change.tab[base.change.tab$FamilyID %in% p.table$X.FamilyID, ]
count.gl<-as.data.frame(names(base.change.tab[2:ncol(base.change.tab)]))
names(count.gl)[1]="cafe.label"
count.gl$Increase=NA
count.gl$Decrease=NA
for (i in 1:nrow(count.gl)) {
  count.gl$Increase[i]=nrow(base.change.tab[as.numeric(base.change.tab[,i+1])>2,])
  count.gl$Decrease[i]=nrow(base.change.tab[as.numeric(base.change.tab[,i+1])<(-2),])
}

#count.gl<-read.delim("Base_clade_results.txt", header = TRUE)
#names(count.gl)[1]<-"cafe.label"
nt.order<-merge(nt.order, count.gl, by = "cafe.label", all.x = TRUE)
rm(count.gl)

##Duplication number
count.dup<-read.delim("Duplications_per_Species_Tree_Node.tsv", header = TRUE)
names(count.dup)[1]<-"r8s.label"
names(count.dup)[2]<-"Duplications"
names(count.dup)[3]<-"Duplications.5"
nt.order<-merge(nt.order, count.dup, by = "r8s.label", all.x = TRUE)
rm(count.dup)

write.table(nt.order, paste0("node_tip_label_count.",pvalue,".txt"),
            row.names = FALSE, sep = "\t", 
            quote = FALSE)

##Merge tree
tree<-full_join(myTree, nt.order, by = "node")
#Species_tree_gain_loss_dup
{
  p<-ggtree(tree, size = 1)+
    geom_tiplab(aes(label=paste("     ",str_replace_all(label,"_"," ")), sep = ""),
                offset = 0.05,
                font = "italic",
                size = 6)+
    #  geom_tippoint(size = , color = "plum2", show.legend = FALSE)+
    #  geom_nodepoint(aes(size = Duplications), color = "plum2", show.legend = FALSE)+
    #  scale_size(range=c(5,10))+
    #  labs(size = "Duplication ratio")
    geom_label2(aes(subset=isTip, label=Duplications.5), 
                nudge_x = 0, nudge_y = 0, label.size=0, 
                size=4, color = "black", fill = "lavender")+
    geom_text2(aes(subset=isTip, label=paste("+",Increase, sep = ""), 
                   hjust = 0.2, vjust = -1.2),size=4, color = "salmon4")+
    geom_text2(aes(subset=isTip, label=paste("-",Decrease, sep = ""), 
                   hjust = 0.2, vjust = 2),size=4, color = "olivedrab4")+
    geom_label2(aes(subset=!isTip & is.na(Duplications)==FALSE, label=Duplications.5), 
                nudge_x = 0, nudge_y = 0, label.size=0, 
                size=4, color = "black", fill = "lavender")+
    geom_text2(aes(subset=!isTip & is.na(Increase)==FALSE, 
                   label=paste("+",Increase, sep = ""), 
                   hjust = 1.1, vjust = -1.2),
               size=4, color = "salmon4")+
    geom_text2(aes(subset=!isTip & is.na(Decrease)==FALSE, 
                   label=paste("-",Decrease, sep = ""), 
                   hjust = 1.1, vjust = 2),
               size=4, color = "olivedrab4")+
    #geom_tiplab(aes(image=paste0("Nature/",label,".png")), geom = "image", size=0.2, offset = 1.25)
    xlim(0, 1000)
  
  p
  
  ggsave(paste0("Species_tree_gain_loss_dup.", pvalue,".png"), 
         width = 10, height = 10, units = "in", dpi = 300)
  ggsave(paste0("Species_tree_gain_loss_dup.",pvalue,".tiff"), 
         width = 10, height = 10, units = "in", dpi = 300)
  
}
#Species_tree_gain_loss_dup_N
{
  p<-ggtree(tree, size = 1)+
    geom_tiplab(aes(label=paste("     ",str_replace_all(label,"_"," ")), sep = ""),
                offset = 0.05,
                font = "italic",
                size = 6)+
    #  geom_tippoint(size = , color = "plum2", show.legend = FALSE)+
    #  geom_nodepoint(aes(size = Duplications), color = "plum2", show.legend = FALSE)+
    #  scale_size(range=c(5,10))+
    #  labs(size = "Duplication ratio")
    geom_label2(aes(subset=isTip, label=Duplications.5), 
                nudge_x = 0, nudge_y = 0, label.size=0, 
                size=4, color = "black", fill = "lavender")+
    geom_text2(aes(subset=isTip, label=paste("+",Increase, sep = ""), 
                   hjust = 0.2, vjust = -1.2),size=4, color = "salmon4")+
    geom_text2(aes(subset=isTip, label=paste("-",Decrease, sep = ""), 
                   hjust = 0.2, vjust = 2),size=4, color = "olivedrab4")+
    geom_label2(aes(subset=!isTip & is.na(Duplications)==FALSE, label=Duplications.5), 
                nudge_x = 0, nudge_y = 0, label.size=0, 
                size=4, color = "black", fill = "lavender")+
    geom_text2(aes(subset=!isTip & is.na(Increase)==FALSE, 
                   label=paste("+",Increase, sep = ""), 
                   hjust = 1.1, vjust = -1.2),
               size=4, color = "salmon4")+
    geom_text2(aes(subset=!isTip & is.na(Decrease)==FALSE, 
                   label=paste("-",Decrease, sep = ""), 
                   hjust = 1.1, vjust = 2),
               size=4, color = "olivedrab4")+
    geom_text2(aes(subset=!isTip, 
                   label=r8s.label, 
                   hjust = -1, vjust = 0),
               size=4, color = "black")+
    #geom_tiplab(aes(image=paste0("Nature/",label,".png")), geom = "image", size=0.2, offset = 1.25)
    xlim(0, 1000)
  
  p
  
  ggsave(paste0("Species_tree_gain_loss_dup_N.",pvalue,".png"), 
         width = 10, height = 10, units = "in", dpi = 300)
  ggsave(paste0("Species_tree_gain_loss_dup_N.",pvalue,".tiff"), 
         width = 10, height = 10, units = "in", dpi = 300)
  
}
##export plot data
Species_tree_data<-p[["data"]]
write.table(Species_tree_data, 
            file = paste0("Species_tree_gain_loss_dup.",pvalue,".txt"), 
            row.names = FALSE, sep = "\t")
#Species_tree_gain_loss_dup_N_topo
{
  p<-ggtree(tree, size = 1, branch.length = "none")+
    geom_tiplab(aes(label=paste("     ",str_replace_all(label,"_"," ")), sep = ""),
                offset = 0.05,
                font = "italic",
                size = 6)+
    #  geom_tippoint(size = , color = "plum2", show.legend = FALSE)+
    #  geom_nodepoint(aes(size = Duplications), color = "plum2", show.legend = FALSE)+
    #  scale_size(range=c(5,10))+
    #  labs(size = "Duplication ratio")
    geom_label2(aes(subset=isTip, label=Duplications.5), 
                nudge_x = 0, nudge_y = 0, label.size=0, 
                size=4, color = "black", fill = "lavender")+
    geom_text2(aes(subset=isTip, label=paste("+",Increase, sep = ""), 
                   hjust = 0.2, vjust = -1.2),size=4, color = "salmon4")+
    geom_text2(aes(subset=isTip, label=paste("-",Decrease, sep = ""), 
                   hjust = 0.2, vjust = 2),size=4, color = "olivedrab4")+
    geom_label2(aes(subset=!isTip & is.na(Duplications)==FALSE, label=Duplications.5), 
                nudge_x = 0, nudge_y = 0, label.size=0, 
                size=4, color = "black", fill = "lavender")+
    geom_text2(aes(subset=!isTip & is.na(Increase)==FALSE, 
                   label=paste("+",Increase, sep = ""), 
                   hjust = 1.1, vjust = -1.2),
               size=4, color = "salmon4")+
    geom_text2(aes(subset=!isTip & is.na(Decrease)==FALSE, 
                   label=paste("-",Decrease, sep = ""), 
                   hjust = 1.1, vjust = 2),
               size=4, color = "olivedrab4")+
    geom_text2(aes(subset=!isTip, 
                   label=r8s.label, 
                   hjust = -0.75, vjust = 0),
               size=4, color = "black")+
    #geom_tiplab(aes(image=paste0("Nature/",label,".png")), geom = "image", size=0.2, offset = 1.25)
    xlim(0, 10)
  
  p
  
  ggsave(paste0("Species_tree_gain_loss_dup_N_topo.",pvalue,".png"), 
         width = 10, height = 10, units = "in", dpi = 300)
  ggsave(paste0("Species_tree_gain_loss_dup_N_topo.",pvalue,".tiff"), 
         width = 10, height = 10, units = "in", dpi = 300)
  
}


#Species_tree_gain_loss
{
  p<-ggtree(tree, size = 1)+
    geom_tiplab(aes(label=paste("     ",str_replace_all(label,"_"," ")), sep = ""),
                offset = 0.05,
                font = "italic",
                size = 6)+
    #  geom_tippoint(size = , color = "plum2", show.legend = FALSE)+
    #  geom_nodepoint(aes(size = Duplications), color = "plum2", show.legend = FALSE)+
    #  scale_size(range=c(5,10))+
    #  labs(size = "Duplication ratio")
    #geom_label2(aes(subset=isTip, label=Duplications), 
    #            nudge_x = 0, nudge_y = 0, label.size=0, 
    #            size=4, color = "black", fill = "lavender")+
    scale_x_continuous(limits = c(0,650))+
    geom_text2(aes(subset=isTip, label=paste("+",Increase, sep = ""), 
                   hjust = 0.25, vjust = -1),size=4, color = "salmon4")+
    geom_text2(aes(subset=isTip, label=paste("-",Decrease, sep = ""), 
                   hjust = 0.25, vjust = 1.5),size=4, color = "olivedrab4")+
    #geom_label2(aes(subset=!isTip & is.na(Duplications)==FALSE, label=Duplications), 
    #            nudge_x = 0, nudge_y = 0, label.size=0, 
    #            size=4, color = "black", fill = "lavender")+
    geom_text2(aes(subset=!isTip & is.na(Increase)==FALSE, 
                   label=paste("+",Increase, sep = ""), 
                   hjust = 1.1, vjust = -1),
               size=4, color = "salmon4")+
    geom_text2(aes(subset=!isTip & is.na(Increase)==FALSE, 
                   label=paste("-",Decrease, sep = ""), 
                   hjust = 1.1, vjust = 1.5),
               size=4, color = "olivedrab4")+
    #geom_tiplab(aes(image=paste0("Nature/",label,".png")), geom = "image", size=0.2, offset = 1.25)
    xlim(0, 1000)
  
  p
  
  ggsave(paste0("Species_tree_gain_loss.",pvalue,".png"),
         width = 10, height = 10, units = "in", dpi = 300)
  ggsave(paste0("Species_tree_gain_loss.",pvalue,".tiff"), 
         width = 10, height = 10, units = "in", dpi = 300)
  
}
#Species_tree_gain_loss_N
{
  p<-ggtree(tree, size = 1)+
    geom_tiplab(aes(label=paste("     ",str_replace_all(label,"_"," ")), sep = ""),
                offset = 0.05,
                font = "italic",
                size = 6)+
    scale_x_continuous(limits = c(0,650))+
    #  geom_tippoint(size = , color = "plum2", show.legend = FALSE)+
    #  geom_nodepoint(aes(size = Duplications), color = "plum2", show.legend = FALSE)+
    #  scale_size(range=c(5,10))+
    #  labs(size = "Duplication ratio")
    #geom_label2(aes(subset=isTip, label=Duplications), 
    #            nudge_x = 0, nudge_y = 0, label.size=0, 
    #            size=4, color = "black", fill = "lavender")+
    geom_text2(aes(subset=isTip, label=paste("+",Increase, sep = ""), 
                   hjust = 0.25, vjust = -1),size=4, color = "salmon4")+
    geom_text2(aes(subset=isTip, label=paste("-",Decrease, sep = ""), 
                   hjust = 0.25, vjust = 1.5),size=4, color = "olivedrab4")+
    #geom_label2(aes(subset=!isTip & is.na(Duplications)==FALSE, label=Duplications), 
    #            nudge_x = 0, nudge_y = 0, label.size=0, 
    #            size=4, color = "black", fill = "lavender")+
    geom_text2(aes(subset=!isTip & is.na(Increase)==FALSE, 
                   label=paste("+",Increase, sep = ""), 
                   hjust = 1.1, vjust = -1),
               size=4, color = "salmon4")+
    geom_text2(aes(subset=!isTip & is.na(Increase)==FALSE, 
                   label=paste("-",Decrease, sep = ""), 
                   hjust = 1.1, vjust = 1.5),
               size=4, color = "olivedrab4")+
    geom_text2(aes(subset=!isTip, 
                   label=r8s.label, 
                   hjust = -0.5, vjust = 0),
               size=4, color = "black")+
    #geom_tiplab(aes(image=paste0("Nature/",label,".png")), geom = "image", size=0.2, offset = 1.25)
    xlim(0,1000)
  
  p
  
  ggsave(paste0("Species_tree_gain_loss_N.",pvalue,".png"), 
         width = 10, height = 10, units = "in", dpi = 300)
  ggsave(paste0("Species_tree_gain_loss_N.",pvalue,".tiff"), 
         width = 10, height = 10, units = "in", dpi = 300)
  
  f = paste0("Species_tree_gain_loss_N.",pvalue,".pptx")
  topptx(p,f, width = 10, height = 10, units = "in")
}

#Species_tree_gain_loss_N_topo
{
  p<-ggtree(tree, size = 1, branch.length = "none")+
    geom_tiplab(aes(label=paste("     ",str_replace_all(label,"_"," ")), sep = ""),
                offset = 0.05,
                font = "italic",
                size = 6)+
    scale_x_continuous(limits = c(0,650))+
    #  geom_tippoint(size = , color = "plum2", show.legend = FALSE)+
    #  geom_nodepoint(aes(size = Duplications), color = "plum2", show.legend = FALSE)+
    #  scale_size(range=c(5,10))+
    #  labs(size = "Duplication ratio")
    #geom_label2(aes(subset=isTip, label=Duplications), 
    #            nudge_x = 0, nudge_y = 0, label.size=0, 
    #            size=4, color = "black", fill = "lavender")+
    geom_text2(aes(subset=isTip, label=paste("+",Increase, sep = ""), 
                   hjust = 0.25, vjust = -0.5),size=4, color = "salmon4")+
    geom_text2(aes(subset=isTip, label=paste("-",Decrease, sep = ""), 
                   hjust = 0.25, vjust = 1.5),size=4, color = "olivedrab4")+
    #geom_label2(aes(subset=!isTip & is.na(Duplications)==FALSE, label=Duplications), 
    #            nudge_x = 0, nudge_y = 0, label.size=0, 
    #            size=4, color = "black", fill = "lavender")+
    geom_text2(aes(subset=!isTip & is.na(Increase)==FALSE, 
                   label=paste("+",Increase, sep = ""), 
                   hjust = 1.1, vjust = -0.5),
               size=4, color = "salmon4")+
    geom_text2(aes(subset=!isTip & is.na(Decrease)==FALSE, 
                   label=paste("-",Decrease, sep = ""), 
                   hjust = 1.1, vjust = 1.5),
               size=4, color = "olivedrab4")+
    geom_text2(aes(subset=!isTip, 
                   label=r8s.label, 
                   hjust = -0.5, vjust = 0),
               size=4, color = "black")+
    #geom_tiplab(aes(image=paste0("Nature/",label,".png")), geom = "image", size=0.2, offset = 1.25)
    xlim(0, 10)
  
  p
  
  ggsave(paste0("Species_tree_gain_loss_N_topo.",pvalue,".png"), 
         width = 10, height = 10, units = "in", dpi = 300)
  ggsave(paste0("Species_tree_gain_loss_N_topo.",pvalue,".tiff"), 
         width = 10, height = 10, units = "in", dpi = 300)
  f = paste0("Species_tree_gain_loss_N_topo.",pvalue,".pptx")
  topptx(p,f, width = 10, height = 10, units = "in")
  
}

##Ortholog stat
orthostat<-read.delim("F:/Snail/longest_protO/Orthofinder/Results_May27/Orthogroups/Orthogroup.stat.txt")
names(orthostat)[1]="group"
mergestat<-matrix(NA, ncol = 3, nrow = 1, dimnames = list(NA,c("group","species","no")))
mergestat<-as.data.frame(mergestat)
for (i in 2:ncol(orthostat)) {
  substat<-orthostat[,c(1,i)]
  substat$species<-names(substat)[2]
  names(substat)[2]="no"
  mergestat<-rbind(mergestat, substat)
}
mergestat<-subset(mergestat, is.na(no)==FALSE)

speciesorder<-read.delim("Species_tree_gain_loss_dup.txt", header = TRUE)
speciesorder<-subset(speciesorder, node <= ncol(orthostat)-1, select = c("y","label"))
speciesorder<-speciesorder[order(speciesorder$y),2]
#speciesorder<-speciesorder$label
grouporder<-c("Single-copy orthologs",
              "Multi-copy orthologs (>1 species)",
              "Multi-copy orthologs (all species)",
              "Unique paralogs",
              "Unclustered genes")
p<-ggplot(data=mergestat, aes(x=species, y=no, fill=factor(group, levels = grouporder)))+
  geom_bar(stat="identity", width = 0.7)+
  scale_x_discrete(limits = speciesorder, 
                   label = paste("",str_replace_all(speciesorder,"_"," ")))+
  scale_y_continuous(limits = c(0,50000), breaks = seq(0,70000,10000))+
  labs(title = "", y="Number of genes", x="", fill = "")+
  scale_fill_manual(values = c("purple4", "plum2", "burlywood2", "tan4", "darkgray"))+
  theme(axis.line = element_line(linetype = "solid", size = 0.5), 
        axis.ticks = element_line(colour = "black", size = 0.5), 
        axis.title = element_text(size = 12), 
        axis.text = element_text(size = 12, colour = "black"), 
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1), 
        axis.text.y = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        panel.background = element_rect(fill = NA), 
        legend.key = element_rect(fill = NA), 
        legend.background = element_rect(fill = NA), 
        legend.position = c(0.18,0.9))
  
p
ggsave("orthostat.png",
       width = 10, height = 10, units = "in", dpi = 300)
ggsave("orthostat.tiff", 
       width = 10, height = 10, units = "in", dpi = 300)