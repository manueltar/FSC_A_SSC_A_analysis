
suppressMessages(library("plyr", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("data.table", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("crayon", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("withr", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("ggplot2", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("farver", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("labeling", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("optparse", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("dplyr", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("withr", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("backports", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("broom", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("rstudioapi", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("cli", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("tzdb", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("svglite", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("ggeasy", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("sandwich", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("digest", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("tidyverse", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("RColorBrewer", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("cowplot", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("ggExtra", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))

# suppressMessages(library("BiocGenerics", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
# suppressMessages(library("S4Vectors", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
# suppressMessages(library("IRanges", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
# suppressMessages(library("GenomeInfoDb", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
# suppressMessages(library("GenomicRanges", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
# suppressMessages(library("Biobase", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
# suppressMessages(library("AnnotationDbi", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
# suppressMessages(library("GO.db", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
# suppressMessages(library("org.Hs.eg.db", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
# suppressMessages(library("TxDb.Hsapiens.UCSC.hg19.knownGene", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
# suppressMessages(library("rtracklayer", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
# suppressMessages(library("R.oo", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
# suppressMessages(library("splitstackshape", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
# suppressMessages(library("ggtranscript", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
# suppressMessages(library("ggpubr", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))


opt = NULL

options(warn = 1)

graphs_2D = function(option_list)
{

  opt_in = option_list
  opt <<- option_list
  
  cat("All options:\n")
  printList(opt)
  
  
  #### READ and transform type ----
  
  type = opt$type
  
  cat("TYPE_\n")
  cat(sprintf(as.character(type)))
  cat("\n")

  
  #### READ and transform out ----
  
  out = opt$out
  
  cat("out_\n")
  cat(sprintf(as.character(out)))
  cat("\n")
  
 
  
  #### READ and transform out ----
  
  time_point_selected = unlist(strsplit(opt$time_point_selected, split=","))
  
  cat("time_point_selected\n")
  cat(sprintf(as.character(time_point_selected)))
  cat("\n")
  
  #### READ and transform out ----
  
  subpopulation_selected = unlist(strsplit(opt$subpopulation_selected, split=","))
  
  cat("subpopulation_selected\n")
  cat(sprintf(as.character(subpopulation_selected)))
  cat("\n")
  
 setwd(out)
 
 object<-readRDS(file='collected_data.rds')
 
 colnames(object)[which(colnames(object) == 'FSC-A')]<-'FSC'
 colnames(object)[which(colnames(object) == 'SSC-A')]<-'SSC'
 
 cat("object_0\n")
 cat(str(object))
 cat("\n")
 cat(sprintf(as.character(names(summary(object$time_point)))))
 cat("\n")
 cat(sprintf(as.character(summary(object$time_point))))
 cat("\n")
 cat(sprintf(as.character(names(summary(object$subpopulation)))))
 cat("\n")
 cat(sprintf(as.character(summary(object$subpopulation))))
 cat("\n")
 cat(sprintf(as.character(names(summary(object$sample)))))
 cat("\n")
 cat(sprintf(as.character(summary(object$sample))))
 cat("\n")
 cat(sprintf(as.character(names(summary(object$Genotype)))))
 cat("\n")
 cat(sprintf(as.character(summary(object$Genotype))))
 cat("\n")
 
 
 object_subset<-droplevels(object[which(object$time_point%in%time_point_selected &
                               object$subpopulation%in%subpopulation_selected),])
 
 
 cat("object_subset_0\n")
 cat(str(object_subset))
 cat("\n")
 cat(sprintf(as.character(names(summary(object_subset$time_point)))))
 cat("\n")
 cat(sprintf(as.character(summary(object_subset$time_point))))
 cat("\n")
 cat(sprintf(as.character(names(summary(object_subset$subpopulation)))))
 cat("\n")
 cat(sprintf(as.character(summary(object_subset$subpopulation))))
 cat("\n")
 cat(sprintf(as.character(names(summary(object_subset$sample)))))
 cat("\n")
 cat(sprintf(as.character(summary(object_subset$sample))))
 cat("\n")
 cat(sprintf(as.character(names(summary(object_subset$Genotype)))))
 cat("\n")
 cat(sprintf(as.character(summary(object_subset$Genotype))))
 cat("\n")
 
 object_subset.dt<-data.table(object_subset, key=c("Genotype","subpopulation"))
 
 Summary_table<-as.data.frame(object_subset.dt[,.(n=.N,
                                                  M_SSC=round(as.numeric(summary(SSC)[3]),0),
                                                  M_FSC=round(as.numeric(summary(FSC)[3]),0)),, by=key(object_subset.dt)], stringsAsFactors=F)
 
 
 cat("Summary_table_1\n")
 cat(str(Summary_table))
 cat("\n")

 
 #### vector_fill for genotypes ----
 
 vector_fill<-brewer.pal(length(levels(object_subset$Genotype)),"Dark2")
 
 cat("vector_fill_0\n")
 cat(str(vector_fill))
 cat("\n")
 
 ##### graph -----
 
 
 path_graphs<-paste(out,'graphs','/',sep='')
 
 if (file.exists(path_graphs)){
   
   
 }else{
   
   dir.create(file.path(path_graphs))
   
 }#path_graphs
 
 # vector_colors_WT<-brewer.pal(9, "YlGn")[c(6)]
 # vector_colors_KI<-brewer.pal(9, "YlOrRd")[c(8)]
 # vector_colors_Del<-brewer.pal(9, "PuBu")[c(7)]
 # 
 # 
 # vector_fill<-c(vector_colors_WT,vector_colors_KI,vector_colors_Del)
 # 
 # 
 # cat("vector_fill\n")
 # cat(str(vector_fill))
 # cat("\n")
 
 A<-round(summary(object_subset$FSC[!is.na(object_subset$FSC)]),0)
 
 
 # cat("summary_GENE_EXP\n")
 # cat(sprintf(as.character(names(A))))
 # cat("\n")
 # cat(sprintf(as.character(A)))
 # cat("\n")
 
 step<-abs(A[6]-A[1])/4
 
 if(step == 0)
 {
   
   step<-1
 }
 
 breaks.FSC<-sort(unique(c(A[6],seq(from= A[1], to=A[6],by=step))))
 # breaks.FSC<-c(273357,538480,803604,1068728,1333851)
 # breaks.FSC<-c(273350,538480,803600,1068730,1333860)
 labels.FSC<-as.character(round(breaks.FSC,0))
 
 
 
 cat("labels.FSC:\t")
 cat(sprintf(as.character(labels.FSC)))
 cat("\n")
 
 A<-round(summary(object_subset$SSC[!is.na(object_subset$SSC)]),0)
 
 
 # cat("summary_GENE_EXP\n")
 # cat(sprintf(as.character(names(A))))
 # cat("\n")
 # cat(sprintf(as.character(A)))
 # cat("\n")
 
 step<-abs(A[6]-A[1])/4
 
 if(step == 0)
 {
   
   step<-1
 }
 
 breaks.SSC<-sort(unique(c(A[6],seq(from= A[1], to=A[6],by=step))))
 # breaks.SSC<-c(77360,446480,815600,1184710,1553830)
 labels.SSC<-as.character(round(breaks.SSC,0))
 
 
 
 cat("labels.SSC:\t")
 cat(sprintf(as.character(labels.SSC)))
 cat("\n")
 
 Density_2D<-ggplot(data=object_subset,
                    aes(x=SSC, y=FSC)) +
   stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = Genotype), bins=6) + 
   scale_fill_manual(values=vector_fill,drop=F)+
   scale_y_continuous(name="FSC-A",breaks=breaks.FSC,labels=labels.FSC, limits=c(breaks.FSC[1],breaks.FSC[length(breaks.FSC)]))+
   scale_x_continuous(name="SSC-A",breaks=breaks.SSC,labels=labels.SSC, limits=c(breaks.SSC[1],breaks.SSC[length(breaks.SSC)]))

 # scale_alpha_continuous(range = c(0, 1))+


   
   
 Density_2D<-Density_2D+
   facet_grid(subpopulation ~ ., scales='free_x', space='free_x', switch="y", drop=F)+
   theme_cowplot(font_size = 4)+
   theme( strip.background = element_blank(),
          strip.placement = "outside",
          strip.text = element_text(size=6),
          panel.spacing = unit(0.2, "lines"),
          panel.background=element_rect(fill="white"),
          panel.border=element_rect(colour="white",size=0,5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    theme_classic()+
    theme(axis.title.y=element_text(size=8, color="black", family="sans"),
          axis.title.x=element_text(size=8, color="black", family="sans"),
          axis.text.y=element_text(size=6, color="black", family="sans"),
          axis.text.x=element_text(size=6, color="black", family="sans"),
          axis.line.x = element_line(size = 0.4),
          axis.ticks.x = element_line(size = 0.4),
          axis.ticks.y = element_line(size = 0.4),
          axis.line.y = element_line(size = 0.4))+
    theme(legend.title = element_text(size=6, color="black", family="sans"),
          legend.text = element_text(size=6, color="black", family="sans"),
          legend.key.size = unit(0.25, 'cm'), #change legend key size
          legend.key.height = unit(0.25, 'cm'), #change legend key height
          legend.key.width = unit(0.25, 'cm'), #change legend key width
          legend.position="right")+
    ggeasy::easy_center_title()
   
   
   
   Density_1D_FSC<-ggplot(data=object_subset,
                      aes(x=FSC, color=Genotype, fill=Genotype)) +
     geom_density(alpha=0.3)+
     scale_fill_manual(values=vector_fill,drop=F)+
     scale_color_manual(values=vector_fill,drop=F)+
   scale_x_continuous(name="FSC-A",breaks=breaks.FSC,labels=labels.FSC, limits=c(breaks.FSC[1],breaks.FSC[length(breaks.FSC)]))
     
   
   Density_1D_FSC<-Density_1D_FSC+
     facet_grid(subpopulation ~ ., scales='free_x', space='free_x', switch="y", drop=F)+
     theme_cowplot(font_size = 4)+
     theme( strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(size=6),
            panel.spacing = unit(0.2, "lines"),
            panel.background=element_rect(fill="white"),
            panel.border=element_rect(colour="white",size=0,5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())+
     geom_vline(data=Summary_table, aes(xintercept=M_FSC, colour=Genotype),
                linetype="dashed", size=0.5) +
     theme_classic()+
     theme(axis.title.y=element_text(size=8, color="black", family="sans"),
           axis.title.x=element_text(size=8, color="black", family="sans"),
           axis.text.y=element_text(size=6, color="black", family="sans"),
           axis.text.x=element_text(size=6, color="black", family="sans"),
           axis.line.x = element_line(size = 0.4),
           axis.ticks.x = element_line(size = 0.4),
           axis.ticks.y = element_line(size = 0.4),
           axis.line.y = element_line(size = 0.4))+
     theme(legend.title = element_text(size=6, color="black", family="sans"),
           legend.text = element_text(size=6, color="black", family="sans"),
           legend.key.size = unit(0.25, 'cm'), #change legend key size
           legend.key.height = unit(0.25, 'cm'), #change legend key height
           legend.key.width = unit(0.25, 'cm'), #change legend key width
           legend.position="hidden")+
     ggeasy::easy_center_title()
   
   Density_1D_SSC<-ggplot(data=object_subset,
                          aes(x=SSC, color=Genotype, fill=Genotype)) +
     geom_density(alpha=0.3)+
     scale_fill_manual(values=vector_fill,drop=F)+
     scale_color_manual(values=vector_fill,drop=F)+
     scale_x_continuous(name="SSC-A",breaks=breaks.SSC,labels=labels.SSC, limits=c(breaks.SSC[1],breaks.SSC[length(breaks.SSC)]))
   
   
   Density_1D_SSC<-Density_1D_SSC+
     facet_grid(subpopulation ~ ., scales='free_x', space='free_x', switch="y", drop=F)+
     theme_cowplot(font_size = 4)+
     theme( strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(size=6),
            panel.spacing = unit(0.2, "lines"),
            panel.background=element_rect(fill="white"),
            panel.border=element_rect(colour="white",size=0,5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())+
     geom_vline(data=Summary_table, aes(xintercept=M_SSC, colour=Genotype),
                linetype="dashed", size=0.5) +
     theme_classic()+
     theme(axis.title.y=element_text(size=8, color="black", family="sans"),
           axis.title.x=element_text(size=8, color="black", family="sans"),
           axis.text.y=element_text(size=6, color="black", family="sans"),
           axis.text.x=element_text(size=6, color="black", family="sans"),
           axis.line.x = element_line(size = 0.4),
           axis.ticks.x = element_line(size = 0.4),
           axis.ticks.y = element_line(size = 0.4),
           axis.line.y = element_line(size = 0.4))+
     theme(legend.title = element_text(size=6, color="black", family="sans"),
           legend.text = element_text(size=6, color="black", family="sans"),
           legend.key.size = unit(0.25, 'cm'), #change legend key size
           legend.key.height = unit(0.25, 'cm'), #change legend key height
           legend.key.width = unit(0.25, 'cm'), #change legend key width
           legend.position="bottom")+
     ggeasy::easy_center_title()
 
 
   
  
   
   
   FSC_violin<-ggplot(data=object_subset,
                      aes(x=Genotype, y=FSC, fill=Genotype)) +
     geom_violin()+
     stat_summary(fun = median, fun.min = median, fun.max = median,
                  geom = "crossbar", width = 0.5)+
     scale_y_continuous(name="FSC-A",breaks=breaks.FSC,labels=labels.FSC, limits=c(breaks.FSC[1],breaks.FSC[length(breaks.FSC)]))+
     scale_fill_manual(values=vector_fill,drop=F)+
     scale_x_discrete(name=NULL, drop=F)
   
   
   
   FSC_violin<-FSC_violin+
     facet_grid(subpopulation ~ ., scales='free_x', space='free_x', switch="y", drop=F)+
     theme_cowplot(font_size = 4)+
     theme( strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(size=6),
            panel.spacing = unit(0.2, "lines"),
            panel.background=element_rect(fill="white"),
            panel.border=element_rect(colour="white",size=0,5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())+
     theme_classic()+
     theme(axis.title.y=element_text(size=8, color="black", family="sans"),
           axis.title.x=element_blank(),
           axis.text.y=element_text(size=6, color="black", family="sans"),
           axis.text.x=element_text(size=6, color="black", family="sans"),
           axis.line.x = element_line(size = 0.4),
           axis.ticks.x = element_line(size = 0.4),
           axis.ticks.y = element_line(size = 0.4),
           axis.line.y = element_line(size = 0.4))+
     theme(legend.title = element_text(size=6),
           legend.text = element_text(size=6),
           legend.key.size = unit(0.25, 'cm'), #change legend key size
           legend.key.height = unit(0.25, 'cm'), #change legend key height
           legend.key.width = unit(0.25, 'cm'), #change legend key width
           legend.position="hidden")+
     ggeasy::easy_center_title()
   
   
   SSC_violin<-ggplot(data=object_subset,
                      aes(x=Genotype, y=SSC, fill=Genotype)) +
     geom_violin()+
     stat_summary(fun = median, fun.min = median, fun.max = median,
                  geom = "crossbar", width = 0.5)+
     scale_y_continuous(name="SSC-A",breaks=breaks.SSC,labels=labels.SSC, limits=c(breaks.SSC[1],breaks.SSC[length(breaks.SSC)]))+
     scale_fill_manual(values=vector_fill,drop=F)+
     scale_x_discrete(name=NULL, drop=F)
   
   
   
   SSC_violin<-SSC_violin+
     facet_grid(subpopulation ~ ., scales='free_x', space='free_x', switch="y", drop=F)+
     theme_cowplot(font_size = 4)+
     theme( strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_text(size=6),
            panel.spacing = unit(0.2, "lines"),
            panel.background=element_rect(fill="white"),
            panel.border=element_rect(colour="white",size=0,5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())+
     theme_classic()+
     theme(axis.title.y=element_text(size=8, color="black", family="sans"),
           axis.title.x=element_blank(),
           axis.text.y=element_text(size=6, color="black", family="sans"),
           axis.text.x=element_text(size=6, color="black", family="sans"),
           axis.line.x = element_line(size = 0.4),
           axis.ticks.x = element_line(size = 0.4),
           axis.ticks.y = element_line(size = 0.4),
           axis.line.y = element_line(size = 0.4))+
     theme(legend.title = element_text(size=6),
           legend.text = element_text(size=6),
           legend.key.size = unit(0.25, 'cm'), #change legend key size
           legend.key.height = unit(0.25, 'cm'), #change legend key height
           legend.key.width = unit(0.25, 'cm'), #change legend key width
           legend.position="hidden")+
     ggeasy::easy_center_title()
  
 cat("path_graphs\n")
 cat(sprintf(as.character(path_graphs)))
 cat("\n")
 
 setwd(path_graphs)
 
 svgname<-paste(paste('Density_2D', sep='_'),".svg",sep='')
 makesvg = TRUE

 if (makesvg == TRUE)
 {
   ggsave(svgname, plot= Density_2D,
          device="svg",
          height=2,  width=4)
 }
 
 svgname<-paste(paste('Density_1D_FSC', sep='_'),".svg",sep='')
 makesvg = TRUE
 
 if (makesvg == TRUE)
 {
   ggsave(svgname, plot= Density_1D_FSC,
          device="svg",
          height=2,  width=4)
 }
 
 svgname<-paste(paste('Density_1D_SSC', sep='_'),".svg",sep='')
 makesvg = TRUE
 
 if (makesvg == TRUE)
 {
   ggsave(svgname, plot= Density_1D_SSC,
          device="svg",
          height=2,  width=4)
 }
 
  
 svgname<-paste(paste('FSC_violin_72h_G', sep='_'),".svg",sep='')
 makesvg = TRUE
 
 if (makesvg == TRUE)
 {
   ggsave(svgname, plot= FSC_violin,
          device="svg",
          height=2,  width=4)
 }
 
 
 svgname<-paste(paste('SSC_violin_72h_G', sep='_'),".svg",sep='')
 makesvg = TRUE
 
 if (makesvg == TRUE)
 {
   ggsave(svgname, plot= SSC_violin,
          device="svg",
          height=2,  width=4)
 }
 
 graph_DEF<-plot_grid(Density_1D_FSC,Density_1D_SSC,
                      nrow = 2,
                      ncol = 1,
                      rel_heights=c(1,1.35))
 
 
 
 

 svgname<-paste(paste("GRAPH_DEF",'FSC_SSC', sep='_'),".svg",sep='')
 makesvg = TRUE
 
 if (makesvg == TRUE)
 {
   ggsave(svgname, plot= graph_DEF,
          device="svg",
          height=3,  width=5)
   
 }
 

}







printList = function(l, prefix = "    ") {
  list.df = data.frame(val_name = names(l), value = as.character(l))
  list_strs = apply(list.df, MARGIN = 1, FUN = function(x) { paste(x, collapse = " = ")})
  cat(paste(paste(paste0(prefix, list_strs), collapse = "\n"), "\n"))
}



#### main script ----

main = function() {
  cmd_line = commandArgs()
  cat("Command line:\n")
  cat(paste(gsub("--file=", "", cmd_line[4], fixed=T),
            paste(cmd_line[6:length(cmd_line)], collapse = " "),
            "\n\n"))
  option_list <- list(
    make_option(c("--time_point_selected"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--subpopulation_selected"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--type"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--out"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required.")
  )
  parser = OptionParser(usage = "140__Rscript_v106.R
                        --subset type
                        --TranscriptEXP FILE.txt
                        --cadd FILE.txt
                        --ncboost FILE.txt
                        --type type
                        --out filename",
                        option_list = option_list)
  opt <<- parse_args(parser)
  
  graphs_2D(opt)

  
}


###########################################################################

system.time( main() )
  