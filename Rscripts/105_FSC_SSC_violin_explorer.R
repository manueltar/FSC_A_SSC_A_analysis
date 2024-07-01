
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

data_wrangling = function(option_list)
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
 
 
 
 path_graphs<-paste(out,'graphs','/',sep='')
 
 if (file.exists(path_graphs)){
   
   
 }else{
   
   dir.create(file.path(path_graphs))
   
 }#path_graphs
 
 
 vector_colors_WT<-brewer.pal(9, "YlGn")[c(4:6)]
 vector_colors_KI<-brewer.pal(9, "YlOrRd")[c(7:9)]
 vector_colors_Del<-brewer.pal(9, "PuBu")[c(6:8)]
 
 
 vector_colors<-c(vector_colors_WT,vector_colors_KI,vector_colors_Del)
 
 
 cat("vector_colors\n")
 cat(str(vector_colors))
 cat("\n")
 
 A<-round(summary(object$FSC[!is.na(object$FSC)]),0)
 
 
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
 
 breaks.Rank<-sort(unique(c(A[6],seq(from= A[1], to=A[6],by=step))))
 labels.Rank<-as.character(round(breaks.Rank,0))
 
 
 
 cat("labels.Rank:\t")
 cat(sprintf(as.character(labels.Rank)))
 cat("\n")
   
   
 
 
 
 
 FSC_violin<-ggplot(data=object,
                          aes(x=sample, y=FSC, fill=sample)) +
   geom_violin()+
   stat_summary(fun = median, fun.min = median, fun.max = median,
                geom = "crossbar", width = 0.5)+
   scale_y_continuous(name="FSC-A",breaks=breaks.Rank,labels=labels.Rank, limits=c(breaks.Rank[1],breaks.Rank[length(breaks.Rank)]))+
   scale_fill_manual(values=vector_colors,drop=F)+
   scale_x_discrete(name=NULL, drop=F)
 
 
 
 FSC_violin<-FSC_violin+
   facet_grid(subpopulation ~ time_point, scales='free_x', space='free_x', switch="y", drop=F)+
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
         axis.text.x=element_text(angle=90,size=6,vjust=1,hjust=1, color="black", family="sans"),
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
 
 
 setwd(path_graphs)
 
 svgname<-paste(paste('FSC_violin', sep='_'),".svg",sep='')
 makesvg = TRUE
 
 if (makesvg == TRUE)
 {
   ggsave(svgname, plot= FSC_violin,
          device="svg",
          height=6, width=6)
 }
 
 A<-round(summary(object$SSC[!is.na(object$SSC)]),0)
 
 
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
 
 breaks.Rank<-sort(unique(c(A[6],seq(from= A[1], to=A[6],by=step))))
 labels.Rank<-as.character(round(breaks.Rank,0))
 
 
 
 cat("labels.Rank:\t")
 cat(sprintf(as.character(labels.Rank)))
 cat("\n")
 
 SSC_violin<-ggplot(data=object,
                    aes(x=sample, y=SSC, fill=sample)) +
   geom_violin()+
   stat_summary(fun = median, fun.min = median, fun.max = median,
                geom = "crossbar", width = 0.5)+
   scale_y_continuous(name="SSC-A",breaks=breaks.Rank,labels=labels.Rank, limits=c(breaks.Rank[1],breaks.Rank[length(breaks.Rank)]))+
   scale_fill_manual(values=vector_colors,drop=F)+
   scale_x_discrete(name=NULL, drop=F)
 
 
 
 SSC_violin<-SSC_violin+
   facet_grid(subpopulation ~ time_point, scales='free_x', space='free_x', switch="y", drop=F)+
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
         axis.text.x=element_text(angle=90,size=6,vjust=1,hjust=1, color="black", family="sans"),
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
 
 
 setwd(path_graphs)
 
 svgname<-paste(paste('SSC_violin', sep='_'),".svg",sep='')
 makesvg = TRUE
 
 if (makesvg == TRUE)
 {
   ggsave(svgname, plot= SSC_violin,
          device="svg",
          height=6, width=6)
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
    make_option(c("--path_24h"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--path_48h"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--path_BP"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--path_72h"), type="character", default=NULL, 
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
  
  data_wrangling(opt)
  
  
}


###########################################################################

system.time( main() )
  