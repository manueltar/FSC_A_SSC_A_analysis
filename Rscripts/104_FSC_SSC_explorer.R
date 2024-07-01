
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
  
  #### READ and transform path_24h ----
  
  path_24h = opt$path_24h
  
  cat("path_24h_\n")
  cat(sprintf(as.character(path_24h)))
  cat("\n")
 
  #### READ and transform path_48h ----
  
  path_48h = opt$path_48h
  
  cat("path_48h_\n")
  cat(sprintf(as.character(path_48h)))
  cat("\n")
  
  #### READ and transform path_72h ----
  
  path_72h = opt$path_72h
  
  cat("path_72h_\n")
  cat(sprintf(as.character(path_72h)))
  cat("\n")
  
  #### LOOP TO OPEN ALL THE FILES ----
  
  
  Path_df<-as.data.frame(rbind(cbind('24_hrs',path_24h),
                               cbind('48_hrs',path_48h),
                               cbind('72_hrs',path_72h)), stringsAsFactors=F)
  
  colnames(Path_df)<-c("time_point","path")
  
  cat("Path_df_0\n")
  cat(str(Path_df))
  cat("\n")
  
  DEBUG<-0
  
  RESULT<-data.frame()
  
  for(i in 1:dim(Path_df)[1])
  {
    time_point_sel<-Path_df$time_point[i]
    path_sel<-Path_df$path[i]
    
    cat("----------------------------------->\t")
    cat(sprintf(as.character(time_point_sel)))
    cat("\t")
    cat(sprintf(as.character(path_sel)))
    cat("\n")
    
    if(file.exists(path_sel))
    {
      file_list <- list.files(path=path_sel, include.dirs = FALSE)
      
      if(DEBUG == 1)
      {
        cat("file_list\n")
        cat(str(file_list))
        cat("\n")
      }
    
      
      indexes_sel <- grep("\\.csv$",file_list)
      
      if(DEBUG == 1)
      {
        cat("indexes_sel\n")
        cat(sprintf(as.character(indexes_sel)))
        cat("\n")
        
      }
      
      file_list_sel <- as.data.frame(file_list[indexes_sel], stringsAsFactors=F)
      colnames(file_list_sel)<-"file"
      
      if(DEBUG == 1)
      {
        cat("file_list_sel_0\n")
        cat(str(file_list_sel))
        cat("\n")
      }
      
      file_list_sel$sample<-gsub("__.+$","",file_list_sel$file)
      file_list_sel$subpopulation<-gsub(paste(file_list_sel$sample,collapse ="|"),"",file_list_sel$file)
      file_list_sel$subpopulation<-gsub("^__","",file_list_sel$subpopulation)
      file_list_sel$subpopulation<-gsub("\\.csv$","",file_list_sel$subpopulation)
      
      
      if(DEBUG == 1)
      {
        cat("file_list_sel_1\n")
        cat(str(file_list_sel))
        cat("\n")
       
      }
      
      if(dim(file_list_sel)[1] >0)
      {
        for(k in 1:dim(file_list_sel)[1])
        {
          
          file_list_sel_FINAL<-file_list_sel[k,]
          
          sample_sel<-file_list_sel_FINAL$sample
          subpopulation_sel<-file_list_sel_FINAL$subpopulation
          FINAL_file<-file_list_sel_FINAL$file
          
          if(DEBUG == 1)
          {
            cat("----->\t")
            cat(sprintf(as.character(FINAL_file)))
            cat("\t")
            cat(sprintf(as.character(sample_sel)))
            cat("\t")
            cat(sprintf(as.character(subpopulation_sel)))
            cat("\n")
          }
          
          setwd(path_sel)
          
          df<-as.data.frame(fread(file=FINAL_file, sep=',', header=T), stringsAsFactors=F)
          
          if(DEBUG == 1)
          {
            cat("df_0\n")
            cat(str(df))
            cat("\n")
            
          }
          
          df$sample<-sample_sel
          df$subpopulation<-subpopulation_sel
          df$time_point<-time_point_sel
          
          if(DEBUG == 1)
          {
            cat("df_1\n")
            cat(str(df))
            cat("\n")
            
          }
          
          RESULT<-rbind(df,RESULT)
          
        }#k in 1:dim(file_list_sel)[1]
      }#dim(file_list_sel)[1] >0
    }#file.exists(path_sel)
  }#(i in 1:dim(Path_df)[1]
  
  if(dim(RESULT)[1] >0)
  {
    
    cat("RESULT_0\n")
    cat(str(RESULT))
    cat("\n")
    cat(sprintf(as.character(names(summary(as.factor(RESULT$time_point))))))
    cat("\n")
    cat(sprintf(as.character(summary(as.factor(RESULT$time_point)))))
    cat("\n")
    cat(sprintf(as.character(names(summary(as.factor(RESULT$subpopulation))))))
    cat("\n")
    cat(sprintf(as.character(summary(as.factor(RESULT$subpopulation)))))
    cat("\n")
    cat(sprintf(as.character(names(summary(as.factor(RESULT$sample))))))
    cat("\n")
    cat(sprintf(as.character(summary(as.factor(RESULT$sample)))))
    cat("\n")
    
    RESULT$time_point<-factor(RESULT$time_point,
                              levels=c("24_hrs","48_hrs","72_hrs"),
                              ordered=T)
    
    RESULT$subpopulation<-factor(RESULT$subpopulation,
                              levels=c("Double_pos","Single_pos"),
                              ordered=T)
    
    RESULT$sample<-factor(RESULT$sample,
                              levels=c("WT_A","WT_B","WT_C","KI_13","KI_27","KI_29","Del_233","Del_235","Del_287"),
                              ordered=T)
    
    
    RESULT$Genotype<-NA
    
    RESULT$Genotype[which(RESULT$sample%in%c("WT_A","WT_B","WT_C"))]<-'GG'
    RESULT$Genotype[which(RESULT$sample%in%c("KI_13","KI_27","KI_29"))]<-'AA'
    RESULT$Genotype[which(RESULT$sample%in%c("Del_233","Del_235","Del_287"))]<-'Del80'
    
    
    RESULT$Genotype<-factor(RESULT$Genotype,
                          levels=c("GG","AA","Del80"),
                          ordered=T)
    
    cat("RESULT_1\n")
    cat(str(RESULT))
    cat("\n")
    cat(sprintf(as.character(names(summary(RESULT$time_point)))))
    cat("\n")
    cat(sprintf(as.character(summary(RESULT$time_point))))
    cat("\n")
    cat(sprintf(as.character(names(summary(RESULT$subpopulation)))))
    cat("\n")
    cat(sprintf(as.character(summary(RESULT$subpopulation))))
    cat("\n")
    cat(sprintf(as.character(names(summary(RESULT$sample)))))
    cat("\n")
    cat(sprintf(as.character(summary(RESULT$sample))))
    cat("\n")
    cat(sprintf(as.character(names(summary(RESULT$Genotype)))))
    cat("\n")
    cat(sprintf(as.character(summary(RESULT$Genotype))))
    cat("\n")
    
    setwd(out)
    
    saveRDS(RESULT, file='collected_data.rds')
    
    
    check<-droplevels(RESULT[which(RESULT$time_point == '72_hrs' &
                          RESULT$subpopulation == 'Single_pos'),])
    
    cat("check_0\n")
    cat(str(check))
    cat("\n")
    cat(sprintf(as.character(names(summary(check$time_point)))))
    cat("\n")
    cat(sprintf(as.character(summary(check$time_point))))
    cat("\n")
    cat(sprintf(as.character(names(summary(check$subpopulation)))))
    cat("\n")
    cat(sprintf(as.character(summary(check$subpopulation))))
    cat("\n")
    cat(sprintf(as.character(names(summary(check$sample)))))
    cat("\n")
    cat(sprintf(as.character(summary(check$sample))))
    cat("\n")
    cat(sprintf(as.character(names(summary(check$Genotype)))))
    cat("\n")
    cat(sprintf(as.character(summary(check$Genotype))))
    cat("\n")


 }#dim(RESULT)[1] >0
  
  
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
  