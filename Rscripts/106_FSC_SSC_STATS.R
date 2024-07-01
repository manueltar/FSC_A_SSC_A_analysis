
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

FSC_stats = function(option_list)
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
  
  time_point_selected = opt$time_point_selected
  
  cat("time_point_selected\n")
  cat(sprintf(as.character(time_point_selected)))
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
 
 
 object_subset<-object[which(object$time_point%in%time_point_selected),]
 
 
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
 
 
 #### LOOP to run comparisons ----
 

 path_stats<-paste(out,'stats','/',sep='')
 
 if (file.exists(path_stats)){
   
   
 }else{
   
   dir.create(file.path(path_stats))
   
 }#path_stats
 
 
 array_subpopulations<-levels(object_subset$subpopulation)
 
 DEBUG <-0
 
 RESULTS<-data.frame()
 
 for(i in 1:length(array_subpopulations))
 {
   array_subpopulations_sel<-array_subpopulations[i]
   
   cat("----------------------------------->\t")
   cat(sprintf(as.character(array_subpopulations_sel)))
   cat("\n")
   
   object_subset_supopulation_subset<-object_subset[which(object_subset$subpopulation%in%array_subpopulations_sel),]
   
   if(DEBUG == 1)
   {
     cat("object_subset_supopulation_subset\n")
     cat(str(object_subset_supopulation_subset))
     cat("\n")
   }
   
   #### Summary_table ----
   
   object_subset_supopulation_subset.dt<-data.table(object_subset_supopulation_subset, key=c("Genotype"))
   
   Summary_table<-as.data.frame(object_subset_supopulation_subset.dt[,.(n=.N,
                                                    Min=round(as.numeric(summary(FSC)[1]),0),
                                                    Q1=round(as.numeric(summary(FSC)[2]),0),
                                                    M=round(as.numeric(summary(FSC)[3]),0),
                                                    Q3=round(as.numeric(summary(FSC)[5]),0),
                                                    Max=round(as.numeric(summary(FSC)[6]),0)),, by=key(object_subset_supopulation_subset.dt)], stringsAsFactors=F)
   
   
   cat("Summary_table_1\n")
   cat(str(Summary_table))
   cat("\n")
   
   #### Pairwise comparisons wilcoxon ----
   
   PW_comparison<-pairwise.wilcox.test(object_subset_supopulation_subset$FSC, object_subset_supopulation_subset$Genotype,
                                       p.adjust.method = "BH")
   
   
   if(DEBUG == 1)
   {
     cat("PW_comparison\n")
     cat(str(PW_comparison))
     cat("\n")
   }
   
   PW_comparison_pvalue_df<-as.data.frame(PW_comparison$p.value, stringsAsFactors=F)
   
   if(DEBUG == 1)
   {
     cat("PW_comparison_pvalue_df\n")
     cat(str(PW_comparison_pvalue_df))
     cat("\n")
   }
   
   list_cols<-list()
   
   for(PW_iteration in 1:dim(PW_comparison_pvalue_df)[2])
   {
     colnames_sel<-colnames(PW_comparison_pvalue_df)[PW_iteration]
     
     if(DEBUG == 1)
     {
       cat("----------------->colnames_sel\n")
       cat(sprintf(as.character(colnames_sel)))
       cat("\n")
     }
     
     list_rows<-list()
     
     
     for(PW_iteration_k in 1:dim(PW_comparison_pvalue_df)[1])
     {
       rownames_sel<-row.names(PW_comparison_pvalue_df)[PW_iteration_k]
       
       if(DEBUG == 1)
       {
         cat("--->rownames_sel\n")
         cat(sprintf(as.character(rownames_sel)))
         cat("\n")
       }
       
       PW_Wilcox_pvalue<-PW_comparison_pvalue_df[PW_iteration_k,PW_iteration]
       
       if(DEBUG == 1)
       {
         cat("PW_Wilcox_pvalue\n")
         cat(sprintf(as.character(PW_Wilcox_pvalue)))
         cat("\n")
       }
       
       
       log_pval_PW_Wilcox<-round(-1*log10(PW_Wilcox_pvalue),4)
       
       if(DEBUG == 1)
       {
         cat("log_pval_PW_Wilcox\n")
         cat(sprintf(as.character(log_pval_PW_Wilcox)))
         cat("\n")
       }
       
       FLAG_NA<-sum(is.na(log_pval_PW_Wilcox))
       
       
       if(DEBUG == 1)
       {
         cat("FLAG_NA\n")
         cat(sprintf(as.character(FLAG_NA)))
         cat("\n")
       }
       
       if(FLAG_NA ==0)
       {
         vector_final_comparisons<-paste(sort(c(colnames_sel,rownames_sel)), collapse=";")
         
         
         a.dt<-as.data.frame(cbind(vector_final_comparisons,PW_Wilcox_pvalue,log_pval_PW_Wilcox), stringsAsFactors=F)
         
         colnames(a.dt)<-c("comparison",'pval','MINUS_logpval')
         
         if(DEBUG == 1)
         {
           cat("a.dt\n")
           cat(str(a.dt))
           cat("\n")
         }
         
         list_rows[[PW_iteration_k]]<-a.dt
         
       }#FLAG_NA ==0
       
       
     }#PW_iteration_k
     
     if(length(list_rows) >0)
     {
       
       df_col = as.data.frame(data.table::rbindlist(list_rows, fill=T), stringsAsFactors=F)
       
       if(DEBUG == 1)
       {
         cat("df_col\n")
         cat(str(df_col))
         cat("\n")
       }
       
       list_cols[[PW_iteration]]<-df_col
     }#length(list_rows) >0
     
   }#PW_iteration
   
   if(length(list_cols) >0)
   {
     PW_comparison = as.data.frame(data.table::rbindlist(list_cols, fill=T), stringsAsFactors=F)
     
     PW_comparison[,which(colnames(PW_comparison) == 'pval')]<-as.numeric(PW_comparison[,which(colnames(PW_comparison) == 'pval')])
     PW_comparison[,which(colnames(PW_comparison) == 'MINUS_logpval')]<-as.numeric(PW_comparison[,which(colnames(PW_comparison) == 'MINUS_logpval')])
     
     if(DEBUG == 1)
     {
       cat("PW_comparison\n")
       cat(str(PW_comparison))
       cat("\n")
     }
     
     
     PW_comparison_NO_NA<-PW_comparison[!is.na(PW_comparison[,which(colnames(PW_comparison) == 'MINUS_logpval')]),]
     
     PW_comparison_NO_NA$c1<-gsub(";.+$","",PW_comparison_NO_NA$comparison)
     PW_comparison_NO_NA$c2<-gsub("^[^;]+;","",PW_comparison_NO_NA$comparison)
     
     
     if(DEBUG == 1)
     {
       cat("PW_comparison_NO_NA_0\n")
       cat(str(PW_comparison_NO_NA))
       cat("\n")
     }
     
     Summary_table_c1<-Summary_table
     colnames(Summary_table_c1)[which(colnames(Summary_table_c1) == 'Genotype')]<-'c1'
     colnames(Summary_table_c1)[-1]<-paste('c1',colnames(Summary_table_c1)[-1], sep='_')
     
     Summary_table_c2<-Summary_table
     colnames(Summary_table_c2)[which(colnames(Summary_table_c2) == 'Genotype')]<-'c2'
     colnames(Summary_table_c2)[-1]<-paste('c2',colnames(Summary_table_c2)[-1], sep='_')
     
     
     PW_comparison_NO_NA<-merge(PW_comparison_NO_NA,
                                Summary_table_c2,
                                by='c2')
     
     PW_comparison_NO_NA<-merge(PW_comparison_NO_NA,
                                Summary_table_c1,
                                by='c1')
     
     PW_comparison_NO_NA$subpopulation<-array_subpopulations_sel
     
     RESULTS<-rbind(PW_comparison_NO_NA,RESULTS)
     
   }#length(list_cols) >0
 }#i in 1:length(array_subpopulations)
 
 
 ##### Save ----
 
 setwd(path_stats)
 
 if(dim(RESULTS)[1] >0)
 {
  
     cat("RESULTS_0\n")
     cat(str(RESULTS))
     cat("\n")
  
   
   write.table(RESULTS,file='FSC_STATS.tsv', sep="\t", quote = F, row.names = F)
   
 }#dim(RESULTS)[1] >0
 
}

SSC_stats = function(option_list)
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
  
  time_point_selected = opt$time_point_selected
  
  cat("time_point_selected\n")
  cat(sprintf(as.character(time_point_selected)))
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
  
  
  object_subset<-object[which(object$time_point%in%time_point_selected),]
  
  
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
  
  
  #### LOOP to run comparisons ----
  
  
  path_stats<-paste(out,'stats','/',sep='')
  
  if (file.exists(path_stats)){
    
    
  }else{
    
    dir.create(file.path(path_stats))
    
  }#path_stats
  
  
  array_subpopulations<-levels(object_subset$subpopulation)
  
  DEBUG <- 0
  
  RESULTS<-data.frame()
  
  for(i in 1:length(array_subpopulations))
  {
    array_subpopulations_sel<-array_subpopulations[i]
    
    cat("----------------------------------->\t")
    cat(sprintf(as.character(array_subpopulations_sel)))
    cat("\n")
    
    object_subset_supopulation_subset<-object_subset[which(object_subset$subpopulation%in%array_subpopulations_sel),]
    
    if(DEBUG == 1)
    {
      cat("object_subset_supopulation_subset\n")
      cat(str(object_subset_supopulation_subset))
      cat("\n")
    }
    
    #### Summary_table ----
    
    object_subset_supopulation_subset.dt<-data.table(object_subset_supopulation_subset, key=c("Genotype"))
    
    Summary_table<-as.data.frame(object_subset_supopulation_subset.dt[,.(n=.N,
                                                     Min=round(as.numeric(summary(SSC)[1]),0),
                                                     Q1=round(as.numeric(summary(SSC)[2]),0),
                                                     M=round(as.numeric(summary(SSC)[3]),0),
                                                     Q3=round(as.numeric(summary(SSC)[5]),0),
                                                     Max=round(as.numeric(summary(SSC)[6]),0)),, by=key(object_subset_supopulation_subset.dt)], stringsAsFactors=F)
    
    
    cat("Summary_table_1\n")
    cat(str(Summary_table))
    cat("\n")
    
    #### Pairwise comparisons wilcoxon ----
    
    PW_comparison<-pairwise.wilcox.test(object_subset_supopulation_subset$SSC, object_subset_supopulation_subset$Genotype,
                                        p.adjust.method = "BH")
    
    
    if(DEBUG == 1)
    {
      cat("PW_comparison\n")
      cat(str(PW_comparison))
      cat("\n")
    }
    
    PW_comparison_pvalue_df<-as.data.frame(PW_comparison$p.value, stringsAsFactors=F)
    
    if(DEBUG == 1)
    {
      cat("PW_comparison_pvalue_df\n")
      cat(str(PW_comparison_pvalue_df))
      cat("\n")
    }
    
    list_cols<-list()
    
    for(PW_iteration in 1:dim(PW_comparison_pvalue_df)[2])
    {
      colnames_sel<-colnames(PW_comparison_pvalue_df)[PW_iteration]
      
      if(DEBUG == 1)
      {
        cat("----------------->colnames_sel\n")
        cat(sprintf(as.character(colnames_sel)))
        cat("\n")
      }
      
      list_rows<-list()
      
      
      for(PW_iteration_k in 1:dim(PW_comparison_pvalue_df)[1])
      {
        rownames_sel<-row.names(PW_comparison_pvalue_df)[PW_iteration_k]
        
        if(DEBUG == 1)
        {
          cat("--->rownames_sel\n")
          cat(sprintf(as.character(rownames_sel)))
          cat("\n")
        }
        
        PW_Wilcox_pvalue<-PW_comparison_pvalue_df[PW_iteration_k,PW_iteration]
        
        if(DEBUG == 1)
        {
          cat("PW_Wilcox_pvalue\n")
          cat(sprintf(as.character(PW_Wilcox_pvalue)))
          cat("\n")
        }
        
        
        log_pval_PW_Wilcox<-round(-1*log10(PW_Wilcox_pvalue),4)
        
        if(DEBUG == 1)
        {
          cat("log_pval_PW_Wilcox\n")
          cat(sprintf(as.character(log_pval_PW_Wilcox)))
          cat("\n")
        }
        
        FLAG_NA<-sum(is.na(log_pval_PW_Wilcox))
        
        
        if(DEBUG == 1)
        {
          cat("FLAG_NA\n")
          cat(sprintf(as.character(FLAG_NA)))
          cat("\n")
        }
        
        if(FLAG_NA ==0)
        {
          vector_final_comparisons<-paste(sort(c(colnames_sel,rownames_sel)), collapse=";")
          
          
          a.dt<-as.data.frame(cbind(vector_final_comparisons,PW_Wilcox_pvalue,log_pval_PW_Wilcox), stringsAsFactors=F)
          
          colnames(a.dt)<-c("comparison",'pval','MINUS_logpval')
          
          if(DEBUG == 1)
          {
            cat("a.dt\n")
            cat(str(a.dt))
            cat("\n")
          }
          
          list_rows[[PW_iteration_k]]<-a.dt
          
        }#FLAG_NA ==0
        
        
      }#PW_iteration_k
      
      if(length(list_rows) >0)
      {
        
        df_col = as.data.frame(data.table::rbindlist(list_rows, fill=T), stringsAsFactors=F)
        
        if(DEBUG == 1)
        {
          cat("df_col\n")
          cat(str(df_col))
          cat("\n")
        }
        
        list_cols[[PW_iteration]]<-df_col
      }#length(list_rows) >0
      
    }#PW_iteration
    
    if(length(list_cols) >0)
    {
      PW_comparison = as.data.frame(data.table::rbindlist(list_cols, fill=T), stringsAsFactors=F)
      
      PW_comparison[,which(colnames(PW_comparison) == 'pval')]<-as.numeric(PW_comparison[,which(colnames(PW_comparison) == 'pval')])
      PW_comparison[,which(colnames(PW_comparison) == 'MINUS_logpval')]<-as.numeric(PW_comparison[,which(colnames(PW_comparison) == 'MINUS_logpval')])
      
      if(DEBUG == 1)
      {
        cat("PW_comparison\n")
        cat(str(PW_comparison))
        cat("\n")
      }
      
      
      PW_comparison_NO_NA<-PW_comparison[!is.na(PW_comparison[,which(colnames(PW_comparison) == 'MINUS_logpval')]),]
      
      PW_comparison_NO_NA$c1<-gsub(";.+$","",PW_comparison_NO_NA$comparison)
      PW_comparison_NO_NA$c2<-gsub("^[^;]+;","",PW_comparison_NO_NA$comparison)
      
      
      if(DEBUG == 1)
      {
        cat("PW_comparison_NO_NA_0\n")
        cat(str(PW_comparison_NO_NA))
        cat("\n")
      }
      
      Summary_table_c1<-Summary_table
      colnames(Summary_table_c1)[which(colnames(Summary_table_c1) == 'Genotype')]<-'c1'
      colnames(Summary_table_c1)[-1]<-paste('c1',colnames(Summary_table_c1)[-1], sep='_')
      
      Summary_table_c2<-Summary_table
      colnames(Summary_table_c2)[which(colnames(Summary_table_c2) == 'Genotype')]<-'c2'
      colnames(Summary_table_c2)[-1]<-paste('c2',colnames(Summary_table_c2)[-1], sep='_')
      
      
      PW_comparison_NO_NA<-merge(PW_comparison_NO_NA,
                                 Summary_table_c2,
                                 by='c2')
      
      PW_comparison_NO_NA<-merge(PW_comparison_NO_NA,
                                 Summary_table_c1,
                                 by='c1')
      
      PW_comparison_NO_NA$subpopulation<-array_subpopulations_sel
      
      RESULTS<-rbind(PW_comparison_NO_NA,RESULTS)
      
    }#length(list_cols) >0
  }#i in 1:length(array_subpopulations)
  
  
  ##### Save ----
  
  setwd(path_stats)
  
  if(dim(RESULTS)[1] >0)
  {
   
    cat("RESULTS_0\n")
    cat(str(RESULTS))
    cat("\n")
   
    
    write.table(RESULTS,file='SSC_STATS.tsv', sep="\t", quote = F, row.names = F)
    
  }#dim(RESULTS)[1] >0
  
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
  
  FSC_stats(opt)
  SSC_stats(opt)
  
  
}


###########################################################################

system.time( main() )
  