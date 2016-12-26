options(warn=1)

# ======================================
# WARNING: Make sure to install the following packages as administrator/root (for them to be available to all users)
# ======================================

#source("http://www.bioconductor.org/biocLite.R")
#if(!require("limma")){ biocLite("limma") }
#if(!require("statmod")){ biocLite("statmod") }
#if(!require("ggplot2")){ install.packages("ggplot2", repos="http://cran.fhcrc.org") }
#if(!require("stringr")){ install.packages("stringr", repos="http://cran.fhcrc.org") }
#if(!require("reshape")){ install.packages("reshape", repos="http://cran.fhcrc.org") }
#if(!require("plyr")){ install.packages("plyr", repos="http://cran.fhcrc.org") }
#if(!require("gtools")){ install.packages("gtools", repos="http://cran.fhcrc.org") }
#if(!require("gtools")){ install.packages("labeling", repos="http://cran.fhcrc.org") }
#if(!require("data.table")){ install.packages("data.table", repos="http://cran.fhcrc.org") }
#if(!require("outliers")){ install.packages("outliers", repos="http://cran.fhcrc.org") }
#if(!require("pryr")){ install.packages("pryr", repos="http://cran.fhcrc.org") }
##if(!require("devtools")){ { install.packages("devtools", repos="http://cran.fhcrc.org") }
##if(!require("lineprof")){ devtools::install_github("hadley/lineprof") }

library(limma)
library(statmod)
library(stringr)
library(reshape)
library(plyr)
library(ggplot2)
library(labeling)
library(gtools)
library(data.table)
library(outliers)
library(pryr)
#library(devtools)
#library(lineprof)


# DEBUGGING log flag/level (0 translates to no debugging log at all)
debuglog <- 10
# DEBUGGING log indentation level (psoitive int, global) for levellog function
loglvl <- 0
lastSysTime <- NA
firstSysTime <- NA
#  Print msg with specific indentation (loglvl*3)
#  - change [int]: change previous indentation level by <change> amount
#  - reset [bool]: reset indentation level to 0 (no indentation)
#  - after [bool]: change indentation after printing msg
levellog <- function (msg, change=0, reset=F,after=F, supression=debuglog){
  currSysTime<-Sys.time()
  if(is.na(firstSysTime)){
    firstSysTime <<- currSysTime
  }
  if(is.na(lastSysTime)){
    lastSysTime <<- currSysTime
    td <- 0
  }else{
    td <- difftime(currSysTime, lastSysTime, unit='secs')
    if(td < 0.1){
      td <- 0
    }
    lastSysTime <<- currSysTime
  }
  if(td > 0){
    tdstr <- sprintf('+%.1fs, ', td)
  }else{
    tdstr <- ''
  }
  tdt <- difftime(currSysTime, firstSysTime, unit='secs')
  if(tdt < 0.1){
    tdt <- 0
  }
  if(tdt > 0){
    tdstr<-paste0(tdstr, sprintf('%.1fs elapsed, ', tdt))
  }
  
  prev_loglvl <- loglvl
  if(reset){
    loglvl <<- 0
  }
  else if(change < 0){
    if(-change > loglvl){
      loglvl <<- 0
    }else{
      loglvl <<- loglvl + change
    }
  }else if (change > 0){
    loglvl <<- loglvl + change
  }
  if(nchar(msg)>0 && ifelse(after , prev_loglvl<supression , loglvl<supression)){
    if(after){
      cat(paste0(paste0(rep(" ",times=prev_loglvl*3),collapse="")," +-- ",msg," [ ", tdstr, round(mem_used()/(1000*1000), digits=1)," MB used ]\n"))
    }else{
      cat(paste0(paste0(rep(" ",times=loglvl*3),collapse="")," +-- ",msg," [ ", tdstr, round(mem_used()/(1000*1000), digits=1)," MB used ]\n"))
    }
  }
}

# FROM: http://musicroamer.com/blog/2011/01/16/r-tips-and-tricks-modified-pairs-plot/
# Scatterplot matrix functionality

panel.cor.scale <- function(x, y, digits=2, prefix="", cex.cor){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y,use="pairwise"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * abs(r))
}

panel.cor <- function(x, y, digits=2, prefix="", cex.cor){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y,use="pairwise"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex )
}

panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  #h <- hist(x, plot = FALSE)
  h <- hist(x, breaks=panel.hist.breaks,plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col=ratios.hist.colour, ...)
}

# FROM: http://www-personal.umich.edu/~ladamic/presentations/Rtutorial/Rtutorial.R
panel.lmline = function (x, y, col = par("col"), bg = NA, pch = par("pch"), cex = 1, col.smooth = reps.scatter.lmline.colour, ...){
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    abline(lm(y[ok] ~ x[ok]), 
           col = col.smooth, ...)
}

pairs.panels <- function (x,y,smooth=TRUE,scale=FALSE,lm=FALSE){
  if (smooth){
    if (scale) {
      if(lm){
        pairs(x,diag.panel=panel.hist,upper.panel=panel.cor.scale,lower.panel=panel.lmline)
      }else{
        pairs(x,diag.panel=panel.hist,upper.panel=panel.cor.scale,lower.panel=panel.smooth)
      }
    }else{
      if(lm){
        pairs(x,diag.panel=panel.hist,upper.panel=panel.cor,lower.panel=panel.lmline)
      }else{
        pairs(x,diag.panel=panel.hist,upper.panel=panel.cor,lower.panel=panel.smooth)
      }
    }
  }else{
    if(scale){
      pairs(x,diag.panel=panel.hist,upper.panel=panel.cor.scale)
    }else{
      pairs(x,diag.panel=panel.hist,upper.panel=panel.cor)
    }
  }
}

lm_eqn<-function(df){
  m = lm(y ~ x, data=df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  return(as.character(as.expression(eq)));
}

# to be used with apply on a limma-results data.frame
# calculate mean, sd and N of ratios between available channels
# conds_cols_idxs the column indexes of quant channels intensities
# x a limma-results data.frame row
calcRowStats<-function(x,conds_cols_idxs,ratio_combs){
  ratios<-c()
  m<-c()
  std<-c()
  N<-c()
  avg.I<-c()
  
  for(i in 1:nrow(ratio_combs)){
    tmp1<-as.numeric(x[conds_cols_idxs[ratio_combs[i,2],]]) #columns of heavier label (assumed to be defined after lighter label in conditions.labels)
    tmp2<-as.numeric(x[conds_cols_idxs[ratio_combs[i,1],]]) #columns of lighter label    
    ratio_i<-(tmp1-tmp2)
    ratios<-rbind(ratios, ratio_i)
    m<-rbind(m, mean(ratio_i,na.rm=T))
    std<-rbind(std, sd(ratio_i,na.rm=T))
    N<-rbind(N, length(which(!is.na(ratio_i)|ratio_i==0)))
    
    #avg.I<-rbind(avg.I, log2(mean(sapply(1:nrow(conds_cols_idxs),function(x) sum(c(2^tmp1[x],2^tmp2[x]),na.rm=T)),na.rm=T)))
    avg.I<-rbind(avg.I, mean(c(tmp1,tmp2),na.rm=T))
  }
  
  return(c(m[,1],std[,1],N[,1],avg.I[,1],as.vector(t(ratios))))
}

# Produces S-plot, Volcano plot, MA plot and Scatterplot (matrix). Called by do_limma_analysis subroutine.
do_results_plots<-function(norm.median.intensities,time.point,exportFormat="pdf",outputFigsPrefix=""){
  levellog("",change=1)
  
  ratio_combs<-combinations(nConditions,2,1:nConditions)
  levellog("Preparing data ...")
  results<-read.table(paste(outputFigsPrefix,"_condition-i_vs_condition-j_",time.point,".txt",sep=""), header = T, sep = "\t",quote='',stringsAsFactors=F,comment.char = "")
  if(nrow(ratio_combs) == 1){
    colnames(results)[grep("p\\.value\\.adj",colnames(results))]<-paste("p.value.adj.",conditions.labels[2],".",conditions.labels[1],sep="")
  }#else{
  #  for(i in 1:nrow(ratio_combs)){
  #    ratio_i_desc<-paste(conditions.labels[ratio_combs[i,2]],"\\.",conditions.labels[ratio_combs[i,1]],sep="")
  #    ratio_i_sub<-paste("L",ratio_combs[i,2],"...L",ratio_combs[i,1],sep="")
  #    colnames(results)<-gsub(ratio_i_desc,ratio_i_sub,colnames(results))
  #  }
  #}
  
  # Due to the mysterious bug (undefined results$ID column), the following does not make sense ...
  #rownames(results)<-results$ID
  #
  tmp<-as.data.frame(t(norm.median.intensities))
  
  #rownames(tmp)<-colnames(norm.median.intensities)
  
  nsamples<-length(colnames(tmp))/nConditions
  colnames(tmp)<-apply(data.frame(cbind(rep(conditions.labels,each=nsamples),rep(1:nsamples))),1,function(x) paste(x['X1'],x['X2']))
  results<-cbind(results,tmp)
  
  results$N<-apply(results[,colnames(tmp)],1,function(x)(nsamples*nConditions)-length(which(is.na(x))))
  
  levellog("Filtering data based on P-value(s) ...")
  signTruth<-rep(FALSE,nrow(results))
  for(i in 1:nrow(ratio_combs)){
    col_desc_<-paste("p.value.adj.",paste(conditions.labels[ratio_combs[i,2]],".",conditions.labels[ratio_combs[i,1]],sep=""),sep="")
    na_indexes<-which(is.na(results[,col_desc_]))
    if(length(na_indexes)>0){
      results[na_indexes,col_desc_]<-1
      signTruth<-(signTruth | results[,col_desc_]<0.05)
      results[na_indexes,col_desc_]<-NA
    }else{
      signTruth<-(signTruth | results[,col_desc_]<0.05)
    }
  }
  
  ndiffexp<-nrow(results[signTruth,])
  
  conds_cols_idxs<-c()
  for(lbl_i in conditions.labels){
    conds_cols_idxs<-rbind(conds_cols_idxs, grep(paste("^",lbl_i,sep=""),colnames(results)))
  }
  
  levellog("Calculating data frame-row statistics ...")
  d<-data.frame(t(apply(results,1, function(x) calcRowStats(x,conds_cols_idxs,ratio_combs))))
  levellog("Performing final formatting operations ...")
  colnames_d_<-c()
  
  for(i in 1:nrow(ratio_combs)){
    ratio_i_str<-paste(conditions.labels[ratio_combs[i,2]],".",conditions.labels[ratio_combs[i,1]],sep="")
    colnames_d_<-rbind(colnames_d_, c(
      paste("log2.avg.",ratio_i_str,sep=""),
      paste("log2.sd.",ratio_i_str,sep=""),
      paste("log2.N.",ratio_i_str,sep=""),
      paste("log2.avg.I.",ratio_i_str,sep="")))
  }
  colnames_d_<-as.vector(colnames_d_)
  for(i in 1:nrow(ratio_combs)){
    ratio_i_str<-paste(conditions.labels[ratio_combs[i,2]],".",conditions.labels[ratio_combs[i,1]],sep="")
    colnames_d_<-c(colnames_d_, paste(paste("log2.",ratio_i_str,sep=""),1:nsamples))
  }
  colnames(d)<-colnames_d_
  
  # Due to the mysterious bug, the following was added ...
  results$ID <- rownames(results)
  results<-cbind(results,d)
  #
  results$ID <- factor(results$ID, levels=unique(as.character(results$ID)))
  results$nID<-1:nrow(results)
  
  levellog("Plotting time ...")
  theme_set(theme_bw())
  # customized colorblind-friendly palette from http://wiki.stdout.org/rcookbook/Graphs/Colors%20(ggplot2)/
  cbPalette <- c("#999999", "#D55E00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#CC79A7")
  
  #my plots
  for(i in 1:nrow(ratio_combs)){
    levellog(paste("Generating plots for combination #",i," ..."),change=1,after=T)
    ratio_i_str<-paste(conditions.labels[ratio_combs[i,2]],".",conditions.labels[ratio_combs[i,1]],sep="")
    
    # 1 - volcano - -log10 P-value vs log ratio
    levellog("Making volcano plot ...")
    figsuffix<-paste("_",ratio_i_str,"-volcano","_",sep="")
    
    if(exportFormat == "pdf"){
      pdf(file=paste(outputFigsPrefix,figsuffix,time.point,".pdf",sep=""),width=10, height=7, family = "Helvetica", pointsize=8)
    }
    
    ratio_i_<-paste("log2.",ratio_i_str,sep="")
    ratio_i_sd_col<-paste("log2.sd.",ratio_i_str,sep="")
    tmp2<-results[,colnames(results)[grep(gsub("\\.","\\\\.",ratio_i_),colnames(results))]]+results[,colnames(results)[grep(gsub("\\.","\\\\.",ratio_i_sd_col),colnames(results))]]
    
    tmp1<-results[,colnames(results)[grep(gsub("\\.","\\\\.",ratio_i_),colnames(results))]]-results[,colnames(results)[grep(gsub("\\.","\\\\.",ratio_i_sd_col),colnames(results))]]
    ratiolim<-ceiling(max(max(range(tmp1,na.rm=T),range(tmp2,na.rm=T)),abs(min(range(tmp1,na.rm=T),range(tmp2,na.rm=T)))))
    panel.hist.breaks<<-(-ratiolim:ratiolim)
    
    ratio_i_p.value.adj<-paste("p.value.adj.",paste(conditions.labels[ratio_combs[i,2]],".",conditions.labels[ratio_combs[i,1]],sep=""),sep="")
    ratio_i_avg_col<-paste("log2.avg.",ratio_i_str,sep="")
    mlog10_ratio_i_p.value.adj<-paste("mlog10_",ratio_i_p.value.adj,sep="")
    diffexp_ratio_i<-paste("diffexp_",ratio_i_str,sep="")
    
    results[,mlog10_ratio_i_p.value.adj]<-(-log10(results[,ratio_i_p.value.adj]))
    
    na_indexes<-which(is.na(results[,ratio_i_p.value.adj]))
    if(length(na_indexes)>0){
      results[na_indexes,ratio_i_p.value.adj]<-1
      results[,diffexp_ratio_i]<-results[,ratio_i_p.value.adj]<0.05
      results[na_indexes,ratio_i_p.value.adj]<-NA
    }else{
      results[,diffexp_ratio_i]<-results[,ratio_i_p.value.adj]<0.05
    }
    
    p<-ggplot(data=results, aes_string(x=ratio_i_avg_col, y=mlog10_ratio_i_p.value.adj, colour=diffexp_ratio_i)) +
      geom_point(alpha=0.7, size=1.75) +
      theme(legend.position = "none", axis.title.y=element_text(vjust=0.2), axis.title.x=element_text(vjust=0), plot.title = element_text(vjust=1.5, lineheight=.8, face="bold")) +
      xlim(c(-ratiolim, ratiolim)) + ylim(c(0, 6)) + scale_colour_manual(values=cbPalette) +
      xlab(paste("average log2 ",sub("\\.","/",ratio_i_str),sep="")) + ylab("-log10 P-value") + ggtitle("P-value vs Fold change") +
      geom_hline(aes(yintercept=-log10(0.05)), colour="#990000", linetype="dashed") +
      geom_text(size=2.5, hjust=1, vjust=-0.5,aes(x=-4.2, y=-log10(0.05)), label="P-value=0.05",colour="#990000")
    print(p)
    
    if(exportFormat == "emf"){
      savePlot(filename=paste(outputFigsPrefix,figsuffix,time.point,".emf",sep=""),type="emf")
    }
    dev.off()
    
    # 2 - value-ordered - log ratio
    levellog("Making value-ordered plot ...")
    figsuffix<-paste("_",ratio_i_str,"-value-ordered-log-ratio","_",sep="")
    
    if(exportFormat == "pdf"){
      pdf(file=paste(outputFigsPrefix,figsuffix,time.point,".pdf",sep=""),width=10, height=7, family = "Helvetica", pointsize=8)
    }
    
    results<-results[with(results, order(results[,c(ratio_i_avg_col)])),]
    results$nID<-1:nrow(results)
    ratio_i_avg_col_ymax<-paste(ratio_i_avg_col,".ymax",sep="")
    ratio_i_avg_col_ymin<-paste(ratio_i_avg_col,".ymin",sep="")
    results[,ratio_i_avg_col_ymax]<-results[,ratio_i_avg_col]+results[,ratio_i_sd_col]
    results[,ratio_i_avg_col_ymin]<-results[,ratio_i_avg_col]-results[,ratio_i_sd_col]
    
    p<-ggplot(data=results, aes_string(x="nID", y=ratio_i_avg_col, colour=diffexp_ratio_i)) +
      geom_point(alpha=0.7, size=1.5) +
      geom_errorbar(aes_string(ymin=ratio_i_avg_col_ymin, ymax=ratio_i_avg_col_ymax), width=1.5) +
      theme(legend.position = "none", axis.title.y=element_text(vjust=0.2), axis.title.x=element_text(vjust=0), plot.title = element_text(vjust=1.5, lineheight=.8, face="bold")) +
      ylim(c(-ratiolim, ratiolim)) + scale_colour_manual(values=cbPalette) +
      xlab(paste(quantitated_items_lbl,"ID")) + ylab(paste("average log2 ",sub("\\.","/",ratio_i_str),sep="")) + ggtitle("Value-ordered fold change")
    print(p)
    
    if(exportFormat == "emf"){
      savePlot(filename=paste(outputFigsPrefix,figsuffix,time.point,".emf",sep=""),type="emf")
    }
    dev.off()    
    
    # 3 - MA plot
    levellog("Making MA plot ...")
    figsuffix<-paste("_",ratio_i_str,"-MA","_",sep="")
    ratio_i_avgI_col<-paste("log2.avg.I.",ratio_i_str,sep="")
    
    if(exportFormat == "pdf"){
      pdf(file=paste(outputFigsPrefix,figsuffix,time.point,".pdf",sep=""),width=10, height=7, family = "Helvetica", pointsize=8)
    }
    
    p<-ggplot(data=results, aes_string(x=ratio_i_avgI_col, y=ratio_i_avg_col, colour=diffexp_ratio_i)) +
      geom_point(alpha=0.7, size=1.75) +
      theme(legend.position = "none", axis.title.y=element_text(vjust=0.2), axis.title.x=element_text(vjust=0), plot.title = element_text(vjust=1.5, lineheight=.8, face="bold")) +
      ylim(c(-ratiolim, ratiolim)) + scale_colour_manual(values=cbPalette) +
      xlab("M (average log2 Intensity)") + ylab(paste("A (average log2 ",sub("\\.","/",ratio_i_str),")",sep="")) + ggtitle("MA plot")
    print(p)
    
    if(exportFormat == "emf"){
      savePlot(filename=paste(outputFigsPrefix,figsuffix,time.point,".emf",sep=""),type="emf")
    }
    dev.off()
    
    # 4 - reproducibility plots & histograms
    levellog("Making reproducibility plot ...")
    figsuffix<-paste("_",ratio_i_str,"-reproducibility","_",sep="")
    
    allratios<-results[,colnames(results)[grep(ratio_i_,colnames(results))]]
    colnames(allratios)<-sub(ratio_i_,paste("log2(",sub("\\.","/",ratio_i_str),") ",sep=""),colnames(allratios))
    
    if(exportFormat == "pdf"){
      pdf(file=paste(outputFigsPrefix,figsuffix,time.point,".pdf",sep=""),width=10, height=7, family = "Helvetica", pointsize=8)
    }
    
    pairs.panels(allratios,scale=T,lm=T)
    
    if(exportFormat == "emf"){
      savePlot(filename=paste(outputFigsPrefix,figsuffix,time.point,".emf",sep=""),type="emf")
    }
    dev.off()
    levellog("",change=-1)
  }
  
  levellog("Saving plots results to files ...")
  colnames(results)<-gsub("[\\._]"," ",colnames(results))
  for(i in 1:nrow(ratio_combs)){
    ratio_i_str<-paste("(",conditions.labels[ratio_combs[i,2]],") (",conditions.labels[ratio_combs[i,1]],")",sep="")
    colnames(results)<-gsub(ratio_i_str,"\\1/\\2",colnames(results))
  }
  colnames(results)<-gsub("p value adj","P-value adjusted",colnames(results))
  colnames(results)<-gsub("p value","P-value",colnames(results))
  colnames(results)<-gsub("mlog10","-log10",colnames(results))
  colnames(results)<-gsub("log2 avg","avg log2",colnames(results))
  colnames(results)<-gsub("log2 sd","sd log2",colnames(results))
  colnames(results)<-gsub("log2 N","N log2",colnames(results))
  colnames(results)<-gsub("ymax","+sd",colnames(results))
  colnames(results)<-gsub("ymin","-sd",colnames(results))
  
  results<-results[,which(!grepl("^-log10",colnames(results)))]
  results<-results[,which(!grepl("sd$",colnames(results)))]
  results<-results[,which(!grepl("^nID$",colnames(results)))]
  results<-results[,which(!grepl("^diffexp",colnames(results)))]
  
  #write.table(results,file=paste(outputFigsPrefix,"_results_",time.point,".txt",sep=""),sep="\t",col.names=NA)
  
  quant_species<-"proteins"
  if(!ProteinQuantitation){
    quant_species<-"peptides"
  }  
  
  levellog(paste("do_results_plots: Quantified ",quant_species,": ",nrow(results)," (",time.point,")",sep=""))
  
  for(i in 1:nrow(ratio_combs)){
    col_desc_<-paste("P-value adjusted ",paste(conditions.labels[ratio_combs[i,2]],"/",conditions.labels[ratio_combs[i,1]],sep=""),sep="")
    ndiffexp_tmp<-length(which(results[,col_desc_]<0.05))
    levellog(paste("do_results_plots: Differentially expressed for ",conditions.labels[ratio_combs[i,2]]," vs ",conditions.labels[ratio_combs[i,1]]," : ",ndiffexp_tmp,sep=""))
  }
  if(nrow(ratio_combs) > 1){
    levellog(paste("do_results_plots: Differentially expressed in at least one combination of conditions: ",ndiffexp,sep=""))
  }
  
  diffexp<-results[,c(grep("^P-value adjusted",colnames(results)),grep("avg log2 [^I]+",colnames(results)),grep("sd log2 ",colnames(results)),grep("N log2 ",colnames(results)),grep("avg log2 I ",colnames(results)))]
  diffexp[,quantitated_items_lbl]<-rownames(diffexp)
  
  diffexp<-diffexp[,c(grep(quantitated_items_lbl,colnames(diffexp)),grep("avg log2 [^I]+",colnames(diffexp)),grep("log2 sd ",colnames(diffexp)),grep("P-value ",colnames(diffexp)), grep("log2 N ",colnames(diffexp)),grep("log2 avg I ",colnames(diffexp)))]
  tmp_protein_groups<-protein_groups
  colnames(tmp_protein_groups)[grep(paste(quantitated_items_lbl,".IDs",sep=""),colnames(tmp_protein_groups))]<-quantitated_items_lbl
  diffexp<-merge(diffexp,tmp_protein_groups[,c(quantitated_items_lbl,sort(colnames(tmp_protein_groups)[grep("Ratio\\.counts",colnames(tmp_protein_groups))]))],by=quantitated_items_lbl,all.x=T)
  diffexp$newcol<-rowSums(diffexp[,colnames(diffexp)[grep("Ratio\\.counts",colnames(diffexp))]],na.rm=T)
  colnames(diffexp)[length(colnames(diffexp))]<-"Ratio.counts.total"  
  
  signTruth<-rep(FALSE,nrow(diffexp))
  col_desc_<-grep("P-value ",colnames(diffexp))
  for(cond_i_col in col_desc_){
    na_indexes<-which(is.na(diffexp[,cond_i_col]))
    if(length(na_indexes)>0){
      diffexp[na_indexes,cond_i_col]<-1
      signTruth<-(signTruth | diffexp[,cond_i_col]<0.05)
      diffexp[na_indexes,cond_i_col]<-NA
    }else{
      signTruth<-(signTruth | diffexp[,cond_i_col]<0.05)
    }    
  }
  
  colnames(diffexp)<-gsub("\\."," ",colnames(diffexp))
  
  if(!ProteinQuantitation){
    diffexp<-merge(diffexp,protein_groups[c("Peptide.IDs","Protein.IDs")],by.x=c("Peptide"),by.y=c("Peptide.IDs"),all.x=T)
    colnames(diffexp)[grep("^Protein\\.IDs$",colnames(diffexp))]<-"Protein"
  }
  dec <- "."
  write.table(diffexp[signTruth,],dec=dec,file=paste(outputFigsPrefix,"_diffexp_",time.point,".txt",sep=""),sep="\t",row.names=F,quote=F)
  
  diffexp<-merge(diffexp,results[,-grep("^(avg log2|P-value adjusted)",colnames(results))],by.x=c(quantitated_items_lbl),by.y=c("ID"),all.x=T)
  write.table(diffexp,dec=dec,file=paste(outputFigsPrefix,"_results_",time.point,".txt",sep=""),sep="\t",row.names=F,quote=F)
  
  levellog("",change=-1)
  return(results)
}

# Performs the differential expression analysis through limma, after quantile normalization.
do_limma_analysis<-function(working_pgroups,time.point,exp_design_fname,exportFormat="pdf",outputFigsPrefix=""){
  levellog("",change=1)
  
  levellog("Preparing limma input data frame ...")
  # Read the sample key
  # Assigns sample names (from the data file) to groups
  # Sample order must be the same as the main data file, but excludes technical
  # replicates as we will aggregate into one value per sample.
  sample.key <- read.delim(exp_design_fname, header=TRUE,row.names=1,colClasses="character")
  sample.key$Category <- factor(sample.key$Category, levels=unique(sample.key$Category))
  
  # Extract protein/peptide quantitation columns only from quantitation input file
  # The quantitation data is in columns 10 to 90.
  
  #prot.intensities <- quantitation[,10:90]
  prot.intensities <- working_pgroups
  
  # Extract the protein names/peptide sequences (imported from the data file) into a
  # separate list for future reference
  
  prot.names <- rownames(prot.intensities)
  
  # Take log2 of intensities
  
  log.intensities  <- log2(prot.intensities)
  
  setwd(limma_output)
  
  levellog("Saving limma input frame ...")
  write.table(working_pgroups,file=paste(outputFigsPrefix,"_limma-input_",quantitated_items_lbl,"Groups.txt",sep=""),sep="\t",row.names = T, col.names=NA)
  
  if(exportFormat == "pdf"){
    pdf(file=paste(outputFigsPrefix,"_limma-graphs_",time.point,".pdf",sep=""),width=10, height=7, family = "Helvetica", pointsize=8)
  }
  # Box plot before normalisation
  boxplot(log.intensities)
  title(main="Intensities Before Normalisation")
  
  # Perform quantile normalisation
  levellog("Performing quantile normalisation ...")
  norm.intensities <- normalizeBetweenArrays(data.matrix(log.intensities), method="quantile");
  
  # Box plot after normalisation
  boxplot(norm.intensities)
  title(main="Intensities After Normalisation")
  
  norm.median.intensities<-as.data.frame(t(as.matrix(norm.intensities)))
  
  # Assign row names to our aggregated intensities from the sample key
  
  row.names(norm.median.intensities) <- row.names(sample.key)
  
  blocking_var<-c()
  if(.GlobalEnv[["n_techreps"]] > 1){
    for(i in unique(rep_structure$biorep)){
      blocking_var<-c(blocking_var, rep(i,length(rep_structure[rep_structure$biorep == i,]$techrep)))
    }
    blocking_var<-rep(blocking_var, nConditions)
  }else{
    blocking_var<-1:nrow(sample.key)
  }
  
  # Setup design matrix
  # This specifies the design of the experiment for limma, replicating
  # the info in the sample key, but representing it in a matrix format
  levellog("Constructing the design matrix ...")
  design <- model.matrix(~0 + factor(sample.key$Category))
  colnames(design) <- levels(sample.key$Category)
  write.table(design,file=paste(outputFigsPrefix,"_limma-design-matrix_",quantitated_items_lbl,"Groups.txt",sep=""),sep="\t",row.names = T, col.names=NA)
  write.table(blocking_var,file=paste(outputFigsPrefix,"_limma-blocking-variable_",quantitated_items_lbl,"Groups.txt",sep=""),sep="\t",row.names = T, col.names=NA)
  fit<-""
  
  if(.GlobalEnv[["LabelFree"]]){
    fitMethod <- 'robust'
  }else{
    fitMethod <- 'ls'
  }
  
  levellog("Fitting the model ...")
  if(.GlobalEnv[["n_bioreps"]] > 1 & .GlobalEnv[["n_techreps"]] > 1){
    # technical replication specification
    corfit <- duplicateCorrelation(t(norm.median.intensities), design=design, block = blocking_var, trim = duplicateCorrelation_trim)
    # Fit the limma model to the data
    # Pass the protein names/peptide sequences to limma as the genes option
    suppressWarnings(fit <- lmFit(t(norm.median.intensities), design, genes=prot.names, block = blocking_var, cor = corfit$consensus, method=fitMethod))
  }else{
    suppressWarnings(fit <- lmFit(t(norm.median.intensities), design, genes=prot.names, method=fitMethod))
  }
  
  # Setup contrast matrix
  # The contrast matrix specifies what comparisons we want to make between groups.
  # We pass the design matrix as the levels option to the makeContrasts function.
  
  levellog("Constructing the contrast matrix ...")
  ratio_combs<-combinations(nConditions,2,1:nConditions)
  contrasts<-c()
  for(i in 1:nrow(ratio_combs)){
    contrasts<-c(contrasts,paste(conditions.labels[ratio_combs[i,2]],"-",conditions.labels[ratio_combs[i,1]],sep=""))
  }
  contrasts <- makeContrasts(contrasts=contrasts, levels=design)
  write.table(contrasts,file=paste(outputFigsPrefix,"_limma-contrasts-matrix_",quantitated_items_lbl,"Groups.txt",sep=""),sep="\t",row.names = T, col.names=NA)  
  # Apply contrast matrix and do empirical bayes analysis to get p-values etc.
  
  levellog("Performing hypothesis testing ...")
  fit2 <- contrasts.fit(fit, contrasts)
  fit2 <- eBayes(fit2)
  
  # Make volcano plot
  
  #volcanoplot(fit2, highlight="20")
  #title(main="Log odds vs fold change STIMULATED-CTRL", sub="Top 20 most significant proteins highlighted")
  
  # Plot a Histogram of co-efficients (log2 ratio)
  for(i in 1:nrow(ratio_combs)){
    ratio_i_str<-paste(conditions.labels[ratio_combs[i,2]],"/",conditions.labels[ratio_combs[i,1]],sep="")
    hist(fit2$coefficients[,i],main=paste("Log2 Fold Change ",ratio_i_str,sep=""), xlab="Log2 Fold Change", breaks=50 )
  }   
  
  
  if(exportFormat == "emf"){
    savePlot(filename=paste(outputFigsPrefix,"_limma-graphs_",time.point,"_hist.emf",sep=""),type="emf")
  }
  
  # Do an MA Plot (mean of log2 intensities vs log2 ratio)
  # Important to inspect the MA plot to ensures that the ratio does not depend on
  # the intensity of the protein/peptide. This shouldn't happen if the data was normalised
  # successfully, and should be unusual in SILAC / label-free experiments.
  #for(i in 1:nrow(ratio_combs)){
  #  plotMA(fit2)
  
  # Add a line at y=0 to the MA plot. The clouds of points should be centred around y=0
  # if most proteins/peptides are unchanges and normalisation worked well.
  
  #  abline(h=0)
  
  #  if(exportFormat == "emf"){
  #    savePlot(filename=paste(outputFigsPrefix,"_limma-graphs_",time.point,"_MA.emf",sep=""),type="emf")
  #  }
  #}
  
  dev.off()
  
  # Output analysis details to file
  # asjust="BH" means adjust the calculated p-values for multiple testing using
  # the Benjamini Hochberg method (FDR)
  levellog("Saving analysis results to file ...")
  write.fit(fit2, file=paste(outputFigsPrefix,"_condition-i_vs_condition-j_",time.point,".txt",sep=""), adjust="BH")
  
  norm.median.intensities<<-norm.median.intensities #for debugging purposes
  
  levellog("Generating analysis plots ...")
  results<-do_results_plots(norm.median.intensities, time.point, exportFormat=exportFormat,outputFigsPrefix=outputFigsPrefix)
  setwd("..")
  
  levellog("",change=-1)
  return(results)
}

read.pgroups_v3<-function(fname,evidence_fname,time.point,keepEvidenceIDs=F){
  levellog("",change=1)
  levellog("Reading data file ...");
  evidence<-read.table(evidence_fname, header = T, sep = "\t",quote="",stringsAsFactors=F,comment.char = "")
  if(PDdata){ pgroups_colname<-'Protein.Group.Accessions' }else{ pgroups_colname<-'^Proteins$' }
  colnames(evidence)[grepl(pgroups_colname,colnames(evidence))]<-'Protein.IDs'
  if(!PDdata){
    ## For MaxQuant correct protein groups in the evidence file using the protein groups file.
    pgroups<-read.table(fname, header = T, sep = "\t",quote="",stringsAsFactors=F,comment.char = "")
    # If there isn't a Protein.Names column (depends on MQ version), create one from the Fasta Headers column
    col_Protein.names <- length(grep('Protein.Names',colnames(pgroups))) > 0
    if(! col_Protein.names){
      pgroups$Protein.names <- str_match(pgroups$Fasta.headers, '>[:alnum:]+[^[:alnum:]]+([^;>]+)')[,2]
    }
    # Construct a table, mapping the correct protein groups IDs (and the corresponding proteins names) to the evidence IDs
    tmp.table.1<-data.table(do.call(rbind, apply(pgroups[,c('Protein.IDs','Protein.names','Evidence.IDs')], 1, function(x){return(cbind(x['Protein.IDs'], x['Protein.names'], unlist(strsplit(x['Evidence.IDs'], ';'))))})))
    setnames(tmp.table.1, colnames(tmp.table.1), c('Protein.IDs', 'Protein.Names', 'id'))
    class(tmp.table.1$id)<-'integer'
    setkey(tmp.table.1, id)
    # Get the evidence records
    tmp.table.2<-data.table(evidence)
    setkey(tmp.table.2, id)
    # Remove the incorrect Protein.IDs column
    tmp.table.2[, c('Protein.IDs', 'Protein.Names') := NULL]
    # Inner join the mapping table with the evidence table and return the data frame that we ought to have in the first place
    evidence<-data.frame(tmp.table.1[tmp.table.2])
    evidence<-evidence[evidence$Reverse == '' & evidence$Contaminant == '', ]
  }
  
  levellog(paste0("read.pgroups_v3: Identified proteins: ",length(unique(evidence$Protein.IDs))," (",time.point,")"))
  
  n1<-nrow(evidence)
  evidence<-evidence[nchar(evidence$Protein.IDs) > 0,]
  levellog(paste0("read.pgroups_v3: Discarded PSM records due to unassigned protein group: ",(n1-nrow(evidence))))
  ## Make Protein.IDs human-readable
  if(PDdata){ pgroups_colname<-'Protein.Descriptions' }else{ pgroups_colname<-'Protein.Names' }
  tmp.table<-data.table(cbind(evidence[, c('Protein.IDs', pgroups_colname)], i=1:nrow(evidence)))
  setkey(tmp.table, Protein.IDs)
  # Generate data.table with unique Protein.IDs
  tmp.table2<-tmp.table[,.(n=.N),by=Protein.IDs]
  setkey(tmp.table2, Protein.IDs)
  # Make a new protein description column in other data.table
  tmp.table[, pdesc := paste0(Protein.IDs, ' [',strtrim(get(pgroups_colname), 50), ' ...]')]
  # set the Protein.IDs in the original data frame
  evidence$Protein.IDs<-tmp.table2[tmp.table][order(i),pdesc]
  ## Assign defined labels (conditions), one for each PSM record
  levellog("read.pgroups_v3: Assigning labels ...")
  levellog("",change=1)
  if(PDdata){ rawfile_col<-'Spectrum.File' }else{ rawfile_col<-'Raw.file' }
  if(LabelFree){
    cond_spec_col<-rawfile_col
  }else{
    if(PDdata){ cond_spec_col<-'Modifications' }else{ cond_spec_col<-'Labeling.State' }
  }
  evidence$label_<-NA
  background_species_lbl<-NA
  for(i in 1:length(conditions.labels)){
    if(PDdata){
      for(cond_i_spec in conditions.labels.Modifications[[i]]){
        if(nchar(cond_i_spec) > 0){
          mi<-which(grepl(cond_i_spec, evidence[, cond_spec_col]))
          evidence[mi,]$label_<-conditions.labels[i]
        }else{
          # if the label specification is the empty string
          # it means that we have background / unlabelled species that we need treat as 'labeled'.
          # In such case, we shouldn't remove any NA-assigned label_ record after this loop.
          # So set a flag in order to set the label for the NA records after the loop.
          background_species_lbl<-conditions.labels[i]
        }
      }
    }else{
      if(LabelFree){
        for(cond_i_spec in conditions.labels.Modifications[[i]]){
          mi<-which(grepl(cond_i_spec, evidence[, cond_spec_col]))
          evidence[mi,]$label_<-conditions.labels[i]
        }        
      }else{
        # MQ nomenclature for labels: 0 the first label, 1 the second etc ...
        mi<-which(grepl((i-1), evidence[, cond_spec_col]))
        evidence[mi,]$label_<-conditions.labels[i]
      }
    }
    levellog(paste0("read.pgroups_v3: Assigned label '", conditions.labels[i],"'."))
  }
  levellog("",change=-1)
  mi<-which(is.na(evidence$label_))
  if(is.na(background_species_lbl)){
    if(length(mi) > 0){
      evidence<-evidence[-mi,]
      levellog(paste("read.pgroups_v3: Discarded PSM records due to unassigned label: ",length(mi),sep=""))
    }
  }else{
    evidence[mi,]$label_<-background_species_lbl
  }
  # Now add the experimental structure information
  evidence<-merge(evidence, .GlobalEnv[["rep_structure"]], by.x=c(rawfile_col), by.y=c('raw_file'))
  
  ## If we have fractionation, remake the rep_desc column and don't take into account the fraction number
  if(length(unique(.GlobalEnv[["rep_structure"]]$fraction)) > 1){
    evidence$rep_desc <- paste0('b',evidence$biorep,'t',evidence$techrep)
  }
  
  ## Generate Venn data for the identified proteins and output to a file
  levellog("read.pgroups_v3: Generating ID Venn data ...")
  tmp.table<-data.table(evidence[, c('Protein.IDs', 'biorep', 'techrep', 'fraction')])
  setkey(tmp.table,  Protein.IDs, biorep, techrep, fraction)
  setwd(limma_output)
  write.table(tmp.table[, .(n=.N), by=.(Protein.IDs,rep=biorep)][,.(Protein.IDs,rep)],file=paste0(outputFigsPrefix,"_id_venn3-data_",time.point,".txt"),sep="\t",row.names=F)
  setwd("..")    
  
  # Bring Labeled or Label-free data to the following common format (table headers):
  # rep_desc Protein.IDs UniqueSequences.Intensity.condition_1 ... UniqueSequences.Intensity.condition_N Intensity.condition_1 ... Intensity.condition_N
  
  levellog("read.pgroups_v3: Standarizing data format ...")
  if(!PDdata){
    colnames(evidence)[grepl('Peptide.ID',colnames(evidence))]<-'Unique.Sequence.ID'
    colnames(evidence)[grepl('Intensity\\..+',colnames(evidence))]<-conditions.labels
    
  }
  if(LabelFree){
    if(PDdata){
      # Precursor Area is unfortunately buggy (sometimes 0/NA), so we are left with Intensity to work with
      #intensityCol <- 'Precursor.Area'
      intensityCol <- 'Intensity'
    }else{
      intensityCol <- 'Intensity'
    }
    evidence.dt<-data.table(evidence[, c('Protein.IDs', 'Unique.Sequence.ID', intensityCol,'label_', 'rep_desc')])
    setkey(evidence.dt, rep_desc, Protein.IDs, Unique.Sequence.ID, label_)
    # Get maximum PSM intensity per peptide/protein/[(rep_desc/label) = raw_file]
    suppressWarnings(evidence.dt<-evidence.dt[, .(maxI=max(get(intensityCol), na.rm = T)), by=.(rep_desc, Protein.IDs, Unique.Sequence.ID, label_)][maxI != -Inf])
  }else{
    if(PDdata){
      evidence.dt<-data.table(evidence[, c('Quan.Usage','Protein.IDs', 'Unique.Sequence.ID', conditions.labels,'rep_desc', 'label_')])
    }else{
      evidence.dt<-data.table(evidence[, c('Protein.IDs', 'Unique.Sequence.ID', conditions.labels,'rep_desc', 'label_')])
    }
    setkey(evidence.dt, rep_desc, Protein.IDs, Unique.Sequence.ID)    
  }
  ## Calculate identified peptide counts per protein for each condition/label and replicate in the following three steps
  # 1. For each condition (per sequnce, protein and replicate), set a corresponding column to TRUE if there are > 0 evidence.dt (PSMs) records, FALSE otherwise
  evidence.dt.seqCounts<-dcast.data.table(evidence.dt[, .(n=.N > 0), by=.(rep_desc, Protein.IDs, Unique.Sequence.ID, label_)], rep_desc + Protein.IDs + Unique.Sequence.ID ~ label_, fill=FALSE)
  # 2. Add a column flagging the common, between conditions/labels, sequences.
  # In case of more than two conditions/labels, the flag designates that there are at least two conditions/labels where the peptide is common
  evidence.dt.seqCounts[, 'common' := rowSums(.SD) > 1,.SDcols=conditions.labels]    
  # 3. Collapse the records for each protein (per replicate) and count the TRUEs.
  # evidence.dt[, .(n.Unique.Sequence.IDs=.N), by=.(rep_desc, Protein.IDs)]
  evidence.dt.seqCounts<-evidence.dt.seqCounts[,c(n.Unique.Sequence.IDs=.N,lapply(.SD, function(x){return(length(which(x)))})), by=.(rep_desc,Protein.IDs),.SDcols=c(conditions.labels, 'common')]
  # 4. Calculate the percentage columns
  evidence.dt.seqCounts[, paste0(conditions.labels,'p') := lapply(.SD, function(x){return((x/sum(.SD))*100)}), by=.(rep_desc,Protein.IDs),.SDcols=c(conditions.labels)]
  ## Rename the peptide counts columns
  setnames(evidence.dt.seqCounts,colnames(evidence.dt.seqCounts)[which(colnames(evidence.dt.seqCounts) %in% conditions.labels)],paste('UniqueSequences',conditions.labels,sep='.'))    
  ## Calculate the protein intensity = (sum of unique peptide intensities) for each condition/label and replicate in the following two steps
  if(LabelFree){
    # 1. Cast the data so that we have columns for each label and intensity separately
    evidence.dt<-dcast.data.table(evidence.dt, rep_desc + Protein.IDs + Unique.Sequence.ID ~ label_, fill=0)    
  }else{
    if(PDdata){
      # 1. Take the (Quan.Usage == 'Used') records and for each peptide keep only the PSM record with the highest intensity
      evidence.dt<-evidence.dt[Quan.Usage == 'Used', lapply(.SD, max), by=.(rep_desc, Protein.IDs, Unique.Sequence.ID), .SDcols=conditions.labels]    
    }else{
      # 1. Take the records with Intensity != NA across labels/conditions and for each peptide keep only the PSM record with the highest intensity
      evidence.dt[, sumI := rowSums(.SD, na.rm = T), .SDcols=conditions.labels]
      evidence.dt<-evidence.dt[sumI > 0, lapply(.SD, max), by=.(rep_desc, Protein.IDs, Unique.Sequence.ID), .SDcols=conditions.labels]    
      evidence.dt[, sumI := NULL]
    }
  }
  
  # Get a vector of unique peptides intensities
  tmp.I<-sort(unique(evidence.dt[,get(conditions.labels)]))
  # If the minimum intensity is zero
  if(tmp.I[1] == 0){
    # Replace 0's with minimum intensity (PD can do this automatically for us)
    minI<-tmp.I[2]
    evidence.dt[, (conditions.labels) := lapply(.SD, function(x){ t<-which(x == 0); if(length(t) > 0){x[t] <- minI}; return(x) }), .SDcols=conditions.labels]
  }else{
    minI<-tmp.I[1]
  }
  
  ## If enabled, do filter out peptides where all 'channels' except filterL_lbl channel have noise-level intensity
  if(filterL && filterL_lvl){
    evidence.dt[, minIcount := rowSums(.SD == minI), .SDcols=conditions.labels[! conditions.labels %in% filterL_lbl]]
    n1<-nrow(evidence.dt)
    evidence.dt<-evidence.dt[minIcount < (nConditions - 1)]
    n2<-nrow(evidence.dt)
    if(n2 < n1){
      levellog(paste0("read.pgroups_v3: Filtered out ", (n1-n2)," peptides having noise-level intensity in all channels except the '", filterL_lbl,"' channel ..."));
    }
    evidence.dt[, minIcount := NULL]
  }
  
  # 2. Calculate the protein intensity (= sum of unique peptide intensities) for each condition/label and replicate
  # Also count the number of quantifiable peptides (those which do not have intensity NA)
  if(LabelFree){
    # Top three in abundance
    #evidence.dt<-evidence.dt[, lapply(.SD, function(x){x<-x[!is.na(x)]; x<-sort(x, decreasing<-T); if(length(x)<3){return(sum(x))}else{return(sum(x[1:3]))}}), by=.(rep_desc, Protein.IDs), .SDcols=conditions.labels]
    evidence.dt<-evidence.dt[, c(n=.N, nas=length(which(is.na(.SD))) ,lapply(.SD, function(x){x<-x[!is.na(x)]; x<-sort(x, decreasing<-T); if(length(x)<3){return(sum(x))}else{return(sum(x[1:3]))}})), by=.(rep_desc, Protein.IDs), .SDcols=conditions.labels]
  }else{
    # All peptides
    evidence.dt<-evidence.dt[, c(n=.N, nas=length(which(is.na(.SD))) ,lapply(.SD, sum, na.rm = T)), by=.(rep_desc, Protein.IDs), .SDcols=conditions.labels] 
  }
  ## Rename the intensity columns
  setnames(evidence.dt,colnames(evidence.dt)[which(colnames(evidence.dt) %in% conditions.labels)],paste('Intensity',conditions.labels,sep='.'))
  ## Merge with the evidence.dt.seqCounts table
  evidence.dt<-merge(evidence.dt, evidence.dt.seqCounts)
  
  # Add the experimental structure information to evidence.dt based on rep_desc (raw file at this point has no information and is dropped)
  tmp.rep_struct<-.GlobalEnv[["rep_structure"]][! duplicated(.GlobalEnv[["rep_structure"]][,c('biorep','techrep')]), !grepl('raw_file', colnames(.GlobalEnv[["rep_structure"]])) & !grepl('fraction', colnames(.GlobalEnv[["rep_structure"]]) )]
  tmp.rep_struct$rep_desc<-paste0('b',tmp.rep_struct$biorep,'t',tmp.rep_struct$techrep)
  evidence.dt<-merge(evidence.dt ,data.table(tmp.rep_struct), by='rep_desc')
  
  ## If enabled, do filter out proteins based on percentage labeling for the desired label
  if(filterL && !filterL_lvl){
    n1<-length(unique(evidence.dt[get(paste0(filterL_lbl,"p")) == 100.0]$Protein.IDs))
    evidence.dt<-evidence.dt[get(paste0(filterL_lbl,"p")) < 100.0]
    levellog(paste0("read.pgroups_v3: Filtered out ", n1," proteins which where identified solely by '", filterL_lbl, "' peptides ..."));
  }
  
  ## Get protein IDs that were quantified with a total of at least 'nRequiredLeastBioreps' different peptides accross at least 'nRequiredLeastBioreps' biological replicates.
  # E.g. 1: with 3 biological replicates, a protein that was quantified by a single peptide in 'nRequiredLeastBioreps' out of the 3 replicates will be discarded if 'nRequiredLeastBioreps' > 1 (retained otherwise).
  # E.g. 2: with 3 biological replicates, a protein that was quantified by a single peptide in 1 out of the 3 replicates will be discarded if 'nRequiredLeastBioreps' > 1 (retained otherwise).
  # E.g. 3: with 3 biological replicates, a protein that was quantified by two peptides in at one of replicates will be discarded if 'nRequiredLeastBioreps' > 1 (retained otherwise).
  # E.g. 4: with 3 biological replicates, a protein that was quantified by two peptides (in total) in 2 out of the 3 replicates will be discarded if 'nRequiredLeastBioreps' > 2 (retained otherwise).
  
  Protein.IDs.quant <- evidence.dt[, .(c1 = sum(N.x-nas)) , by=.(Protein.IDs, biorep)][, .(nQuantPeps = sum(c1), geqXnRequiredLeastBioreps = .N >= .GlobalEnv[["nRequiredLeastBioreps"]]), by=.(Protein.IDs)][nQuantPeps >= .GlobalEnv[["nRequiredLeastBioreps"]] & geqXnRequiredLeastBioreps == T]$Protein.IDs
  levellog(paste0("read.pgroups_v3: Filtered out ", (length(unique(evidence.dt$Protein.IDs)) - length(Protein.IDs.quant))," proteins which were not identified in at least ",nRequiredLeastBioreps," biological replicate(s) with at least a total of ",nRequiredLeastBioreps," peptide(s)"));
  evidence.dt[,nQuantPeps := N.x-nas]
  evidence.dt<-evidence.dt[Protein.IDs %in% Protein.IDs.quant]
  
  ## Experimental filter based on outlier removal (grubbs method) based on the first condition specified.
  ## NOTE: It is applied when there are no technical replicates in Label-free data, where variability is expected to be very high.
  # If a protein intensity in condition i and biological replicate j is found to be an outlier based on the distribution
  # of intensities from all biological replicates, then
  # the biological replicate j is removed for that particular protein for all conditions.
  #if(LabelFree && .GlobalEnv[["n_techreps"]] < 2){
  #  evidence.dt.bad <- suppressWarnings(evidence.dt[, lapply(.SD, function(x){p.val = grubbs.test(x)$p.value; if(!is.na(p.val) && p.val < 0.05){outlier.true <- T}else{outlier.true <- F}; if(outlier.true){return(.I[outlier(x, logical=T)][1] )}else{return(as.integer(0))} }),by=.(Protein.IDs),.SDcols=paste0('Intensity.',conditions.labels[1])][,get(paste0('Intensity.',conditions.labels[1]))])
  #  evidence.dt.bad <- evidence.dt.bad[evidence.dt.bad > 0]
  #  evidence.dt<-evidence.dt[! evidence.dt.bad]
  #  levellog(paste0("read.pgroups_v3: Filtered out ", length(evidence.dt.bad)," protein intensities based on outlier detection on condition '",conditions.labels[1],"'."));
  #  Protein.IDs.quant <- evidence.dt[, .(c1 = sum(n-nas)) , by=.(Protein.IDs, biorep)][, .(nQuantPeps = sum(c1), geqXnRequiredLeastBioreps = .N >= .GlobalEnv[["nRequiredLeastBioreps"]]), by=.(Protein.IDs)][nQuantPeps >= .GlobalEnv[["nRequiredLeastBioreps"]] & geqXnRequiredLeastBioreps == T]$Protein.IDs
  #  levellog(paste0("read.pgroups_v3: Filtered out another ", (length(unique(evidence.dt$Protein.IDs)) - length(Protein.IDs.quant))," proteins which were not identified in at least ",nRequiredLeastBioreps," biological replicate(s) with at least a total of ",nRequiredLeastBioreps," peptide(s)"));
  #  evidence.dt[,nQuantPeps := n-nas]
  #  evidence.dt<-evidence.dt[Protein.IDs %in% Protein.IDs.quant]
  #}
  
  
  
  ## Generate Venn data for the identified proteins and output to a file
  levellog("read.pgroups_v3: Generating quant Venn data ...")
  setwd(limma_output)  
  write.table(evidence.dt[, .(Protein.IDs, rep=biorep)],file=paste0(outputFigsPrefix,"_quant_venn3-data-",.GlobalEnv[["nRequiredLeastBioreps"]],"reps_",time.point,".txt"),sep="\t",row.names=F)
  setwd("..")
  
  ## Cast the table to the following format
  # Protein.IDs Intensity.[<rep_desc_X>.<label/condition_Y> ...] [<rep_desc_X>.Ratio.counts ...] [<rep_desc_X>.uniqueSequences ...] time.point [<label/condition_Y> ...] [<label/condition_Y>p ...]
  
  ## Step 1: For each 'rep_desc', add to a growing dataframe the evidence.dt data, renaming the columns accordingly
  # Also, calculate the missing columns required by the target format and drop the unnecessary columns
  setkey(evidence.dt, Protein.IDs)
  pgroups<-data.frame(Protein.IDs = unique(evidence.dt)$Protein.IDs)
  setkey(evidence.dt, rep_desc)
  for(rep_desc_i in unique(evidence.dt)$rep_desc){
    rep_desc_i_pgroups<-data.frame(evidence.dt[rep_desc == rep_desc_i,])
    allcols<-colnames(rep_desc_i_pgroups)
    # Rename Intensity cols
    colsl<-grepl('^Intensity' ,allcols)
    colnames(rep_desc_i_pgroups)[colsl]<-gsub("^Intensity(.+)$",paste("Intensity\\1",rep_desc_i,sep='.'), allcols[colsl])
    # Rename UniqueSequences cols
    colsl<-grepl('^UniqueSequences' ,allcols)
    colnames(rep_desc_i_pgroups)[colsl]<-gsub("^UniqueSequences(.+)$",paste(rep_desc_i,"uniqueSequences\\1",sep='.'), allcols[colsl])
    # Add new column <rep_desc_X>.uniqueSequences
    rep_desc_i_pgroups[, paste(rep_desc_i,'uniqueSequences',sep='.')]<-rowSums(rep_desc_i_pgroups[, colnames(rep_desc_i_pgroups)[colsl]])
    # Rename 'p' (percentage) cols
    colsl<-allcols %in% paste0(conditions.labels,'p')
    colnames(rep_desc_i_pgroups)[colsl]<-gsub("^(.+)$",paste("\\1",rep_desc_i,sep='.'), allcols[colsl])
    # Rename the 'nQuantPeps' column to <rep_desc_i>.Ratio.counts
    colsl<-allcols %in% c('nQuantPeps')
    colnames(rep_desc_i_pgroups)[colsl]<-paste(rep_desc_i,'Ratio.counts',sep='.')
    # merge with the growing data frame
    cc<-intersect(names(pgroups), names(rep_desc_i_pgroups))
    pgroups<-merge(pgroups, rep_desc_i_pgroups[, ! colnames(rep_desc_i_pgroups) %in% c('biorep', 'techrep', 'fraction', 'rep_desc', cc[! grepl('Protein.IDs', cc)] )], all.x = T)
  }
  # Step 2: Calculate the columns [<label/condition_Y> ...] containing the number of unique sequences found per condition in all replicates
  allcols<-colnames(pgroups)
  for(cond_i in conditions.labels){
    colsl<-grepl(paste('uniqueSequences', cond_i,sep='\\.') ,allcols)
    pgroups[, cond_i]<-rowSums(pgroups[, allcols[colsl]])
  }
  # Step 3: Calculate the columns [<label/condition_Y>p ...] containing the percentage of unique sequences that were found in a specific condition in all replicates
  allcols<-colnames(pgroups)
  for(cond_i in conditions.labels){
    colsl<-allcols %in% conditions.labels & ! allcols %in% cond_i
    pgroups[, paste0(cond_i,'p')]<-(pgroups[, cond_i]/rowSums(pgroups[, c(cond_i, allcols[colsl])]))*100
  }
  # Step 4: Add time-point column
  pgroups$time.point<-time.point
  # Step 5: Remove unnecessary columns (uniqueSequences per rep_desc and percentage unique peptides per rep_desc)
  allcols<-colnames(pgroups)
  pgroups<-pgroups[,-which(grepl('uniqueSequences\\.', allcols) | grepl('p\\.b[0-9]+t[0-9]+$', allcols) | grepl('^common$', allcols))]
  ##
  levellog(paste0("read.pgroups_v3: Quantifiable proteins: ", nrow(pgroups)," (",time.point,")"))
  levellog("",change=-1)
  ## 
  return(pgroups)  
}

#Prepare protein intensity table for differential expression analysis (the format limma requires)
prepare_working_pgroups<-function(working_pgroups){
  rownames(working_pgroups)<-working_pgroups[,paste(quantitated_items_lbl,".IDs",sep="")]
  inten_cols<-c()
  for(cond_i in conditions.labels){
    inten_cols<-c(inten_cols,sort(colnames(working_pgroups)[grep(paste("Intensity.",cond_i,".b",sep=""),colnames(working_pgroups))]))
  }  
  working_pgroups<-working_pgroups[,inten_cols]
  colnames(working_pgroups)<-sub("Intensity\\.","",inten_cols)
  return(working_pgroups)
}

#GLOBAL variables

duplicateCorrelation_trim<-0.15 # use 0.22 for "bad" datasets (too many missing values)

addLabel<-function(lblname, lbl.Modifications){
  levellog("", change=1)
  #If label name is a number some routines won't work, it has to be converted to some acceptable variable name
  lblname<-make.names(lblname)
  labeltxt <- "label";
  if(!LabelFree){
    lbl.Modifications<-gsub("\\(","\\\\(",lbl.Modifications)
    lbl.Modifications<-gsub("\\)","\\\\)",lbl.Modifications)
    unmod_idx<-which(lbl.Modifications == "")
    if(length(unmod_idx) > 0){
      rest_idx<-which(lbl.Modifications[-unmod_idx] != "")
      if(length(rest_idx) > 0){
        lbl.Modifications<-c(lbl.Modifications[unmod_idx],paste(lbl.Modifications[-unmod_idx],"\\)",sep=""))
      }
    }else{
      lbl.Modifications<-paste(lbl.Modifications,"\\)",sep="")
    }
  }else{
    labeltxt <- "condition";
  }
  lblname_i<-which(grepl(paste("^",lblname,"$",sep=""),conditions.labels))
  if(length(lblname_i) != 0){
    levellog(paste("addLabel: Error adding ",labeltxt," '",lblname,"': An existing ",labeltxt," with name '",lblname,"' (specification: ",paste(unlist(conditions.labels.Modifications[lblname_i]),collapse=", "),") already exists. Please try a different name.",sep=""), change=-1)
    return(FALSE)
  }
  
  conditions.labels<<-c(conditions.labels, lblname)
  j<-length(conditions.labels.Modifications)+1
  conditions.labels.Modifications[[j]]<<-lbl.Modifications
  nConditions<<-length(conditions.labels)
  levellog("", change=-1)
}
removeLabel<-function(lblname){
  levellog("", change=1)
  lblname_i<-which(grepl(paste("^",lblname,"$",sep=""),conditions.labels))
  if(length(lblname_i) != 0){
    conditions.labels<<-conditions.labels[-lblname_i]
    conditions.labels.Modifications<<-conditions.labels.Modifications[-lblname_i]
  }
  nConditions<<-length(conditions.labels)
  levellog("", change=-1)
}
clearLabels<-function(){
  levellog("", change=1)
  conditions.labels<<-c()
  conditions.labels.Modifications<<-list()
  nConditions<<-length(conditions.labels)
  levellog("", change=-1)
}

addMod<-function(modname, mod.Modifications){
  levellog("", change=1)
  modname<-make.names(modname)
  mod.Modifications<-gsub("\\(","\\\\(",mod.Modifications)
  mod.Modifications<-gsub("\\)","\\\\)",mod.Modifications)
  unmod_idx<-which(mod.Modifications == "")
  if(length(unmod_idx) > 0){
    rest_idx<-which(mod.Modifications[-unmod_idx] != "")
    if(length(rest_idx) > 0){
      mod.Modifications<-c(mod.Modifications[unmod_idx],paste(mod.Modifications[-unmod_idx],"\\)",sep=""))
    }
  }else{
    mod.Modifications<-paste(mod.Modifications,"\\)",sep="")
  }
  modname_i<-which(grepl(paste("^",modname,"$",sep=""),conditions.Mods))
  if(length(modname_i) != 0){
    levellog(paste("addMod: Error adding modification '",modname,"': An existing modification with name '",modname,"' (specification: ",paste(unlist(conditions.Mods.Modifications[modname_i]),collapse=", "),") already exists. Please try a different name.",sep=""), change=-1)
    return(FALSE)
  }
  i<-1
  for(mod_i in conditions.Mods.Modifications){
    for(mod_i in mod.Modifications){
      mod_i_matches<-which(grepl(paste("^",mod_i,"$",sep=""),mod_i))
      if(length(mod_i_matches) != 0){
        levellog(paste("addMod: Error adding modification '",modname,"': Existing modification with name '",conditions.Mods[i],"' has an identical specification (",mod_i,").",sep=""),change=-1)
        return(FALSE)
      }
    }
    i<-i+1
  }
  conditions.Mods<<-c(conditions.Mods, modname)
  j<-length(conditions.Mods.Modifications)+1
  conditions.Mods.Modifications[[j]]<<-mod.Modifications
  nMods<<-length(conditions.Mods)
  levellog("", change=-1)
}
removeMod<-function(modname){
  levellog("", change=1)
  modname_i<-which(grepl(paste("^",modname,"$",sep=""),conditions.Mods))
  if(length(modname_i) != 0){
    conditions.Mods<<-conditions.Mods[-modname_i]
    conditions.Mods.Modifications<<-conditions.Mods.Modifications[-modname_i]
  }
  nMods<<-length(conditions.Mods)
  levellog("", change=-1)
}
clearMods<-function(){
  levellog("", change=1)
  conditions.Mods<<-c()
  conditions.Mods.Modifications<<-list()
  nMods<<-length(conditions.Mods)
  levellog("", change=-1)
}

unlabeled_peptide_regex<-"^$"
clearLabels()
clearMods()
paramssetfromGUI<-F
working_directory<-getwd()
limma_output<-"msdiffexp_out"
LabelFree<-F
#source("/home/gefstathiou/Documents/ProteoSign/ProteoSign/uploads/L/msdiffexp_wd/MSdiffexp_definitions.R")
#source("/home/gefstathiou/Documents/ProteoSign/ProteoSign/uploads/L2/msdiffexp_wd/MSdiffexp_definitions.R")
#source("/home/gefstathiou/Documents/ProteoSign/ProteoSign/uploads/L2_MQ/msdiffexp_wd/MSdiffexp_definitions.R")
#source("/home/gefstathiou/Documents/ProteoSign/ProteoSign/uploads/LF/msdiffexp_wd/MSdiffexp_definitions.R")
#source("/home/gefstathiou/Documents/ProteoSign/ProteoSign/uploads/LF_MQ/msdiffexp_wd/MSdiffexp_definitions.R")
source("MSdiffexp_definitions.R")

perform_analysis<-function(){
  levellog("",change=1)
  setwd(working_directory)
  rep_structure<-read.table(experimental_structure_file,col.names=c('raw_file','biorep','techrep','fraction'))
  rep_structure<-rep_structure[order(rep_structure[,2],rep_structure[,3],rep_structure[,4]),]
  
  if(length(unique(rep_structure$biorep)) == 1){
    levellog("Error: Cannot accept dataset with just one biological replicate. Aborting ...")
    return(F)
  }
  
  if(length(unique(rep_structure$techrep)) > 1){
    if(length(unique(rep_structure$fraction)) > 1){
      # we have techreps and fractions
      rep_structure$rep_desc<-paste(paste(paste('b',rep_structure$biorep,sep=''),'t',rep_structure$techrep,sep=''),'f',rep_structure$fraction,sep='')
    }else{
      #we have bioreps and techreps
      rep_structure$rep_desc<-paste(paste('b',rep_structure$biorep,sep=''),'t',rep_structure$techrep,sep='')
    }
  }else{
    if(length(unique(rep_structure$fraction)) > 1){
      # we have fractions but not techreps
      rep_structure$rep_desc<-paste(paste(paste('b',rep_structure$biorep,sep=''),'t',rep_structure$techrep,sep=''),'f',rep_structure$fraction,sep='')
    }else{
      # we just have bioreps
      rep_structure$rep_desc<-paste(paste('b',rep_structure$biorep,sep=''),'t',rep_structure$techrep,sep='')
      # it should be like below, but for backward compatibility with other parts of the code, we keep the convention that in the rep. description, we will always have the terms 'b' (i.e. bio-rep) and 't', even if we don't have tech-reps ...
      # rep_structure$rep_desc<-paste('b',rep_structure$biorep,sep='')
    }
  }
  
  .GlobalEnv[["rep_structure"]]<-rep_structure
  .GlobalEnv[["n_bioreps"]]<-max(rep_structure$biorep)
  .GlobalEnv[["n_techreps"]]<-min(ddply(rep_structure[,c("biorep","techrep")],c("biorep"),function(x){return(max(x$techrep))})$V1)
  
  if(ProteinQuantitation){
    quantitated_items_lbl<<-"Protein"
  }else{
    quantitated_items_lbl<<-"Peptide"
  }
  if(file.exists(limma_output)){
    unlink(limma_output, recursive=T, force=T)
  }
  dir.create(limma_output)
  if(grepl("\"",readLines(evidence_fname, n=1))){
    levellog("Removing double quotes from input data file #1 ...")
    tmpdata<-gsub("\"", "", readLines(evidence_fname))
    evidence_fname_cleaned<-file(evidence_fname, open="w")
    writeLines(tmpdata, con=evidence_fname_cleaned)
    close(evidence_fname_cleaned)
  }
  levellog("Reading input data ...")
  if(PDdata){
    protein_groups<<-read.pgroups_v3(pgroups_fname,evidence_fname,time.point,keepEvidenceIDs=T)
  }else{
    if(grepl("\"",readLines(pgroups_fname, n=1))){
      levellog("Removing double quotes from input data file #2 ...")
      tmpdata<-gsub("\"", "", readLines(pgroups_fname))
      pgroups_fname_cleaned<-file(pgroups_fname, open="w")
      writeLines(tmpdata, con=pgroups_fname_cleaned)
      close(pgroups_fname_cleaned)
    }
    protein_groups<<-read.pgroups_v3(pgroups_fname,evidence_fname,time.point,keepEvidenceIDs=T)
  }
  
  setwd(limma_output)
  write.table(protein_groups,file=paste(outputFigsPrefix,"_proteinGroupsDF.txt",sep=""),row.names=F,sep="\t")
  setwd("..")
  
  expdesign<-c()
  for(cond_i in conditions.labels){
    expdesign<-rbind(expdesign,cbind(paste(sub("Intensity\\.","",sort(colnames(protein_groups)[grep(paste("Intensity.",cond_i,".b",sep=""),colnames(protein_groups))]))),cond_i))  
  }
  colnames(expdesign)<-c("Sample","Category")
  write.table(expdesign,file="curr_exp_design.txt",row.names=F,quote=F,sep = "\t")
  exp_design_fname<<-"curr_exp_design.txt"
  
  levellog("Performing the analysis ...")
  do_limma_analysis(prepare_working_pgroups(protein_groups),time.point,exp_design_fname,exportFormat="pdf",outputFigsPrefix=outputFigsPrefix)
  
  levellog("Data analysis finished.")
  levellog("",change=-1)
  return(T)
}

#================ PRODUCTION ===============

if(GUI & !paramssetfromGUI){  
}else{
  perform_analysis()
}

