options(warn=1)

# ======================================
# WARNING: Make sure to install the following packages as administrator/root (for them to be available to all users)
# ======================================

#source("http://www.bioconductor.org/biocLite.R")
#if(!require("limma")){ biocLite("limma") }
#if(!require("statmod")){ biocLite("statmod") }
#if(!require("ggplot2")){ install.packages("ggplot2", repos="http://cran.fhcrc.org") }
#if(!require("reshape")){ install.packages("reshape", repos="http://cran.fhcrc.org") }
#if(!require("plyr")){ install.packages("plyr", repos="http://cran.fhcrc.org") }
#if(!require("gtools")){ install.packages("gtools", repos="http://cran.fhcrc.org") }
#if(!require("gtools")){ install.packages("labeling", repos="http://cran.fhcrc.org") }
#if(!require("data.table")){ install.packages("data.table", repos="http://cran.fhcrc.org") }

library(limma)
library(statmod)
library(reshape)
library(plyr)
library(ggplot2)
library(labeling)
library(gtools)
library(data.table)

# DEBUGGING log flag/level (0 translates to no debugging log at all)
debuglog <- 10
# DEBUGGING log indentation level (psoitive int, global) for levellog function
loglvl <- 0
#  Print msg with specific indentation (loglvl*3)
#  - change [int]: change previous indentation level by <change> amount
#  - reset [bool]: reset indentation level to 0 (no indentation)
#  - after [bool]: change indentation after printing msg
levellog <- function (msg, change=0, reset=F,after=F, supression=debuglog){
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
			cat(paste(paste(rep(" ",times=prev_loglvl*3),collapse="")," +-- ",msg," [",Sys.time(),"]\n"))
		}else{
			cat(paste(paste(rep(" ",times=loglvl*3),collapse="")," +-- ",msg," [",Sys.time(),"]\n"))
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

  if(.GlobalEnv[["n_bioreps"]]>1){
    levellog(paste("do_results_plots: Quantified ",quant_species," (>2 peptides/",.GlobalEnv[["n_techreps"]]," injection(s) in at least ",nRequiredLeastBioreps," replicates): ",nrow(results)," (",time.point,")",sep=""))
  }else{
    levellog(paste("do_results_plots: Quantified ",quant_species," (>2 peptides/",.GlobalEnv[["n_techreps"]]," injection(s)): ",nrow(results)," (",time.point,")",sep=""))
  }  
  
  for(i in 1:nrow(ratio_combs)){
	col_desc_<-paste("P-value adjusted ",paste(conditions.labels[ratio_combs[i,2]],"/",conditions.labels[ratio_combs[i,1]],sep=""),sep="")
	ndiffexp_tmp<-length(which(results[,col_desc_]<0.05))
	levellog(paste("do_results_plots: Differentially expressed for ",conditions.labels[ratio_combs[i,2]]," vs ",conditions.labels[ratio_combs[i,1]]," : ",ndiffexp_tmp,sep=""))
  }
  levellog(paste("do_results_plots: Differentially expressed in at least one combination of conditions: ",ndiffexp,sep=""))
  
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
	

  
	levellog("Fitting the model ...")
  if(.GlobalEnv[["n_bioreps"]] > 1 & .GlobalEnv[["n_techreps"]] > 1){
		# technical replication specification
		corfit <- duplicateCorrelation(t(norm.median.intensities), design=design, block = blocking_var, trim = duplicateCorrelation_trim)
		# Fit the limma model to the data
		# Pass the protein names/peptide sequences to limma as the genes option
		fit <- lmFit(t(norm.median.intensities), design, genes=prot.names, block = blocking_var, cor = corfit$consensus)
	}else{
		fit <- lmFit(t(norm.median.intensities), design, genes=prot.names)
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
  if(!PDdata){
    n1<-nrow(evidence)
    evidence<-evidence[nchar(evidence$Reverse) == 0 & nchar(evidence$Contaminant) == 0,]
    levellog(paste0("read.pgroups_v3: Removed ", (n1 - nrow(evidence))," PSMs belonging to contaminants."))
  }
  if(PDdata){ pgroups_colname<-'Protein.Group.Accessions' }else{ pgroups_colname<-'^Proteins$' }
  colnames(evidence)[grepl(pgroups_colname,colnames(evidence))]<-'Protein.IDs'
  if(!PDdata){
    # TODO:
    # For MaxQuant correct protein groups using the protein groups file.
    # Algo:
    # Step 1. Extract the Protein.IDs and Peptide.IDs data from the protein groups file.
    # Step 2. Correct the Protein.IDs accordingly inside the evidence data frame, using the peptide ids.
  }
  
  levellog(paste0("read.pgroups_v3: Identified proteins: ",length(unique(evidence$Protein.IDs))," (",time.point,")"))
  
  n1<-nrow(evidence)
  evidence<-evidence[nchar(evidence$Protein.IDs) > 0,]
  levellog(paste("read.pgroups_v3: Discarded PSM records due to unassigned protein group: ",(n1-nrow(evidence)),sep=""))
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
  if(LabelFree){
    cond_spec_col<-"Spectrum.File"
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
      # MQ nomenclature for labels: 0 the first label, 1 the second etc ...
      mi<-which(grepl((i-1), evidence[, cond_spec_col]))
      evidence[mi,]$label_<-conditions.labels[i]
    }
  }
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
  if(PDdata){ rawfile_col<-'Spectrum.File' }else{ rawfile_col<-'Raw.file' }
  evidence<-merge(evidence, .GlobalEnv[["rep_structure"]], by.x=c(rawfile_col), by.y=c('raw_file'))
  
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
    evidence.dt<-data.table(evidence[, c('Protein.IDs', 'Unique.Sequence.ID', 'Intensity','label_', 'rep_desc')])
    setkey(evidence.dt, rep_desc, Protein.IDs, Unique.Sequence.ID, label_)
    # Get maximum PSM intensity per peptide/protein/[(rep_desc/label) = raw_file]
    evidence.dt<-evidence.dt[, .(maxI=max(Intensity)), by=.(rep_desc, Protein.IDs, Unique.Sequence.ID, label_)]    
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
  
  # Get a the vector of unique peptides intensities
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
  evidence.dt<-evidence.dt[, lapply(.SD, sum, na.rm = T), by=.(rep_desc, Protein.IDs), .SDcols=conditions.labels]
  ## Rename the intensity columns
  setnames(evidence.dt,colnames(evidence.dt)[which(colnames(evidence.dt) %in% conditions.labels)],paste('Intensity',conditions.labels,sep='.'))
  ## Merge with the evidence.dt.seqCounts table
  evidence.dt<-merge(evidence.dt, evidence.dt.seqCounts)  
  
  # Add the experimental structure information to evidence.dt based on rep_desc (raw file at this point has no information and is dropped)
  evidence.dt<-merge(evidence.dt ,data.table(.GlobalEnv[["rep_structure"]][! duplicated(.GlobalEnv[["rep_structure"]]$rep_desc), !grepl('raw_file', colnames(.GlobalEnv[["rep_structure"]]))]), by='rep_desc')
  ## If we have fractionation, combine fraction data (sum intensities and unique sequence counts accross fractions)
  if(length(unique(evidence.dt$fraction)) > 1){
    # Drop rep_desc and the percentages columns as we will generate a new ones after combining the fraction data.
    # Also drop the fraction col.
    evidence.dt[, c('rep_desc', 'fraction', paste0(conditions.labels,'p')) := NULL] 
    #
    setkey(evidence.dt, biorep, techrep, Protein.IDs)
    # Combine
    evidence.dt<-evidence.dt[, lapply(.SD, sum, na.rm = T), by=.(biorep, techrep, Protein.IDs)]
    # Calculate the percentage columns
    evidence.dt[, paste0(conditions.labels,'p') := lapply(.SD, function(x){return((x/sum(.SD))*100)}), by=.(biorep, techrep, Protein.IDs),.SDcols=paste('UniqueSequences',conditions.labels,sep='.')]    
    # Create new rep_desc column
    evidence.dt[, 'rep_desc' := paste0('b',biorep,'t',techrep)]    
  }
  ## If enabled, do filter out proteins based on percentage labeling for the desired label
  if(filterL && !filterL_lvl){
    n1<-length(unique(evidence.dt[get(paste0(filterL_lbl,"p")) == 100.0]$Protein.IDs))
    evidence.dt<-evidence.dt[get(paste0(filterL_lbl,"p")) < 100.0]
    levellog(paste0("read.pgroups_v3: Filtered out ", n1," proteins which where identified solely by '", filterL_lbl, "' peptides ..."));
  }
  
  # TODO:
  ## Get protein IDs that were identified/quantified in at least 'nRequiredLeastBioreps' biological replicate and at least be a total of 2 peptides in at least 'nRequiredLeastBioreps'
  Protein.IDs.quant<-evidence.dt[, .(nTechrep=.N, n.Unique.Sequence.IDs=sum(n.Unique.Sequence.IDs)), by=.(Protein.IDs, biorep)][, .(pass=.N >= (.GlobalEnv[["nRequiredLeastBioreps"]]) && n.Unique.Sequence.IDs >= (.GlobalEnv[["nRequiredLeastBioreps"]])), by=.(Protein.IDs)][pass == T, Protein.IDs]
  evidence.dt<-evidence.dt[Protein.IDs %in% Protein.IDs.quant]
  
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
      # Rename the 'n.Unique.Sequence.IDs' column to <rep_desc_X>.Ratio.counts
      colsl<-allcols %in% c('n.Unique.Sequence.IDs')
      colnames(rep_desc_i_pgroups)[colsl]<-paste(rep_desc_i,'Ratio.counts',sep='.')
      # merge with the growing data frame
      pgroups<-merge(pgroups, rep_desc_i_pgroups[, ! colnames(rep_desc_i_pgroups) %in% c('biorep', 'techrep', 'fraction', 'rep_desc')], all.x = T)
  }
  # Step 2: Calculate the columns [<label/condition_Y> ...] [<label/condition_Y>p ...]
  allcols<-colnames(pgroups)
  for(cond_i in conditions.labels){
    colsl<-grepl(paste('uniqueSequences', cond_i,sep='\\.') ,allcols)
    pgroups[, cond_i]<-rowSums(pgroups[, allcols[colsl]])
  }
  # Step 3: Calculate the columns [<label/condition_Y>p ...]
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

# Reads MaxQuant (1.3.0.5) proteinGroups table, discards information not required (for downstream analysis) and returns the table
read.pgroups_v2<-function(fname,evidence_fname, time.point,generateVenns=F){
  pgroups<-read.table(fname, header = T, sep = "\t",quote='',stringsAsFactors = FALSE,comment.char="")
  # mq_labels_names is a vector of the labels found in the data (not user defined)
  # conditions.labels is the same vector or a subset of it in the case the user has specified that there are contaminant labeled species
  # WARNING: the above implies that the user must know the right names for the labels before he can use the addLabel() function

  mq_labels_names<-c()
  if(filterL){
    mq_labels_names<-c(filterL_lbl, conditions.labels)
  }else{
    mq_labels_names<-conditions.labels
  }
  #mq_labels_names<-unique(sub("^Intensity\\.([^\\.]+)\\..+$","\\1",colnames(pgroups)[grep("^Intensity\\.([^\\.]+)\\..+$",colnames(pgroups))]))

  evidence<-read.table(evidence_fname, header = T, sep = "\t",quote='',stringsAsFactors = FALSE,comment.char="")
  
  pgroups<-pgroups[pgroups$Reverse != "+" & pgroups$Contaminant != "+", ]
	pgroups$Protein.IDs<-paste(sub("^([^;]*).*","\\1",pgroups$Protein.names)," [",sub("^([^;]*).*","\\1",pgroups$Gene.names)," ...] [",sub("^([^;]*).*","\\1",pgroups$Protein.IDs)," ...]",sep="")

  levellog(paste("read.pgroups_v2: Identified proteins (w/o contaminants): ",nrow(pgroups)," (",time.point,")",sep=""))
  
	pgroups_evidence<-as.data.frame(do.call(rbind,apply(cbind(as.character(pgroups$id),pgroups$Evidence.IDs),1,function(x) cbind(x[1],unlist(strsplit(x[2],";"))))),stringsAsFactors=F)
  colnames(pgroups_evidence)<-c("id","Evidence.IDs")
	pgroups_evidence$id<-as.numeric(pgroups_evidence$id)
	pgroups_evidence$Evidence.IDs<-as.numeric(pgroups_evidence$Evidence.IDs)
  evidence_labels_intensity_cols<-paste("Intensity.",mq_labels_names,sep="")
  evidence_sequences<-evidence[,c("id","Sequence","Raw.file","Labeling.State","Modifications", evidence_labels_intensity_cols)]
  colnames(evidence_sequences)[1:5]<-c("Evidence.IDs","Sequence","Spectrum.File","Labeling.State", "Modifications")
	pgroups_evidence<-merge(pgroups_evidence,evidence_sequences,by="Evidence.IDs",all.x=T)
  pgroups_evidence<-merge(pgroups_evidence,pgroups[,c("id","Protein.IDs")],by="id",all.x=T)
  pgroups_evidence<-pgroups_evidence[,-1]
    
  #Count the number of peptide labels (unique combination of peptide sequence and labeling state) per protein
  subtotals<-ddply(pgroups_evidence,c("Protein.IDs"),function(x){
    cond_lengths<-c()
    for(cond_i in conditions.labels){
      x_idxs<-which(x$Labeling.State==(which(cond_i==conditions.labels)-1))
      x$lab_seq<-paste(x$Labeling.State,x$Sequence,sep="_")
      cond_lengths<-cbind(cond_lengths, length(unique(x[x_idxs,c("lab_seq")])))
    }
    ret<-data.frame(cond_lengths)
    colnames(ret)<-conditions.labels
    return(ret)
  })
  
  lblcols_idxs<-which(colnames(subtotals) %in% mq_labels_names)
  #Calculate the respective percentages
  for(label_i in conditions.labels){
    subtotals$newcol<-apply(subtotals,1,function(x){
      row_subtotals<-as.numeric(x[lblcols_idxs])
      if(sum(row_subtotals) == 0){
        return(0)
      }else{
        return(100*(row_subtotals[which(label_i==conditions.labels)]/sum(row_subtotals)))
      }
    })
    colnames(subtotals)[length(colnames(subtotals))]<-paste(label_i,"p",sep="")
  }  
  
  if(ProteinQuantitation){
    #Count the number of peptide sequences per protein and replicate
    subtotals2<-ddply(pgroups_evidence,c("Spectrum.File","Protein.IDs"),function(x){
      uniqueSequences<-length(unique(x$Sequence))
      ret<-data.frame(cbind(uniqueSequences))
      colnames(ret)<-c("uniqueSequences")
      return(ret)
    })
  }else{
    #Calculate peptide intesity per label and per replicate
    #Warning: Contrary to PD data, here we don't peptide uniqueness information, so one peptide might
    # correspond to more than one protein IDs
    subtotals2<-ddply(pgroups_evidence,c("Spectrum.File","Sequence","Modifications"),function(x){
      cond_sums<-c()
      for(cond_i in evidence_labels_intensity_cols){
        cond_sums<-cbind(cond_sums, sum(x[,cond_i],na.rm=T))
      }
      cond_counts<-sum(!is.na(x[,evidence_labels_intensity_cols[1]]))
      ret<-data.frame(cbind(cond_sums,cond_counts))
      colnames(ret)<-c(paste("Intensity.",mq_labels_names,sep=""),"Ratio.counts")
      ret$Evidence.IDs<-paste(x$Evidence.IDs,collapse=";")
      ret$Sequence<-toupper(x$Sequence[1])
      ret$Protein.IDs<-paste(unique(x$Protein.IDs),collapse=";") # maybe more than on protein IDs, so we have to collapse
      ret$nProteins<-length(unique(x$Protein.IDs))
      return(ret)    
    })
    #discard peptides that belong to more than one protein (i.e. non-unqiue)
    subtotals2<-subtotals2[subtotals2$nProteins<2,]
    subtotals2<-subtotals2[,!colnames(subtotals2) %in% "nProteins"]
    # Remove peptides that are not of interest (conditions.Mod is not empty means the user wants peptides with certain modifications only)
    if(!ProteinQuantitation && length(conditions.Mods)>0){
      for(i in 1:length(conditions.Mods)){
        for(mod_i_mods in conditions.Mods.Modifications[[i]]){
          #strip modification from parentheses and escape characters
          mod_i_mods<-sub("\\\\","",mod_i_mods)
          mod_i_mods<-sub(")$","",mod_i_mods)
          subtotals2<-subtotals2[grepl(mod_i_mods,subtotals2$Modifications),]
        }
      } 
    }
  }
  #Re-arrange data and assign column names
  if(ProteinQuantitation){
    melted_subtotals<-melt(subtotals2)
  }else{
    melted_subtotals<-melt(subtotals2,id.vars=c("Spectrum.File", "Sequence", "Modifications", "Evidence.IDs","Protein.IDs"))
  }
  
	melted_subtotals$Spectrum.File<-factor(melted_subtotals$Spectrum.File)
  tmp_merged<-merge(data.frame(raw_file=levels(melted_subtotals$Spectrum.File),row_order=1:nlevels(melted_subtotals$Spectrum.File)),rep_structure)
	levels(melted_subtotals$Spectrum.File)<-tmp_merged[order(tmp_merged$row_order),]$rep_desc
  
	if(ProteinQuantitation){
	  pgroups_uniqueSequences<-as.data.frame(tapply(melted_subtotals$value,list(Protein.IDs=melted_subtotals$Protein.IDs,RawFile=melted_subtotals$Spectrum.File,LMHn=melted_subtotals$variable),function(x) x))
	}else{
	  pgroups_uniqueSequences<-as.data.frame(tapply(melted_subtotals$value,list(Peptide.IDs=paste(melted_subtotals$Sequence," [",melted_subtotals$Modifications,"]",sep=""),RawFile=melted_subtotals$Spectrum.File,LMHn=melted_subtotals$variable),function(x) sum(x,na.rm=T)))
	}
  
	pgroups_uniqueSequences[,paste(quantitated_items_lbl,".IDs",sep="")]<-row.names(pgroups_uniqueSequences)

  # At this point we merge unique sequences counts with already calculated for us peptide intensities and counts of intensities ratios (available in the proteinGroups.txt->pgroups variable), contrary to the PD case where we have to do these calculations on our own
  if(ProteinQuantitation){
	  if(keepEvidenceIDs){
	  	pgroups<-merge(pgroups_uniqueSequences, pgroups[,c("Protein.IDs",sort(colnames(pgroups)[grep("Ratio\\..*\\.count.b",colnames(pgroups))]),sort(colnames(pgroups)[grep("Intensity\\..*\\.b",colnames(pgroups))]),"Evidence.IDs")],by="Protein.IDs",all.x=T)
	  }else{
	  	pgroups<-merge(pgroups_uniqueSequences, pgroups[,c("Protein.IDs",sort(colnames(pgroups)[grep("Ratio\\..*\\.count.b",colnames(pgroups))]),sort(colnames(pgroups)[grep("Intensity\\..*\\.b",colnames(pgroups))]))],by="Protein.IDs",all.x=T)
	  }
  }else{
    subtotals2$Peptide.IDs<-paste(subtotals2$Sequence," [",subtotals2$Modifications,"]",sep="")
    #pgroups_evidence$Peptide.IDs<-paste(pgroups_evidence$Sequence," [",pgroups_evidence$Modifications,"]",sep="")
    #subtotals2<-merge(subtotals2, unique(pgroups_evidence[,c("Peptide.IDs","Protein.IDs")]),by="Peptide.IDs",all.x=T)
    #if(keepEvidenceIDs){
    #  pgroups<-merge(pgroups_uniqueSequences, unique(subtotals2[,c("Peptide.IDs","Protein.IDs","Evidence.IDs")]),by="Peptide.IDs",all.x=T)
    #}else{
      pgroups<-merge(pgroups_uniqueSequences, unique(subtotals2[,c("Peptide.IDs","Protein.IDs")]),by="Peptide.IDs",all.x=T)
    #}    
  }
	#For razor-unique peptide column: replace the triple-dot R replaces the MaxQuant column name with
  #colnames(pgroups)<-sub("\\.\\.\\.","\\.",colnames(pgroups)) 
	#colnames(pgroups)<-sub("Ratio.([^\\.]+)\\.([^\\.]+)\\.count\\.(.*)","\\3.Ratio.counts.\\1.\\2",colnames(pgroups))  
  #pgroups<-merge(pgroups,subtotals,by="Protein.IDs",all.x=T)
  
  if(ProteinQuantitation){
    colnames(pgroups)[grep("^Ratio.[^\\.]+\\.[^\\.]+\\.count\\.",colnames(pgroups))]<-sub("^Ratio.([^\\.]+)\\.([^\\.]+)\\.count\\.(.+)$","\\3.\\1.\\2.Ratio.counts",colnames(pgroups)[grep("^Ratio.[^\\.]+\\.[^\\.]+\\.count\\.",colnames(pgroups))])
    #
    #for(i in 1:length(conditions.labels)){
    #  colnames(pgroups)[grep("^Intensity\\.",colnames(pgroups))]<-sub(paste("^Intensity\\.(",mq_labels_names[i],")\\.",sep=""),paste("Intensity.",conditions.labels[i],".",sep=""),colnames(pgroups)[grep("^Intensity\\.",colnames(pgroups))])
    #}    
  }else{
    #for(i in 1:length(conditions.labels)){
    #   colnames(pgroups)[grep(paste("^[^\\.]+\\.Intensity\\.",mq_labels_names[i],"$",sep=""),colnames(pgroups))]<-sub(paste("^([^\\.]+)\\.Intensity\\.",mq_labels_names[i],"$",sep=""),paste("Intensity.",conditions.labels[i],".\\1",sep=""),colnames(pgroups)[grep(paste("^[^\\.]+\\.Intensity\\.",mq_labels_names[i],"$",sep=""),colnames(pgroups))])
    #}    
  }
  
  if(ProteinQuantitation){
    tmp_map<-unique(merge(evidence,data.frame(Raw.file=rep_structure$raw_file,biorep=rep_structure$biorep,techrep=rep_structure$techrep))[,c("biorep","techrep","Experiment")])
    tmp_map$rep_desc<-paste("b",tmp_map$biorep,"t",tmp_map$techrep,sep="")
    tmp_str<-colnames(pgroups)[grep("Ratio\\.counts$",colnames(pgroups))]
    tmp_matches<-sub("(^[^\\.]+).*","\\1",tmp_str)
    tmp_matches_invert<-sub("^[^\\.]+(.*)","\\1",tmp_str)
    tmp_merged<-merge(data.frame(Experiment=tmp_matches, Experiment_rest=tmp_matches_invert, row_order=1:length(tmp_matches)),tmp_map)
    rep_desc<-paste(tmp_merged[order(tmp_merged$row_order),]$rep_desc,tmp_merged[order(tmp_merged$row_order),]$Experiment_rest,sep="")
    colnames(pgroups)[grep("Ratio\\.counts$",colnames(pgroups))]<-rep_desc
    
    tmp_str<-colnames(pgroups)[grep("^Intensity",colnames(pgroups))]
    tmp_matches<-sub(".*\\.([^\\.]+)$","\\1",tmp_str)
    tmp_matches_invert<-sub("(.*\\.)[^\\.]+$","\\1",tmp_str)
    tmp_merged<-merge(data.frame(Experiment=tmp_matches, Experiment_rest=tmp_matches_invert, row_order=1:length(tmp_matches)),tmp_map)
    
    colnames(pgroups)[grep("^Intensity",colnames(pgroups))]<-sub("^Intensity\\.([^\\.]+)\\..*","Intensity.\\1.",colnames(pgroups)[grep("^Intensity",colnames(pgroups))])
    colnames(pgroups)[grep("^Intensity",colnames(pgroups))]<-paste(colnames(pgroups)[grep("^Intensity",colnames(pgroups))],tmp_merged[order(tmp_merged$row_order),]$rep_desc,sep="")
    
    
  }
  row.names(pgroups)<-pgroups[,paste(quantitated_items_lbl,".IDs",sep="")]
  
  #
  if(generateVenns){
    setwd(limma_output)
    venn_data<-id_Venn3_pgroups(pgroups)
    write.table(venn_data,file=paste(outputFigsPrefix,"_id_venn3-data_",time.point,".txt",sep=""),sep="\t",row.names=F)
    setwd("..")
  }
  #
  
  if(ProteinQuantitation){
    #Remove proteins with total ratio count of 0 (not quantifiable)
    pgroups<-pgroups[rowSums(pgroups[,colnames(pgroups)[grep("Ratio\\.counts",colnames(pgroups))]],na.rm=T)>0,]
    #Integrate percentage labeling information
    pgroups<-merge(pgroups,subtotals,by="Protein.IDs",all.x=T)
    #If enabled, do filter out proteins based on percentage labeling for the desited label
    if(filterL && !filterL_lvl){
      fl<-paste(filterL_lbl,"p",sep="")
      pgroups<-pgroups[pgroups[,c(fl)]<100,]
    }
  }
  
  pgroups$time.point<-time.point
  
  quant_species<-"proteins"
  if(!ProteinQuantitation){
    quant_species<-"peptides"
  }  
  
  levellog(paste("read.pgroups_v2: Quantifiable ",quant_species," (w/o contaminants): ",nrow(pgroups)," (",time.point,")",sep=""))

  if(generateVenns){
    setwd(limma_output)
    venn_data<-quant_Venn3_pgroups(pgroups_filter_2reps_v2(pgroups,time.point))
    write.table(venn_data,file=paste(outputFigsPrefix,"_quant_venn3-data-2reps_",time.point,".txt",sep=""),sep="\t",row.names=F)
    setwd("..")  
  }
  
	return(pgroups)
}

## PATCHED -- number of conditions/labels-indpendent function
#Like the above, but for quantified proteins (quantified means having a Ratio.H.M.count greater than 0 for each replicate.
quant_Venn3_pgroups<-function(pgroups){
  
  pgroups<-pgroups[,c(paste(quantitated_items_lbl,".IDs",sep=""),colnames(pgroups)[grep("\\.Ratio\\.counts$",colnames(pgroups))])]
  
  venn_data<-c()
    for(brep_i in 1:.GlobalEnv[["n_bioreps"]]){
      if(.GlobalEnv[["n_techreps"]]>1){
        b_i<-data.frame(Protein.IDs=pgroups[rowSums(pgroups[,colnames(pgroups)[grep(paste("^b",brep_i,sep=""),colnames(pgroups))]], na.rm=T) > 0,c(paste(quantitated_items_lbl,".IDs",sep=""))],stringsAsFactors=F)
        b_i$rep<-as.character(brep_i)
        venn_data<-rbind(venn_data,b_i)
      }else{
        b_i_col<-colnames(pgroups)[grep(paste("^b",brep_i,sep=""),colnames(pgroups))]
        b_i<-data.frame(Protein.IDs=pgroups[!is.na(pgroups[,b_i_col]) & pgroups[,b_i_col]>0,c(paste(quantitated_items_lbl,".IDs",sep=""))],stringsAsFactors=F)
        b_i$rep<-as.character(brep_i)
        venn_data<-rbind(venn_data,b_i)
      }
    }
  colnames(venn_data)[1]<-paste(quantitated_items_lbl,".IDs",sep="")

	return(venn_data)
}


# MaxQuant (1.3.0.5) only
# Like the above, apart from the quant filter.
# Quantified proteins using this function will be considered only those with a total Ratio.H.M.count > 2 for at least two replicates
#TODO: generalize for any replicate structure for the identified proteins, as it now assumes 3 biological x 3 technical replicates (nested)
do_generate_Venn3_data_quant_filter_2reps<-function(pgroups,time.point,outputFigsPrefix=""){
	setwd(limma_output)
	#venn_data<-quant_Venn3_pgroups(pgroups[pgroups$time.point == time.point,])
	venn_data<-quant_Venn3_pgroups(pgroups_filter_2reps_v2(pgroups[pgroups$time.point == time.point,]))
	write.table(venn_data,file=paste(outputFigsPrefix,"_quant_venn3-data-2reps_",time.point,".txt",sep=""),sep="\t",row.names=F)
	venn_data<-id_Venn3_pgroups(pgroups[pgroups$time.point == time.point,])
	write.table(venn_data,file=paste(outputFigsPrefix,"_id_venn3-data_",time.point,".txt",sep=""),sep="\t",row.names=F)
	setwd("..")
}

# Like the above, but for PD (1.3)
do_generate_Venn3_data_quant_filter_2reps_PD<-function(pgroups,time.point,evidence_fname,outputFigsPrefix=""){
  venn_data<-id_Venn3_pgroups_PD("",evidence_fname,time.point)
	setwd(limma_output)
	write.table(venn_data,file=paste(outputFigsPrefix,"_id_venn3-data_",time.point,".txt",sep=""),sep="\t",row.names=F)
  venn_data<-quant_Venn3_pgroups(pgroups_filter_2reps_v2(pgroups[pgroups$time.point == time.point,]))
	write.table(venn_data,file=paste(outputFigsPrefix,"_quant_venn3-data-2reps_",time.point,".txt",sep=""),sep="\t",row.names=F)
	setwd("..")
}

#More stringent quant filter, require quantitation in at least 2 replicates if bioreps>1 or in at least 2 injections if bioreps=1
pgroups_filter_2reps_v2<-function(pgroups){	#reps is dummy here
  #write.table(pgroups,file="proteinGroups_beforeFiltering.txt",sep="\t",row.names=F);
	reps_cols<-colnames(pgroups)[grep("Ratio\\.counts",colnames(pgroups))]
	
	ratioRepTruth<-c()
	pgroups[,reps_cols]<-apply(pgroups[,reps_cols], 2,function(x){replace(x, is.na(x), 0)})
	
	tmp_order<-regmatches(colnames(pgroups)[grep("Ratio\\.counts",colnames(pgroups))],regexpr("^b[0-9][^\\.]+",colnames(pgroups)[grep("Ratio\\.counts",colnames(pgroups))]))
	tmp_orderdf<-data.frame(rep_desc=tmp_order)
	tmp_orderdf$rindex<-1:nrow(tmp_orderdf)
  
	# if the following is true, then it means we have fractions and we no longer need them, so rep_structure has to be redefined
	#if(nrow(tmp_orderdf) < (nrow(rep_structure)*nConditions)){
	
  colnames(.GlobalEnv[["rep_structure"]])[grep("rep_desc",colnames(rep_structure))]<-"rep_desc_old"
	.GlobalEnv[["rep_structure"]]$rep_desc<-gsub("^b([0-9]+)t([0-9]+).*","b\\1t\\2",rep_structure$rep_desc_old)
	.GlobalEnv[["rep_structure"]]<-unique(rep_structure[,c("biorep","techrep","rep_desc")])
	# TESTING the following  
  #.GlobalEnv[["rep_structure"]]$rep_desc<-paste(.GlobalEnv[["rep_structure"]]$rep_desc,'f0',sep='')
	#}
  indexmap<-merge(tmp_orderdf,rep_structure)
	indexmap<-indexmap[order(indexmap$rindex),]
  
	nConditions_combs<-nrow(combinations(nConditions,2,1:nConditions))
	i<-1
  for(i in 1:.GlobalEnv[["n_bioreps"]]){
  #for(rep_cols_i in i_bioreps){
		curr_techreps_cols<-reps_cols[indexmap[indexmap$biorep==i,]$rindex]
		#ratioRepTruth<-cbind(ratioRepTruth,rowSums(pgroups[,curr_techreps_cols],na.rm=T)>2)
		if(.GlobalEnv[["n_techreps"]]>1){
        		ratioRepTruth<-cbind(ratioRepTruth,rowSums(pgroups[,curr_techreps_cols],na.rm=T)>(2*nConditions_combs))
      		}else{
        		ratioRepTruth<-cbind(ratioRepTruth,pgroups[,curr_techreps_cols]>0)
      		}
    i<-i+1
	}
	filter<-apply(ratioRepTruth,1,function(x) length(which(x)))
	if(.GlobalEnv[["n_bioreps"]]>1){
    pgroups_intersect<-pgroups[filter>(nRequiredLeastBioreps-1),]
	}else{
	  pgroups_intersect<-pgroups[filter>0,]
	}
	
	#replace 0 with NA
	pgroups_intersect<-as.data.frame(lapply(pgroups_intersect, function(x){replace(x, x == 0, NA)}),stringsAsFactors = FALSE)
	pgroups_intersect<-as.data.frame(lapply(pgroups_intersect, function(x){replace(x, is.nan(x), NA)}),stringsAsFactors = FALSE)

  quant_species<-"proteins"
  if(!ProteinQuantitation){
    quant_species<-"peptides"
  }
  #if(n_bioreps>1){
  #  cat(paste("pgroups_filter_2reps_v2: Quantified ",quant_species," (>2 peptides/",n_techreps," injection(s) in at least ",nRequiredLeastBioreps," replicates): ",nrow(pgroups_intersect[pgroups_intersect$time.point == time.point,])," (",time.point,")\n",sep=""))
  #}else{
  #  cat(paste("pgroups_filter_2reps_v2: Quantified ",quant_species," (>2 peptides/",n_techreps," injection(s)): ",nrow(pgroups_intersect[pgroups_intersect$time.point == time.point,])," (",time.point,")\n",sep=""))
  #}
	#write.table(pgroups_intersect,file="proteinGroups_AfterFiltering.txt",sep="\t",row.names=F);
	return(pgroups_intersect)
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

#Perform the analysis using the more stringent quant filter (see above pgroups_filter_2reps_v2)
do_analyse_all_2reps_v2<-function(pgroups,time.point,exp_design_fname,exportFormat="pdf",outputFigsPrefix=""){
	levellog("Filtering data based on desired reproducibility level...",change=1)
	working_pgroups<-pgroups_filter_2reps_v2(pgroups[pgroups$time.point == time.point,])
	levellog("Formatting data for the statistical analysis ...")
	working_pgroups<-prepare_working_pgroups(working_pgroups)
	outputFigsPrefix<-paste(outputFigsPrefix,"-all-2reps",sep="")
	levellog("Performing the statistical analysis ...")
	ret<-do_limma_analysis(working_pgroups,time.point,exp_design_fname,exportFormat="pdf",outputFigsPrefix=outputFigsPrefix)
	levellog("",change=-1)
	return(ret)
}

#MaxQuant (1.3.0.5) only
#Gets rid of proteins identified just by peptides of a certain label named 'filterL_lbl'.
filter_unlabeled_proteins<-function(protein_groups,evidence,filterL_lbl="")
{
  levellog("", change=1)
	N_proteins_before<-nrow(protein_groups)
	N_peptides_before<-nrow(evidence)
	#protein_groups$Protein.IDs<-paste(sub("^([^;]*).*","\\1",protein_groups$Protein.names)," [",sub("^([^;]*).*","\\1",protein_groups$Gene.names)," ...] [",sub("^([^;]*).*","\\1",protein_groups$Protein.IDs)," ...]",sep="")
	#protein_groups$Protein.IDs<-gsub("\"","",protein_groups$Protein.IDs)
	evidence<-evidence[,c("id","Protein.group.IDs","Sequence","Labeling.State","Raw.file","MS.MS.Scan.Number","m.z","Charge","Mass.Error..ppm.","K.Count","R.Count")]
	evidence<-evidence[!is.na(evidence$Labeling.State) & evidence$Labeling.State > 0,]
	N_peptides_after<-nrow(evidence)
	evidence_pgroups<-as.data.frame(do.call(rbind,apply(evidence,1,function(x) cbind(x["id"],unlist(strsplit(x["Protein.group.IDs"],";"))))),stringsAsFactors=F)
	colnames(evidence_pgroups)<-c("id","Protein.group.IDs")
	row.names(evidence_pgroups)<-NULL
	protein_groups<-protein_groups[protein_groups$id %in% evidence_pgroups$Protein.group.IDs,]
	N_proteins_after<-nrow(protein_groups)
	levellog(paste("filter_unlabeled_proteins: Before L peptide filtering (proteins, peptides): ",N_proteins_before,", ",N_peptides_before,". After: ",N_proteins_after,", ",N_peptides_after,".",sep=""))
  levellog("", change=-1)
	return(protein_groups)
}

# Again, for MaxQuant only
getProteinPeptideData_2reps_filter<-function(pgroups_fname,evidence_fname,output_fname,time.point, filterL=T,linkLimmaout=F,limma_outfname=""){
	protein_groups<-read.pgroups_v2(pgroups_fname,time.point,filterL=filterL,evidence_fname=evidence_fname,keepEvidenceIDs=T)
	protein_groups<-pgroups_filter_2reps_v2(protein_groups[protein_groups$time.point == time.point,])

	evidence<-read.table(evidence_fname, header = T, sep = "\t",quote='',stringsAsFactors=F,comment.char = "")
	
	peptide_evidence<-as.data.frame(do.call(rbind,apply(protein_groups,1,function(x) cbind(x["Protein.IDs"],unlist(strsplit(x["Evidence.IDs"],";"))))),stringsAsFactors=F)
	colnames(peptide_evidence)<-c("Protein.IDs","Evidence.ID")
	peptide_evidence$Protein.IDs<-factor(peptide_evidence$Protein.IDs)
	peptide_evidence$Evidence.ID<-as.numeric(peptide_evidence$Evidence.ID)
	evidence<-evidence[,c("id","Sequence","Labeling.State","Raw.file","MS.MS.Scan.Number","m.z","Charge","Mass.Error..ppm.","K.Count","R.Count","Protein.group.IDs")]
	colnames(evidence)<-c("Evidence.ID","Sequence","Labeling.State","Raw.file","MS.MS.Scan.Number","m.z","Charge","Mass.Error.ppm.","K.Count","R.Count","Protein.group.IDs")
	peptide_evidence<-merge(peptide_evidence,protein_groups[,!names(protein_groups) %in% c("Evidence.IDs")],by="Protein.IDs",all.x=T)
	peptide_evidence<-merge(peptide_evidence,evidence,by="Evidence.ID",all.x=T)
	if(linkLimmaout){
		limmaout<-read.table(limma_outfname, header = T, sep = "\t",quote='',stringsAsFactors=F,comment.char = "")
		peptide_evidence<-merge(peptide_evidence,limmaout,by="Protein.IDs",all.x=T)
	}
	write.table(peptide_evidence,file=output_fname,sep="\t",row.names=F)
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

#   i<-1
#   for(lbl_i in conditions.labels.Modifications){
#     for(mod_i in lbl.Modifications){
#       lbl_i_matches<-which(grepl(paste("^",mod_i,"$",sep=""),lbl_i))
#       if(length(lbl_i_matches) != 0){
#         cat(paste("addLabel: Error adding label '",lblname,"': Existing label with name '",conditions.labels[i],"' has an identical modification (",mod_i,").",sep=""))
#         return(FALSE)
#       }
#     }
#     i<-i+1
#   }
  
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
source("/home/gefstathiou/Documents/ProteoSign/ProteoSign/uploads/L2_MQ/msdiffexp_wd/MSdiffexp_definitions.R")
#source("/home/gefstathiou/Documents/ProteoSign/ProteoSign/uploads/LF/msdiffexp_wd/MSdiffexp_definitions.R")
#source("MSdiffexp_definitions.R")

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
  levellog("Removing double quotes from input data file #1 ...")
  tmpdata<-gsub("\"", "", readLines(evidence_fname))
  evidence_fname_cleaned<-file(evidence_fname, open="w")
  writeLines(tmpdata, con=evidence_fname_cleaned)
  close(evidence_fname_cleaned)
  levellog("Reading input data ...")
  if(PDdata){
    protein_groups<<-read.pgroups_v3(pgroups_fname,evidence_fname,time.point,keepEvidenceIDs=T)
    #protein_groups<<-read.pgroups_v2_PD(pgroups_fname,evidence_fname,time.point,keepEvidenceIDs=T)
    #do_generate_Venn3_data_quant_filter_2reps_PD(protein_groups,time.point,evidence_fname,outputFigsPrefix=outputFigsPrefix)
  }else{
    levellog("Removing double quotes from input data file #2 ...")
    tmpdata<-gsub("\"", "", readLines(pgroups_fname))
    pgroups_fname_cleaned<-file(pgroups_fname, open="w")
    writeLines(tmpdata, con=pgroups_fname_cleaned)
    close(pgroups_fname_cleaned)    
    #protein_groups<<-read.pgroups_v2(pgroups_fname,evidence_fname,time.point,generateVenns=F)
    protein_groups<<-read.pgroups_v3(pgroups_fname,evidence_fname,time.point,keepEvidenceIDs=T)
    #do_generate_Venn3_data_quant_filter_2reps(protein_groups,time.point,outputFigsPrefix=outputFigsPrefix)
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
  #results<-do_analyse_all_2reps_v2(protein_groups,time.point,exp_design_fname,exportFormat="pdf",outputFigsPrefix=outputFigsPrefix)
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

