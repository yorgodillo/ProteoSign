# Based on a Progenesis LC-MS Limma Analysis script

# Load the limma library so we can use it
# Can be installed by running following commands in R
#
source("http://www.bioconductor.org/biocLite.R")
if(!require("limma")){ biocLite("limma") }
if(!require("statmod")){ biocLite("statmod") }
if(!require("ggplot2")){ install.packages("ggplot2", repos="http://cran.fhcrc.org") }
if(!require("reshape")){ install.packages("reshape", repos="http://cran.fhcrc.org") }
if(!require("plyr")){ install.packages("plyr", repos="http://cran.fhcrc.org") }
if(!require("tcltk")){ install.packages("tcltk", repos="http://cran.fhcrc.org") }
if(!require("gtools")){ install.packages("gtools", repos="http://cran.fhcrc.org") }

library(limma)
library(reshape)
library(plyr)
library(ggplot2)
library(tcltk)
library(gtools)

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
  n_bioreps<-length(which(!duplicated(rep_structure)))/nConditions
  n_techreps<-length(rep_structure)/(n_bioreps*nConditions)
  if(n_bioreps>1){
    cat(paste("do_results_plots: Quantified ",quant_species," (>2 peptides/",n_techreps," injection(s) in at least ",nRequiredLeastBioreps," replicates): ",nrow(results)," (",time.point,")\n",sep=""))
  }else{
    cat(paste("do_results_plots: Quantified ",quant_species," (>2 peptides/",n_techreps," injection(s)): ",nrow(results)," (",time.point,")\n",sep=""))
  }  
  
  for(i in 1:nrow(ratio_combs)){
	col_desc_<-paste("P-value adjusted ",paste(conditions.labels[ratio_combs[i,2]],"/",conditions.labels[ratio_combs[i,1]],sep=""),sep="")
	ndiffexp_tmp<-length(which(results[,col_desc_]<0.05))
	cat(paste("do_results_plots: Differentially expressed for ",conditions.labels[ratio_combs[i,2]]," vs ",conditions.labels[ratio_combs[i,1]]," : ",ndiffexp_tmp,"\n",sep=""))
  }
  cat(paste("do_results_plots: Differentially expressed in at least one combination of conditions: ",ndiffexp,"\n",sep=""))
  
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
  dec <- ","
  if(grepl("(English|Thai)",Sys.getlocale())){
    dec <- "."
  }
  write.table(diffexp[signTruth,],dec=dec,file=paste(outputFigsPrefix,"_diffexp_",time.point,".txt",sep=""),sep="\t",row.names=F,quote=F)
  
  diffexp<-merge(diffexp,results[,-grep("^(avg log2|P-value adjusted)",colnames(results))],by.x=c(quantitated_items_lbl),by.y=c("ID"),all.x=T)
  write.table(diffexp,dec=dec,file=paste(outputFigsPrefix,"_results_",time.point,".txt",sep=""),sep="\t",row.names=F,quote=F)
  
  levellog("",change=-1)
  return(results)
}

# Performs the differential expression analysis through limma, after quantile normalization.
do_limma_analysis<-function(working_pgroups,time.point,exp_design_fname,rep_structure,exportFormat="pdf",outputFigsPrefix=""){
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

	# Setup design matrix
	# This specifies the design of the experiment for limma, replicating
	# the info in the sample key, but representing it in a matrix format
	levellog("Constructing the design matrix ...")
	design <- model.matrix(~0 + factor(sample.key$Category))
	colnames(design) <- levels(sample.key$Category)
	write.table(design,file=paste(outputFigsPrefix,"_limma-design-matrix_",quantitated_items_lbl,"Groups.txt",sep=""),sep="\t",row.names = T, col.names=NA)
	write.table(rep_structure,file=paste(outputFigsPrefix,"_limma-blocking-variable_",quantitated_items_lbl,"Groups.txt",sep=""),sep="\t",row.names = T, col.names=NA)
	fit<-""
	n_bioreps<-length(which(!duplicated(rep_structure)))/nConditions
	n_techreps<-length(rep_structure)/(n_bioreps*nConditions)
	
	levellog("Fitting the model ...")
  if(n_bioreps > 1 & n_techreps > 1){
		# technical replication specification
		corfit <- duplicateCorrelation(t(norm.median.intensities), design=design, block = rep_structure, trim = duplicateCorrelation_trim)
		# Fit the limma model to the data
		# Pass the protein names/peptide sequences to limma as the genes option
		fit <- lmFit(t(norm.median.intensities), design, genes=prot.names, block = rep_structure, cor = corfit$consensus)
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

# Reads MaxQuant (1.3.0.5) proteinGroups table, discards information not required (for downstream analysis) and returns the table
read.pgroups_v2<-function(fname,evidence_fname, time.point,generateVenns=T){
  pgroups<-read.table(fname, header = T, sep = "\t",quote='',stringsAsFactors = FALSE,comment.char="")
  mq_labels_names<-unique(sub("^Intensity\\.([^\\.]+)\\..+$","\\1",colnames(pgroups)[grep("^Intensity\\.([^\\.]+)\\..+$",colnames(pgroups))]))
  evidence<-read.table(evidence_fname, header = T, sep = "\t",quote='',stringsAsFactors = FALSE,comment.char="")
  
  pgroups<-pgroups[pgroups$Reverse != "+" & pgroups$Contaminant != "+", ]
	pgroups$Protein.IDs<-paste(sub("^([^;]*).*","\\1",pgroups$Protein.names)," [",sub("^([^;]*).*","\\1",pgroups$Gene.names)," ...] [",sub("^([^;]*).*","\\1",pgroups$Protein.IDs)," ...]",sep="")

  cat(paste("read.pgroups_v2: Identified proteins (w/o contaminants): ",nrow(pgroups)," (",time.point,")\n",sep=""))
  
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
    
  #Count the number of peptide labels (unique combination of peptide sequence and labeling state) per protein and replicate
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
  
  #Calculate the respective percentages
  for(label_i in conditions.labels){
    subtotals$newcol<-apply(subtotals,1,function(x){
      row_subtotals<-as.numeric(x[-1])
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
      cond_counts<-sum(!is.na(x[,evidence_labels_intensity_cols[1]])) # It doesn't matter which label is used for counting. This is the ratio counts, and since PD replaces missing signlas with the minimum intensity, there will always be a ratio, and the number of ratios will be equal to the number of records here. Unwanted ratios, such as those that are equal to 1 and should not be considered/counted are filtered above, at the peptide-filtering stage if chosen by the user (recommended)
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
  
	n_bioreps<-length(which(!duplicated(rep_structure)))/nConditions
	n_techreps<-length(rep_structure)/(n_bioreps*nConditions)
	i_bioreps<-which(!duplicated(rep_structure))[1:n_bioreps]
	
	melted_subtotals$Spectrum.File<-factor(melted_subtotals$Spectrum.File)
	
	rep_desc<-paste(paste("b",rep_structure[1:(length(rep_structure)/nConditions)],sep=""),paste("t",rep(1:n_techreps),sep=""),sep="")
	if(!is.na(rep_order)){
	  o<-unlist(lapply(rep_order,function(x)((x-1)*n_techreps+1):(((x-1)*n_techreps+1)+n_techreps-1)))
	  rep_desc<-rep_desc[o]
	}
	
	levels(melted_subtotals$Spectrum.File)<-rep_desc
  
	if(ProteinQuantitation){
	  pgroups_uniqueSequences<-as.data.frame(tapply(melted_subtotals$value,list(Protein.IDs=melted_subtotals$Protein.IDs,RawFile=melted_subtotals$Spectrum.File,LMHn=melted_subtotals$variable),function(x) x))
	}else{
	  pgroups_uniqueSequences<-as.data.frame(tapply(melted_subtotals$value,list(Peptide.IDs=paste(melted_subtotals$Sequence," [",melted_subtotals$Modifications,"]",sep=""),RawFile=melted_subtotals$Spectrum.File,LMHn=melted_subtotals$variable),function(x) sum(x,na.rm=T)))
	}
  
	pgroups_uniqueSequences[,paste(quantitated_items_lbl,".IDs",sep="")]<-row.names(pgroups_uniqueSequences)
  
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
    for(i in 1:length(conditions.labels)){
      colnames(pgroups)[grep("^Intensity\\.",colnames(pgroups))]<-sub(paste("^Intensity\\.(",mq_labels_names[i],")\\.",sep=""),paste("Intensity.",conditions.labels[i],".",sep=""),colnames(pgroups)[grep("^Intensity\\.",colnames(pgroups))])
    }    
  }else{
    for(i in 1:length(conditions.labels)){
       colnames(pgroups)[grep(paste("^[^\\.]+\\.Intensity\\.",mq_labels_names[i],"$",sep=""),colnames(pgroups))]<-sub(paste("^([^\\.]+)\\.Intensity\\.",mq_labels_names[i],"$",sep=""),paste("Intensity.",conditions.labels[i],".\\1",sep=""),colnames(pgroups)[grep(paste("^[^\\.]+\\.Intensity\\.",mq_labels_names[i],"$",sep=""),colnames(pgroups))])
    }    
  }
  
  if(ProteinQuantitation){
    #Retain only first combination of labels for Ratio.counts columns (others have same counts)
    #WARNING: Maybe this is not correct if MQ is used with 're-quantify' disabled 
    if(nConditions > 2){
      pgroups_rest<-pgroups[,-grep("Ratio\\.counts$",colnames(pgroups))]
      pgroups<-cbind(pgroups_rest, pgroups[,grep("Ratio\\.counts$",colnames(pgroups))[1:(n_bioreps*n_techreps)]])
    }
    colnames(pgroups)[grep("Ratio\\.counts$",colnames(pgroups))]<-sub("^([^\\.]+)\\.[^\\.]+\\.[^\\.]+\\.Ratio\\.counts","\\1.Ratio.counts",colnames(pgroups)[grep("Ratio\\.counts$",colnames(pgroups))])
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
  
  cat(paste("read.pgroups_v2: Quantifiable ",quant_species," (w/o contaminants): ",nrow(pgroups)," (",time.point,")\n",sep=""))

  if(generateVenns){
    setwd(limma_output)
    venn_data<-quant_Venn3_pgroups(pgroups_filter_2reps_v2(pgroups,time.point))
    write.table(venn_data,file=paste(outputFigsPrefix,"_quant_venn3-data-2reps_",time.point,".txt",sep=""),sep="\t",row.names=F)
    setwd("..")  
  }
  
	return(pgroups)
}

## PATCHED -- number of conditions/labels-indpendent function
# Like the (see) above function, but without protein grouping.
id_Venn3_pgroups_PD<-function(fname,evidence_fname,time.point,rep_structure,filterL=F,rep_order=NA){
  evidence<-read.table(evidence_fname, header = T, sep = "\t",quote='',stringsAsFactors=F,comment.char = "")
  #Generate Evidence ID
  evidence$id<-1:(nrow(evidence))	
  allproteins<-as.data.frame(tapply(evidence$Protein.Group.Accessions,list(Acc=evidence$Protein.Group.Accessions),length))
  allproteins<-data.frame(Protein.IDs=names(allproteins[[1]]),stringsAsFactors=F)
  
  tmpdf<-evidence[,c("Protein.Group.Accessions","Protein.Descriptions")]
  colnames(tmpdf)<-c("Protein.IDs","Protein.Descriptions")
  tmpdf<-tmpdf[!duplicated(tmpdf$Protein.IDs),]
  allproteins<-merge(allproteins,tmpdf,by="Protein.IDs",all.x=T)
  
  tmp<-sub("^([^;]*).*","\\1",allproteins$Protein.IDs)
  tmp1<-unlist(lapply(allproteins$Protein.Descriptions, function(x) substr(x,1,gregexpr(" - \\[",x)[[1]][1]-1)))
  
  allproteins$Protein.Group.Accessions<-allproteins$Protein.IDs   #keep old IDs
  allproteins$Protein.IDs<-paste(sub("^(.*) OS=.*","\\1",tmp1)," [",sub(".* GN=([^ ]*).*$","\\1",tmp1)," ...]",sep="")
  allproteins$Protein.IDs<-paste(allproteins$Protein.IDs," [",tmp," ...]",sep="")
  allproteins<-allproteins[!duplicated(allproteins$Protein.IDs),]
  
  #cat(paste("Identified proteins: ",nrow(allproteins)," (",time.point,")",sep=""))
  
  
  evidence<-merge(evidence,allproteins,by="Protein.Group.Accessions",all.x=T)
  evidence$Spectrum.File<-factor(evidence$Spectrum.File)
  
  if(ProteinQuantitation){
    #Count unique peptide sequences per protein and replicate
    subtotals2<-ddply(evidence,c("Spectrum.File","Protein.IDs"),function(x){
      ids<-paste(x$id,collapse=";")
      ret<-data.frame(cbind(ids))
      colnames(ret)<-c("Evidence.IDs")
      nUniqueSequences<-length(unique(x$Unique.Sequence.ID))
      ret$uniqueSequences<-nUniqueSequences
      return(ret)
    })
    all_evidence_ids<-ddply(subtotals2,c("Protein.IDs"),function(x) data.frame(Evidence.IDs=paste(x$Evidence.IDs,collapse=";")))
  }else{
    #Count unique peptide sequences per replicate
    #First, We have to remove the labels from the modifications column, because we have to pivot on modifications but not on labels
    modcol<-evidence$Modifications
    for(i in 1:length(conditions.labels)){
      for(mod_i in conditions.labels.Modifications[[i]]){
        if(mod_i != ""){
          modcol<-gsub(paste("[A-Z]+[0-9]+\\(",mod_i,"[; ]*",sep=""),"",modcol)
        }
      }
    }
    modcol<-gsub("^[ ]+","",modcol)
    modcol<-gsub(";[ ]+$","",modcol)
    evidence[,"Modifications.only"]<-modcol    
    
    subtotals2<-ddply(evidence,c("Spectrum.File","Unique.Sequence.ID","Modifications.only"),function(x){
      ids<-paste(x$id,collapse=";")
      ret<-data.frame(cbind(ids))
      colnames(ret)<-c("Evidence.IDs")
      ret$Sequence<-toupper(x$Sequence[1])
      return(ret)
    })
  }
  
  #Re-arrange data and assign column names
  if(ProteinQuantitation){
    melted_subtotals<-melt(subtotals2)
  }else{
    melted_subtotals<-melt(subtotals2,id.vars=c("Spectrum.File", "Sequence", "Modifications.only", "Evidence.IDs"))
  }
  
  #write.table(tmp,file="tmp.txt",row.names=F,sep="\t")
  #write.table(evidence,file="tmp2.txt",row.names=F,sep="\t")
  
  n_bioreps<-length(which(!duplicated(rep_structure)))/nConditions
  n_techreps<-length(rep_structure)/(n_bioreps*nConditions)
  i_bioreps<-which(!duplicated(rep_structure))[1:n_bioreps]
  
  melted_subtotals$Spectrum.File<-factor(melted_subtotals$Spectrum.File)
  
  rep_desc<-paste(paste("b",rep_structure[1:(length(rep_structure)/nConditions)],sep=""),paste("t",rep(1:n_techreps),sep=""),sep="")
  if(!is.na(rep_order)){
    o<-unlist(lapply(rep_order,function(x)((x-1)*n_techreps+1):(((x-1)*n_techreps+1)+n_techreps-1)))
    rep_desc<-rep_desc[o]
  }
  
  if(LabelFree){
    tmp_rep_desc<-c()
    for(i in 1:nConditions){
      tmp_rep_desc[[i]]<-paste("c",i,rep_desc,sep="")
      levels(melted_subtotals$Spectrum.File)[which(levels(melted_subtotals$Spectrum.File) %in% paste(conditions.labels.Modifications[[i]],".raw",sep=""))]<-tmp_rep_desc[[i]]
    }
    rep_desc<-unlist(tmp_rep_desc)
    melted_subtotals$brtr<-NA
    biorep_techrep<-regmatches(levels(melted_subtotals$Spectrum.File), regexpr("b.*", levels(melted_subtotals$Spectrum.File)))
    for(i in 1:length(levels(melted_subtotals$Spectrum.File))){
      lvl_i<-levels(melted_subtotals$Spectrum.File)[i]
      brtr<-biorep_techrep[i]
      melted_subtotals[melted_subtotals$Spectrum.File == lvl_i,"brtr"]<-brtr
    }
    melted_subtotals$brtr<-factor(melted_subtotals$brtr)
  }else{
    levels(melted_subtotals$Spectrum.File)<-rep_desc
  }
  
  if(ProteinQuantitation){
    if(LabelFree){
      pgroups<-as.data.frame(tapply(melted_subtotals$value,list(Protein.IDs=melted_subtotals$Protein.IDs,RawFile=melted_subtotals$brtr,LMHn=melted_subtotals$variable),function(x) sum(x,na.rm=T)))
    }else{
      pgroups<-as.data.frame(tapply(melted_subtotals$value,list(Protein.IDs=melted_subtotals$Protein.IDs,RawFile=melted_subtotals$Spectrum.File,LMHn=melted_subtotals$variable),function(x) sum(x,na.rm=T)))
    }    
  }else{
    pgroups<-as.data.frame(tapply(melted_subtotals$value,list(Peptide.IDs=paste(melted_subtotals$Sequence," [",melted_subtotals$Modifications.only,"]",sep=""),RawFile=melted_subtotals$Spectrum.File,LMHn=melted_subtotals$variable),function(x) length(x)))
    pgroups[is.na(pgroups)]<-0
  }

  pgroups[,paste(quantitated_items_lbl,".IDs",sep="")]<-row.names(pgroups)

  if(keepEvidenceIDs && ProteinQuantitation)
  {
    pgroups<-merge(pgroups,all_evidence_ids,by="Protein.IDs",all.x=T)
  }
  
  # Remove peptides that are not of interest (conditions.Mod is not empty means the user wants peptides with certain modifications only)
  if(!ProteinQuantitation && length(conditions.Mods)>0){
    for(i in 1:length(conditions.Mods)){
      for(mod_i_mods in conditions.Mods.Modifications[[i]]){
        pgroups<-pgroups[grepl(mod_i_mods,pgroups$Peptide.IDs),]
      }
    }    
  }
  
  if(ProteinQuantitation){
    #pgroups<-pgroups[,!(colnames(pgroups) %in% c("Protein.Descriptions"))]
    #cat(paste("id_Venn3_pgroups_PD: Identified proteins: ",nrow(pgroups)," (",time.point,")\n",sep=""))
  }else{
    cat(paste("id_Venn3_pgroups_PD: Peptide modified sequences: ",nrow(pgroups)," (",time.point,")\n",sep=""))
  }
  
  row.names(pgroups)<-pgroups[,paste(quantitated_items_lbl,".IDs",sep="")]
	
	venn_data<-c()
  for(brep_i in 1:n_bioreps){
    if(n_techreps>1){
      b_i<-data.frame(Protein.IDs=pgroups[rowSums(pgroups[,colnames(pgroups)[grep(paste("b",brep_i,sep=""),colnames(pgroups))]],na.rm=T)>0,c(paste(quantitated_items_lbl,".IDs",sep=""))],stringsAsFactors=F)
      b_i$rep<-as.character(brep_i)
      venn_data<-rbind(venn_data,b_i)
    }else{
      b_i_col<-colnames(pgroups)[grep(paste("b",brep_i,sep=""),colnames(pgroups))]
      b_i<-data.frame(Protein.IDs=pgroups[!is.na(pgroups[,b_i_col]) & pgroups[,b_i_col]>0,c(paste(quantitated_items_lbl,".IDs",sep=""))],stringsAsFactors=F)
      b_i$rep<-as.character(brep_i)
      venn_data<-rbind(venn_data,b_i)
    }
  }
  colnames(venn_data)[1]<-paste(quantitated_items_lbl,".IDs",sep="")
	return(venn_data)
}

#Like the above, but for MaxQuant data
id_Venn3_pgroups<-function(pgroups){
  n_bioreps<-length(which(!duplicated(rep_structure)))/nConditions
  n_techreps<-length(rep_structure)/(n_bioreps*nConditions)
  i_bioreps<-which(!duplicated(rep_structure))[1:n_bioreps]  
  
  if(ProteinQuantitation){
    pgroups<-pgroups[,c(paste(quantitated_items_lbl,".IDs",sep=""),colnames(pgroups)[grep("uniqueSequences$",colnames(pgroups))])]
  }
  venn_data<-c()
  for(brep_i in 1:n_bioreps){
    if(n_techreps>1){
      b_i<-data.frame(Protein.IDs=pgroups[rowSums(pgroups[,colnames(pgroups)[grep(paste("b",brep_i,sep=""),colnames(pgroups))]],na.rm=T)>0,c(paste(quantitated_items_lbl,".IDs",sep=""))],stringsAsFactors=F)
      b_i$rep<-as.character(brep_i)
      venn_data<-rbind(venn_data,b_i)
    }else{
      b_i_col<-colnames(pgroups)[grep(paste("b",brep_i,sep=""),colnames(pgroups))]
      b_i<-data.frame(Protein.IDs=pgroups[!is.na(pgroups[,b_i_col]) & pgroups[,b_i_col]>0,c(paste(quantitated_items_lbl,".IDs",sep=""))],stringsAsFactors=F)
      b_i$rep<-as.character(brep_i)
      venn_data<-rbind(venn_data,b_i)
    }
  }
  colnames(venn_data)[1]<-paste(quantitated_items_lbl,".IDs",sep="")
	return(venn_data)
}

## PATCHED -- number of conditions/labels-indpendent function
#Like the above, but for quantified proteins (quantified means having a Ratio.H.M.count greater than 0 for each replicate.
quant_Venn3_pgroups<-function(pgroups){
  
  n_bioreps<-length(which(!duplicated(rep_structure)))/nConditions
  n_techreps<-length(rep_structure)/(n_bioreps*nConditions)
  i_bioreps<-which(!duplicated(rep_structure))[1:n_bioreps]
  
  pgroups<-pgroups[,c(paste(quantitated_items_lbl,".IDs",sep=""),colnames(pgroups)[grep("\\.Ratio\\.counts$",colnames(pgroups))])]
  
  venn_data<-c()
    for(brep_i in 1:n_bioreps){
      if(n_techreps>1){
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
	venn_data<-quant_Venn3_pgroups(pgroups_filter_2reps_v2(pgroups[pgroups$time.point == time.point,],time.point))
	write.table(venn_data,file=paste(outputFigsPrefix,"_quant_venn3-data-2reps_",time.point,".txt",sep=""),sep="\t",row.names=F)
	venn_data<-id_Venn3_pgroups(pgroups[pgroups$time.point == time.point,])
	write.table(venn_data,file=paste(outputFigsPrefix,"_id_venn3-data_",time.point,".txt",sep=""),sep="\t",row.names=F)
	setwd("..")
}

# Like the above, but for PD (1.3)
do_generate_Venn3_data_quant_filter_2reps_PD<-function(pgroups,time.point,evidence_fname,rep_structure,rep_order=NA,outputFigsPrefix=""){
  venn_data<-id_Venn3_pgroups_PD("",evidence_fname,time.point,rep_structure,rep_order)
	setwd(limma_output)
	write.table(venn_data,file=paste(outputFigsPrefix,"_id_venn3-data_",time.point,".txt",sep=""),sep="\t",row.names=F)
  venn_data<-quant_Venn3_pgroups(pgroups_filter_2reps_v2(pgroups[pgroups$time.point == time.point,],time.point))
	write.table(venn_data,file=paste(outputFigsPrefix,"_quant_venn3-data-2reps_",time.point,".txt",sep=""),sep="\t",row.names=F)
	setwd("..")
}

#More stringent quant filter, require quantitation in at least 2 replicates if bioreps>1 or in at least 2 injections if bioreps=1
pgroups_filter_2reps_v2<-function(pgroups,reps){	#reps is dummy here
  n_bioreps<-length(which(!duplicated(rep_structure)))/nConditions
  n_techreps<-length(rep_structure)/(n_bioreps*nConditions)
  i_bioreps<-which(!duplicated(rep_structure))[1:n_bioreps]
	reps_cols<-colnames(pgroups)[grep("Ratio\\.counts",colnames(pgroups))]
	
	ratioRepTruth<-c()
	pgroups[,reps_cols]<-apply(pgroups[,reps_cols], 2,function(x){replace(x, is.na(x), 0)})
	
	for(rep_cols_i in i_bioreps){
		curr_techreps_cols<-reps_cols[c(rep(rep_cols_i,n_techreps)+(0:(n_techreps-1)))]
		#ratioRepTruth<-cbind(ratioRepTruth,rowSums(pgroups[,curr_techreps_cols],na.rm=T)>2)
		if(n_techreps>1){
        		ratioRepTruth<-cbind(ratioRepTruth,rowSums(pgroups[,curr_techreps_cols],na.rm=T)>2)
      		}else{
        		ratioRepTruth<-cbind(ratioRepTruth,pgroups[,curr_techreps_cols]>0)
      		}		
	}
	filter<-apply(ratioRepTruth,1,function(x) length(which(x)))
	if(n_bioreps>1){
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
do_analyse_all_2reps_v2<-function(pgroups,time.point,exp_design_fname,rep_structure,exportFormat="pdf",outputFigsPrefix=""){
	levellog("Filtering data based on desired reproducibility level...",change=1)
	working_pgroups<-pgroups_filter_2reps_v2(pgroups[pgroups$time.point == time.point,])
	levellog("Formatting data for the statistical analysis ...")
	working_pgroups<-prepare_working_pgroups(working_pgroups)
	outputFigsPrefix<-paste(outputFigsPrefix,"-all-2reps",sep="")
	levellog("Performing the statistical analysis ...")
	ret<-do_limma_analysis(working_pgroups,time.point,exp_design_fname,rep_structure,exportFormat="pdf",outputFigsPrefix=outputFigsPrefix)
	levellog("",change=-1)
	return(ret)
}

#MaxQuant (1.3.0.5) only
#Gets rid of proteins identified just by peptides of a certain label named 'filterL_lbl'.
filter_unlabeled_proteins<-function(protein_groups,evidence,filterL_lbl="")
{
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
	cat(paste("filter_unlabeled_proteins: Before L peptide filtering (proteins, peptides): ",N_proteins_before,", ",N_peptides_before,". After: ",N_proteins_after,", ",N_peptides_after,".\n",sep=""))
	return(protein_groups)
}

## PATCHED -- number of conditions/labels-indpendent function
read.pgroups_v2_PD<-function(fname,evidence_fname,time.point,rep_structure,keepEvidenceIDs=F,rep_order=NA){
	levellog("",change=1)
	levellog("Reading data file ...");
	evidence<-read.table(evidence_fname, header = T, sep = "\t",quote="",stringsAsFactors=F,comment.char = "")
	#Generate Evidence ID
	evidence$id<-1:(nrow(evidence))
	#Generate Protein IDs
	levellog("Generating Protein IDs ...");
	allproteins<-as.data.frame(tapply(evidence$Protein.Group.Accessions,list(Acc=evidence$Protein.Group.Accessions),length))
	allproteins<-data.frame(Protein.IDs=names(allproteins[[1]]),stringsAsFactors=F)
	
	tmpdf<-evidence[,c("Protein.Group.Accessions","Protein.Descriptions")]
	colnames(tmpdf)<-c("Protein.IDs","Protein.Descriptions")
	tmpdf<-tmpdf[!duplicated(tmpdf$Protein.IDs),]
	allproteins<-merge(allproteins,tmpdf,by="Protein.IDs",all.x=T)

	tmp<-sub("^([^;]*).*","\\1",allproteins$Protein.IDs)
	tmp1<-unlist(lapply(allproteins$Protein.Descriptions, function(x) substr(x,1,gregexpr(" - \\[",x)[[1]][1]-1)))

	allproteins$Protein.Group.Accessions<-allproteins$Protein.IDs   #keep old IDs
  allproteins$Protein.IDs<-paste(sub("^(.*) OS=.*","\\1",tmp1)," [",sub(".* GN=([^ ]*).*$","\\1",tmp1)," ...]",sep="")
	allproteins$Protein.IDs<-paste(allproteins$Protein.IDs," [",tmp," ...]",sep="")
	allproteins<-allproteins[!duplicated(allproteins$Protein.IDs),]
  
	cat(paste("read.pgroups_v2_PD: Identified proteins: ",nrow(allproteins)," (",time.point,")\n",sep=""))
	evidence<-merge(evidence,allproteins,by="Protein.Group.Accessions",all.x=T)
  levellog("Counting number of peptides per protein per label ...");
  #Count number of peptides per protein per label
  subtotals<-ddply(evidence,c("Protein.IDs"),function(x) {
	  cond_lengths<-c()
    conditionscolumn<-"Modifications"
    if(LabelFree){
      conditionscolumn<-make.names("Spectrum File")
    }
	  heavier.labels.Modifications<-conditions.labels.Modifications
    light_mod_index<-grep("^$",conditions.labels.Modifications)
    if(length(light_mod_index) == 1){
      cond_lengths<-cbind(cond_lengths, length(which(!grepl(unlabeled_peptide_regex,x$Modifications))))
      heavier.labels.Modifications<-conditions.labels.Modifications[-light_mod_index]
    }
	  for(cond_i in heavier.labels.Modifications){
	    labelTruth<-FALSE
	    for(label_i in unlist(cond_i)){
	      labelTruth<-labelTruth | grepl(label_i,x[,conditionscolumn])
	    }
	    cond_lengths<-cbind(cond_lengths, length(which(labelTruth)))
	  }
	  ret<-data.frame(cond_lengths)
	  colnames(ret)<-conditions.labels
	  return(ret)
	})  
  #Calculate the respective percentages
  levellog("Calculating the respective percentages ...");
	for(label_i in conditions.labels){
	  subtotals$newcol<-apply(subtotals,1,function(x){
      row_subtotals<-as.numeric(x[-1])
      if(sum(row_subtotals) == 0){
        return(0)
      }else{
        return(100*(row_subtotals[which(label_i==conditions.labels)]/sum(row_subtotals)))
      }
	    })
    colnames(subtotals)[length(colnames(subtotals))]<-paste(label_i,"p",sep="")
	}

  #
	evidence$Spectrum.File<-factor(evidence$Spectrum.File)
  #Remove peptides not suitable for quantitation (those originating from multiple proteins, i.e. not unique)
  if(length(grep("Quan.Info",colnames(evidence)))>0 & length(grep("Quan.Usage",colnames(evidence)))>0){
    evidence<-evidence[evidence$Quan.Info=="Unique" & evidence$Quan.Usage=="Used",]
  }
  
  #In case of label-free data, add columns of intensities for each condition like with labeled data
  if(LabelFree){
    colnames(evidence)[grep("Intensity", colnames(evidence))]<-"oldIntensity"
    new_evidence<-evidence
    for(cond_i in conditions.labels){
      new_evidence[,cond_i]<-NA
    }
    for(i in 1:length(conditions.labels)){
      tmpvec<-new_evidence$Spectrum.File %in% paste(unlist(conditions.labels.Modifications[i]),".raw",sep="")
      new_evidence[tmpvec,conditions.labels[i]]<-new_evidence[tmpvec,"oldIntensity"]
    }
    old_evidence<-evidence
    evidence<-new_evidence
  }
  
  
  if(!LabelFree){ 
    levellog("Performing peptide-level filtering of single-channel quantitations ...");
  	#Peptide-level filtering of single-channel quantitations (the rest of the channels have the same intensity, i.e. noise level)
    #There is no point in doing this for label-free data, since they are by definition "single-channel" quantitations
  	if(ProteinQuantitation && nConditions == 2){ #single-ratio case: implicit (user doesn't have to specify it), so we should protect from noise-level one-to-ones
  	  evidence<-evidence[!(evidence[,conditions.labels[1]]==evidence[,conditions.labels[2]]), ]
  	}else if(ProteinQuantitation && filterL && filterL_lvl){ #multiple-ratio case: explicit, user have to choose to discard peptides where a certain pair of channels have noise-level intensity
      labelTruth<-TRUE
      rest_labels<-conditions.labels[which(conditions.labels != filterL_lbl)]
      for(lbl_i in rest_labels[-1]){
        labelTruth<-labelTruth & (evidence[,c(lbl_i)]==evidence[,c(rest_labels[1])])
      }
      if(length(rest_labels[-1]) > 0){
        evidence<-evidence[!labelTruth, ]
      }
    }
  }
  
  #====================

  if(ProteinQuantitation){  
	levellog("Calculating protein intesity per label and per replicate ...");
    #Calculate protein intesity per label and per replicate
      subtotals2<-ddply(evidence,c("Spectrum.File","Protein.IDs"),function(x){
        ids<-paste(x$id,collapse=";")
        cond_sums<-c()
        for(cond_i in conditions.labels){
          cond_sums<-cbind(cond_sums, sum(x[,cond_i],na.rm=T))
        }
        cond_counts<-sum(!is.na(x[,conditions.labels[1]])) # It doesn't matter which label is used for counting. This is the ratio counts, and since PD replaces missing signlas with the minimum intensity, there will always be a ratio, and the number of ratios will be equal to the number of records here. Unwanted ratios, such as those that are equal to 1 and should not be considered/counted are filtered above, at the peptide-filtering stage if chosen by the user (recommended)
        ret<-data.frame(cbind(cond_sums,cond_counts))
        colnames(ret)<-c(conditions.labels,"Ratio.counts")
        nUniqueSequences<-length(unique(x$Unique.Sequence.ID))
        ret$uniqueSequences<-nUniqueSequences
        ret$Evidence.IDs<-ids
        return(ret)
        #data.frame(Evidence.IDs=paste(x$id,collapse=";"),Light=sum(x$Light,na.rm=T),Medium=sum(x$Medium,na.rm=T),Heavy=sum(x$Heavy,na.rm=T),n.Peptides=sum(!is.na(x$Heavy)))
      })    
  }else{
    #Calculate peptide intesity per label and per replicate
	levellog("Calculating peptide intesity per label and per replicate ...");
    #First, We have to remove the labels from the modifications column, because we have to pivot on modifications but not on labels
    modcol<-evidence$Modifications
    for(i in 1:length(conditions.labels)){
      for(mod_i in conditions.labels.Modifications[[i]]){
        if(mod_i != ""){
          modcol<-gsub(paste("[A-Z]+[0-9]+\\(",mod_i,"[; ]*",sep=""),"",modcol)
        }
      }
    }
    modcol<-gsub("^[ ]+","",modcol)
    modcol<-gsub(";[ ]+$","",modcol)
    evidence[,"Modifications.only"]<-modcol

    subtotals2<-ddply(evidence,c("Spectrum.File","Unique.Sequence.ID","Modifications.only"),function(x){
      cond_sums<-c()
      for(cond_i in conditions.labels){
        cond_sums<-cbind(cond_sums, sum(x[,cond_i],na.rm=T))
      }
      cond_counts<-sum(!is.na(x[,conditions.labels[1]])) # It doesn't matter which label is used for counting. This is the ratio counts, and since PD replaces missing signlas with the minimum intensity, there will always be a ratio, and the number of ratios will be equal to the number of records here. Unwanted ratios, such as those that are equal to 1 and should not be considered/counted are filtered above, at the peptide-filtering stage if chosen by the user (recommended)
      ret<-data.frame(cbind(cond_sums,cond_counts))
      colnames(ret)<-c(conditions.labels,"Ratio.counts")
      ret$Evidence.IDs<-paste(x$id,collapse=";")
      ret$Sequence<-toupper(x$Sequence[1])
      ret$Protein.IDs<-x$Protein.IDs[1]
      return(ret)    
      #data.frame(Evidence.IDs=paste(x$id,collapse=";"),Light=sum(x$Light,na.rm=T),Medium=sum(x$Medium,na.rm=T),Heavy=sum(x$Heavy,na.rm=T),n.Peptides=sum(!is.na(x$Heavy)))
    })    
  }
  
	#write.table(file="tmp.txt",subtotals2,row.names=F,sep="\t")

	levellog("Re-arranging data and assigning column names ...");
  #Re-arrange data and assign column names
	if(ProteinQuantitation){
    melted_subtotals<-melt(subtotals2)
	}else{
	  melted_subtotals<-melt(subtotals2,id.vars=c("Spectrum.File", "Modifications.only", "Evidence.IDs", "Sequence", "Protein.IDs", "Unique.Sequence.ID"))
	}

	n_bioreps<-length(which(!duplicated(rep_structure)))/nConditions
	n_techreps<-length(rep_structure)/(n_bioreps*nConditions)
	i_bioreps<-which(!duplicated(rep_structure))[1:n_bioreps]
	
	
  melted_subtotals$Spectrum.File<-factor(melted_subtotals$Spectrum.File)

	rep_desc<-paste(paste("b",rep_structure[1:(length(rep_structure)/nConditions)],sep=""),paste("t",rep(1:n_techreps),sep=""),sep="")
	if(!is.na(rep_order)){
		o<-unlist(lapply(rep_order,function(x)((x-1)*n_techreps+1):(((x-1)*n_techreps+1)+n_techreps-1)))
		rep_desc<-rep_desc[o]
	}
	# For the label-free case: multiplex conditions so we have the same data format downstream, i.e. as with labelled experiments
  if(LabelFree){
    tmp_rep_desc<-c()
    for(i in 1:nConditions){
      tmp_rep_desc[[i]]<-paste("c",i,rep_desc,sep="")
      levels(melted_subtotals$Spectrum.File)[which(levels(melted_subtotals$Spectrum.File) %in% paste(conditions.labels.Modifications[[i]],".raw",sep=""))]<-tmp_rep_desc[[i]]
    }
    rep_desc<-unlist(tmp_rep_desc)
    melted_subtotals$brtr<-NA
    biorep_techrep<-regmatches(levels(melted_subtotals$Spectrum.File), regexpr("b.*", levels(melted_subtotals$Spectrum.File)))
    for(i in 1:length(levels(melted_subtotals$Spectrum.File))){
      lvl_i<-levels(melted_subtotals$Spectrum.File)[i]
      brtr<-biorep_techrep[i]
      melted_subtotals[melted_subtotals$Spectrum.File == lvl_i,"brtr"]<-brtr
    }
    melted_subtotals$brtr<-factor(melted_subtotals$brtr)
  }else{
    levels(melted_subtotals$Spectrum.File)<-rep_desc
  }

	if(ProteinQuantitation){
    if(LabelFree){
      pgroups<-as.data.frame(tapply(melted_subtotals$value,list(Protein.IDs=melted_subtotals$Protein.IDs,RawFile=melted_subtotals$brtr,LMHn=melted_subtotals$variable),function(x) sum(x,na.rm=T)))
    }else{
      pgroups<-as.data.frame(tapply(melted_subtotals$value,list(Protein.IDs=melted_subtotals$Protein.IDs,RawFile=melted_subtotals$Spectrum.File,LMHn=melted_subtotals$variable),function(x) sum(x,na.rm=T)))
    }
	}else{
	  pgroups<-as.data.frame(tapply(melted_subtotals$value,list(Peptide.IDs=paste(melted_subtotals$Sequence," [",melted_subtotals$Modifications.only,"]",sep=""),RawFile=melted_subtotals$Spectrum.File,LMHn=melted_subtotals$variable),function(x) sum(x,na.rm=T)))
	}

	for(cond_i in conditions.labels){
	  colnames(pgroups)[grep(paste("\\.",cond_i,"$",sep=""),colnames(pgroups))]<-sub(paste("(.*)\\.",cond_i,sep=""),paste("Intensity.",cond_i,".\\1",sep=""),colnames(pgroups)[grep(paste("\\.",cond_i,"$",sep=""),colnames(pgroups))])
	}  
  
	pgroups[,paste(quantitated_items_lbl,".IDs",sep="")]<-row.names(pgroups)

	#Collect peptide records for each protein 
	if(keepEvidenceIDs && ProteinQuantitation)
	{
	  all_evidence_ids<-ddply(subtotals2,c("Protein.IDs"),function(x) data.frame(Evidence.IDs=paste(x$Evidence.IDs,collapse=";")))
		pgroups<-merge(pgroups,all_evidence_ids,by=paste(quantitated_items_lbl,".IDs",sep=""),all.x=T)
	}

  if(ProteinQuantitation){
	  pgroups<-pgroups[,!(colnames(pgroups) %in% c("Protein.Descriptions"))]
  }
	pgroups$time.point<-time.point
	if(!ProteinQuantitation){
	  subtotals2$Peptide.IDs<-paste(subtotals2$Sequence," [",subtotals2$Modifications.only,"]",sep="")
	  pgroups<-merge(pgroups,subtotals2[!duplicated(subtotals2$Peptide.IDs),c("Peptide.IDs","Protein.IDs")],by="Peptide.IDs",all.x=T)
	  # Remove peptides that we don't want to quantify (conditions.Mod is not empty means the user wants to do quantification on peptides with certain modifications only)
    if(length(conditions.Mods)>0){
      for(i in 1:length(conditions.Mods)){
        for(mod_i_mods in conditions.Mods.Modifications[[i]]){
          pgroups<-pgroups[grepl(mod_i_mods,pgroups$Peptide.IDs),]
        }
	    }
	  }
    #write.table(file="tmp.txt",merge(pgroups,subtotals2[!duplicated(subtotals2$Peptide.IDs),c("Peptide.IDs","Protein.IDs")],by="Peptide.IDs",all.x=T),row.names=F,sep="\t")
	}
  pgroups<-merge(pgroups,subtotals,by="Protein.IDs",all.x=T)

  #If enabled, do filter out proteins based on percentage labeling for the desired label
  if(ProteinQuantitation && filterL && !filterL_lvl){
	levellog("Filter out proteins based on percentage labeling for the desited label ...");
    fl<-paste(filterL_lbl,"p",sep="")
    pgroups<-pgroups[pgroups[,c(fl)]<100,]
  }
  
  if(ProteinQuantitation){
	  cat(paste("read.pgroups_v2_PD: Quantifiable proteins: ",nrow(pgroups)," (",time.point,")\n",sep=""))
  }else{
    cat(paste("read.pgroups_v2_PD: Quantifiable peptides: ",nrow(pgroups)," (",time.point,")\n",sep=""))
  }
	
	row.names(pgroups)<-pgroups[,paste(quantitated_items_lbl,".IDs",sep="")]
	
	levellog("",change=-1)
	return(pgroups)
}

# VALIDATION HELP
# For MaxQuant only, for PD this can be done much better through its GUI
getValidationData<-function(pgroups_fname,diffexp_fname,evidence_fname,output_fname){
	diffexp<-read.table(diffexp_fname, header = T, sep = "\t",quote='',stringsAsFactors=F,comment.char = "")
	protein_groups<-read.table(pgroups_fname, header = T, sep = "\t",quote='',stringsAsFactors=F,comment.char = "")
	evidence<-read.table(evidence_fname, header = T, sep = "\t",quote='',stringsAsFactors=F,comment.char = "")
	
	colnames(diffexp)<-gsub("^X.","",colnames(diffexp))
	colnames(diffexp)<-gsub(".$","",colnames(diffexp))

	protein_groups$Protein.IDs<-paste(sub("^([^;]*).*","\\1",protein_groups$Protein.names)," [",sub("^([^;]*).*","\\1",protein_groups$Gene.names)," ...] [",sub("^([^;]*).*","\\1",protein_groups$Protein.IDs)," ...]",sep="")
	protein_groups$Protein.IDs<-gsub("\"","",protein_groups$Protein.IDs)
	diffexp$Protein.IDs<-gsub("\"","",diffexp$Protein.IDs)
	protein_groups<-protein_groups[protein_groups$Protein.IDs %in% diffexp$Protein.IDs,]
	diffexp_evidence<-as.data.frame(do.call(rbind,apply(protein_groups,1,function(x) cbind(x["Protein.IDs"],unlist(strsplit(x["Evidence.IDs"],";"))))),stringsAsFactors=F)
	colnames(diffexp_evidence)<-c("Protein.IDs","Evidence.ID")
	diffexp_evidence$Protein.IDs<-factor(diffexp_evidence$Protein.IDs)
	diffexp_evidence$Evidence.ID<-as.numeric(diffexp_evidence$Evidence.ID)
	evidence<-evidence[,c("id","Sequence","Labeling.State","Raw.file","MS.MS.Scan.Number","m.z","Charge","Mass.Error..ppm.","K.Count","R.Count")]
	colnames(evidence)<-c("Evidence.ID","Sequence","Labeling.State","Raw.file","MS.MS.Scan.Number","m.z","Charge","Mass.Error.ppm.","K.Count","R.Count")
	diffexp_evidence<-merge(diffexp_evidence,diffexp,by="Protein.IDs",all.x=T)
	diffexp_evidence<-merge(diffexp_evidence,evidence,by="Evidence.ID",all.x=T)
	write.table(diffexp_evidence,file=output_fname,sep="\t",row.names=F)
}

#getValidationData(pgroups_fname="QuaNCATrev_IV_4h_proteinGroups.txt",
#diffexp_fname="QuaNCAT-rev_IV_4h_HM-all-2reps_diffexp_4h.txt",
#evidence_fname="QuaNCATrev_IV_4h_evidence.txt",
#output_fname="QuaNCATrev_IV_4h_HM-all-2reps_diffexpevidence.txt")

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

#getProteinPeptideData_2reps_filter(
#"QuaNCATrev_IV_2h_proteinGroups.txt",
#"QuaNCATrev_IV_2h_evidence.txt",
#"QuaNCATrev_IV_2h_HM-all-2reps_evidence.txt",
#"2h",
#filterL=T,
#linkLimmaout=T,
#limma_outfname="QuaNCAT-rev_IV_2h_HM-all-2reps_limmaout_2h.txt")

#GLOBAL variables

duplicateCorrelation_trim<-0.15 # use 0.22 for "bad" datasets (too many missing values)


#
paramssetfromGUI<-F
getParamsViaGUI <- function(){
	base <<- tktoplevel()
	#tkwm.deiconify(base)
	#tkgrab.set(base)
	#tkfocus(base)
	tkwm.title(base,'MS-based proteomics data differential expression analysis')
	paramstkframe <- tkframe(base)

	# create tcl variables to associate with the 
	# entry fields -- to include starting values
	# replace the '' with the desired value
	time.point_ <- tclVar('')
	PDdata_ <- tclVar('0')
	outputFigsPrefix_ <- tclVar('')
	filterL_ <- tclVar('0')
	filterL_lvl_ <- tclVar('1')
	filterL_lbl_ <- tclVar(conditions.labels[1])
	pgroups_fname_ <- tclVar('')
	evidence_fname_ <- tclVar('')
	mqValidation_ <- tclVar('0')
	rep_order_ <- tclVar('')
  techreps_ <- tclVar('3')
	bioreps_ <- tclVar('3')
	ProteinQuantitation_ <- tclVar('1')

	tkLabelWidth<-40
	tkLabelWidthFac<-0.85
	tkLabelWidth2<-as.integer(tkLabelWidth*tkLabelWidthFac)
	pgroupsentry<-tklabel(paramstkframe,anchor='w', width=tkLabelWidth,text='',relief='ridge')
	tkgrid(tklabel(paramstkframe,anchor='w', text='Protein file',width=tkLabelWidth2),row=0,column=0)
	tkgrid(pgroupsentry,row=0,column=1)
	
	evidenceentry<-tklabel(paramstkframe,anchor='w', width=tkLabelWidth,text='',relief='ridge')
	tkgrid(tklabel(paramstkframe,anchor='w', text='Peptide file',width=tkLabelWidth2),row=1,column=0)
	tkgrid(evidenceentry,row=1,column=1)  
	tkgrid(tklabel(paramstkframe,anchor='w', text='Proteome Discoverer quantitation ?',width=tkLabelWidth2),row=2,column=0)
  tkgrid(tkcheckbutton(paramstkframe,variable=PDdata_,command=function(...){
    if(as.numeric(tclvalue(PDdata_))==1){
      tkconfigure(validationlabel, state="disabled");
      tkconfigure(validationcheck, state="disabled");
      tkconfigure(bioorderlabel, state="normal");
      tkconfigure(bioorderentry, state="normal");
      tkconfigure(techrepslabel, state="normal");
      tkconfigure(techrepsentry, state="normal");
      tkconfigure(biorepslabel, state="normal");
      tkconfigure(biorepsentry, state="normal");   
    }else{
      tkconfigure(validationlabel, state="normal");
      tkconfigure(validationcheck, state="normal");      
      tkconfigure(bioorderlabel, state="disabled");
      tkconfigure(bioorderentry, state="disabled");      
      tkconfigure(techrepslabel, state="disabled");
      tkconfigure(techrepsentry, state="disabled");
      tkconfigure(biorepslabel, state="disabled");
      tkconfigure(biorepsentry, state="disabled");   
    }
	}),row=2,column=1)
	
	tkgrid(tklabel(paramstkframe,anchor='w', text='Timepoint [text]',width=tkLabelWidth2),row=3,column=0)
	tkgrid(tkentry(paramstkframe,width=tkLabelWidth,textvariable=time.point_),row=3,column=1)
	
	tkgrid(tklabel(paramstkframe,anchor='w', text='Output figure file prefix [text]',width=tkLabelWidth2),row=4,column=0)
  tkgrid(tkentry(paramstkframe,width=tkLabelWidth,textvariable=outputFigsPrefix_),row=4,column=1)
	
  dslbqlbl<-tklabel(paramstkframe,anchor='w', text='Disregard single-label-based quantitation ?',width=tkLabelWidth2)
  tkgrid(dslbqlbl,row=5,column=0)
	dslbchk<-tkcheckbutton(paramstkframe,width=tkLabelWidth,variable=filterL_,command=function(...){
	  st<-"disabled"
    if(as.numeric(tclvalue(filterL_))==1){
      st<-"normal"
	  }
    handles<-c(filterL_rb, filterL_rb_lbl, filterL_lvl_rb, filterL_lvl_rb_lbl)
	  for(h in handles){
	    tkconfigure(h, state=st)  
	  }
	})
	tkgrid(dslbchk,row=5,column=1)
	if(nConditions < 3){
	  tkconfigure(dslbqlbl, state="disabled");
	  tkconfigure(dslbchk, state="disabled");
	}
  
  filterL_lvl_rb<<-list()
	filterL_lvl_rb_lbl<<-list()
  
	rb<-tkradiobutton(paramstkframe)
	tkconfigure(rb,variable=filterL_lvl_,value=0,state="disabled")
	rb_lbl<-tklabel(paramstkframe,text="Sequence-based",state="disabled")
	tkgrid(rb_lbl,row=6,column=1)
	tkgrid(rb,row=6,column=2)
	filterL_lvl_rb[[length(filterL_lvl_rb)+1]]<<-rb
	filterL_lvl_rb_lbl[[length(filterL_lvl_rb_lbl)+1]]<<-rb_lbl
  
	rb<-tkradiobutton(paramstkframe)
	tkconfigure(rb,variable=filterL_lvl_,value=1,state="disabled")
	rb_lbl<-tklabel(paramstkframe,text="Intensity-based",state="disabled")
	tkgrid(rb_lbl,row=7,column=1)
	tkgrid(rb,row=7,column=2)
	filterL_lvl_rb[[length(filterL_lvl_rb)+1]]<<-rb
	filterL_lvl_rb_lbl[[length(filterL_lvl_rb_lbl)+1]]<<-rb_lbl  
  
  tmp_row<-8
  filterL_rb<<-list()
	filterL_rb_lbl<<-list()
  for(cond_i in conditions.labels){
    rb<-tkradiobutton(paramstkframe)
    tkconfigure(rb,variable=filterL_lbl_,value=cond_i,state="disabled")
    rb_lbl<-tklabel(paramstkframe,text=cond_i,state="disabled")
    tkgrid(rb_lbl,row=tmp_row,column=1)
    tkgrid(rb,row=tmp_row,column=2)
    filterL_rb[[length(filterL_rb)+1]]<<-rb
    filterL_rb_lbl[[length(filterL_rb_lbl)+1]]<<-rb_lbl
    tmp_row<-tmp_row+1
  }

	tkgrid(tklabel(paramstkframe,anchor='w', text='Timepoint [text]',width=tkLabelWidth2),row=tmp_row,column=0)
	tkgrid(tkentry(paramstkframe,width=tkLabelWidth,textvariable=time.point_),row=tmp_row,column=1)
	tmp_row<-tmp_row+1
	tkgrid(tklabel(paramstkframe,anchor='w', text='Output figure file prefix [text]',width=tkLabelWidth2),row=tmp_row,column=0)
	tkgrid(tkentry(paramstkframe,width=tkLabelWidth,textvariable=outputFigsPrefix_),row=tmp_row,column=1)
	tmp_row<-tmp_row+1
  validationlabel<<-tklabel(paramstkframe,anchor='w', text='Generate validation data ? (MQ)',width=tkLabelWidth2)
  validationcheck<<-tkcheckbutton(paramstkframe,width=tkLabelWidth,variable=mqValidation_)
	tkgrid(validationlabel,row=tmp_row,column=0)
  tkgrid(validationcheck,row=tmp_row,column=1)
	tmp_row<-tmp_row+1
	biorepslabel<<-tklabel(paramstkframe,anchor='w', text='Samples (replicates) [number]',width=tkLabelWidth2,state='disabled')
	biorepsentry<<-tkentry(paramstkframe,width=tkLabelWidth,textvariable=bioreps_,state='disabled')
	tkgrid(biorepslabel,row=tmp_row,column=0)
	tkgrid(biorepsentry,row=tmp_row,column=1)
	tmp_row<-tmp_row+1
	bioorderlabel<<-tklabel(paramstkframe,anchor='w', text='Acquisition order [space-separated numbers]',width=tkLabelWidth2,state='disabled')
	bioorderentry<<-tkentry(paramstkframe,width=tkLabelWidth,textvariable=rep_order_,state='disabled')
	tkgrid(bioorderlabel,row=tmp_row,column=0)
	tkgrid(bioorderentry,row=tmp_row,column=1)
	tmp_row<-tmp_row+1
  techrepslabel<<-tklabel(paramstkframe,anchor='w', text='LC-MS/MS runs per sample [number]',width=tkLabelWidth2,state='disabled')
	techrepsentry<<-tkentry(paramstkframe,width=tkLabelWidth,textvariable=techreps_,state='disabled')
	tkgrid(techrepslabel,row=tmp_row,column=0)
  tkgrid(techrepsentry,row=tmp_row,column=1)
	tmp_row<-tmp_row+1
	tkgrid(tklabel(paramstkframe,anchor='w', text='Protein quantitation ?',width=tkLabelWidth2),row=tmp_row,column=0)
	tkgrid(tkcheckbutton(paramstkframe,variable=ProteinQuantitation_),row=tmp_row,column=1)
	tmp_row<-tmp_row+1
  
	tkbind(pgroupsentry,'<Button-1>',function(...){
		chosenfile<-tk_choose.files(multi=F, caption = "Select protein file", filter=matrix(c("Text", ".txt"), 1, 2, byrow = TRUE));
		if(length(chosenfile)>0){
			working_directory<<-dirname(chosenfile);
			setwd(working_directory);
			chosenfile<-basename(chosenfile);
			tkconfigure(pgroupsentry,text=chosenfile);
			tclvalue(pgroups_fname_)<-chosenfile;
			tkraise(base);   
			}
	})
	tkbind(evidenceentry,'<Button-1>',function(...){
		chosenfile<-tk_choose.files(multi=F, caption = "Select peptide file", filter=matrix(c("Text", ".txt"), 1, 2, byrow = TRUE));
		if(length(chosenfile)>0){
			working_directory<<-dirname(chosenfile);
			setwd(working_directory);
			chosenfile<-basename(chosenfile);
			tkconfigure(evidenceentry,text=chosenfile);
			tclvalue(evidence_fname_)<-chosenfile;
			tkraise(base);
		}
	})

	tkpack(paramstkframe,side='top', pady=c(10,10))
	controlbtnstkframe = tkframe(base)
	tkgrid(tkbutton(controlbtnstkframe,text='Apply & Run',command=function(...){
		time.point <<- tclvalue(time.point_);
		PDdata <<- as.numeric(tclvalue(PDdata_))==1;
		ProteinQuantitation <<- as.numeric(tclvalue(ProteinQuantitation_))==1;
    if(ProteinQuantitation){
      quantitated_items_lbl<<-"Protein"
    }else{
      quantitated_items_lbl<<-"Peptide"
    }
		outputFigsPrefix <<- tclvalue(outputFigsPrefix_);
		filterL <<- as.numeric(tclvalue(filterL_))==1;
		filterL_lvl <<- as.numeric(tclvalue(filterL_lvl_))==1;
		filterL_lbl <<- as.character(tclvalue(filterL_lbl_));
		pgroups_fname <<- tclvalue(pgroups_fname_);
		evidence_fname <<- tclvalue(evidence_fname_);
		mqValidation <<- as.numeric(tclvalue(mqValidation_))==1;
		rep_order <<- as.numeric(unlist(strsplit(tclvalue(rep_order_)," ")));
		techreps <<- as.numeric(unlist(strsplit(tclvalue(techreps_)," ")));
		bioreps <<- as.numeric(unlist(strsplit(tclvalue(bioreps_)," ")));
		if(length(rep_order) == 0){
		  rep_order<<-NA
		}
		if(length(techreps_) == 0){
		  techreps<<-3
		}
		if(length(bioreps_) == 0){
		  bioreps<<-3
		}
		paramssetfromGUI <<- T;
		tkgrab.release(base); tkdestroy(base)
	}),tkbutton(controlbtnstkframe,text='Cancel',command=function(...){ 
		tkgrab.release(base); tkdestroy(base)
	}))	
	tkpack(controlbtnstkframe,side='bottom', pady=c(10,10))

  #handles<<-c(validationlabel, validationcheck, bioorderlabel, bioorderentry, techrepslabel, techrepsentry, biorepslabel, biorepsentry, filterL_rb, filterL_rb_lbl, filterL_lvl_rb, filterL_lvl_rb_lbl) 
  #for(h in handles){
  #  print(h)
  #  tkconfigure(h, state="disabled")
  #}
	#tkraise(base)
	tkwait.window(base)
}

addLabel<-function(lblname, lbl.Modifications){
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
        lbl.Modifications<-c(lbl.Modifications[unmod_idx],paste("Label:",lbl.Modifications[-unmod_idx],sep=""))    
      }
    }else{
      lbl.Modifications<-paste(lbl.Modifications,"\\)",sep="")
      lbl.Modifications<-paste("Label:",lbl.Modifications,sep="")  
    }
  }else{
    labeltxt <- "condition";
  }
  lblname_i<-which(grepl(paste("^",lblname,"$",sep=""),conditions.labels))
  if(length(lblname_i) != 0){
    cat(paste("addLabel: Error adding ",labeltxt," '",lblname,"': An existing ",labeltxt," with name '",lblname,"' (specification: ",paste(unlist(conditions.labels.Modifications[lblname_i]),collapse=", "),") already exists. Please try a different name.",sep=""))
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
}
removeLabel<-function(lblname){
  lblname_i<-which(grepl(paste("^",lblname,"$",sep=""),conditions.labels))
  if(length(lblname_i) != 0){
    conditions.labels<<-conditions.labels[-lblname_i]
    conditions.labels.Modifications<<-conditions.labels.Modifications[-lblname_i]
  }
  nConditions<<-length(conditions.labels)
}
clearLabels<-function(){
  conditions.labels<<-c()
  conditions.labels.Modifications<<-list()
  nConditions<<-length(conditions.labels)
}

addMod<-function(modname, mod.Modifications){
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
    cat(paste("addMod: Error adding modification '",modname,"': An existing modification with name '",modname,"' (specification: ",paste(unlist(conditions.Mods.Modifications[modname_i]),collapse=", "),") already exists. Please try a different name.",sep=""))
    return(FALSE)
  }
  i<-1
  for(mod_i in conditions.Mods.Modifications){
    for(mod_i in mod.Modifications){
      mod_i_matches<-which(grepl(paste("^",mod_i,"$",sep=""),mod_i))
      if(length(mod_i_matches) != 0){
        cat(paste("addMod: Error adding modification '",modname,"': Existing modification with name '",conditions.Mods[i],"' has an identical specification (",mod_i,").",sep=""))
        return(FALSE)
      }
    }
    i<-i+1
  }
  conditions.Mods<<-c(conditions.Mods, modname)
  j<-length(conditions.Mods.Modifications)+1
  conditions.Mods.Modifications[[j]]<<-mod.Modifications
  nMods<<-length(conditions.Mods)
}
removeMod<-function(modname){
  modname_i<-which(grepl(paste("^",modname,"$",sep=""),conditions.Mods))
  if(length(modname_i) != 0){
    conditions.Mods<<-conditions.Mods[-modname_i]
    conditions.Mods.Modifications<<-conditions.Mods.Modifications[-modname_i]
  }
  nMods<<-length(conditions.Mods)
}
clearMods<-function(){
  conditions.Mods<<-c()
  conditions.Mods.Modifications<<-list()
  nMods<<-length(conditions.Mods)
}

unlabeled_peptide_regex<-"^Label:$"
clearLabels()
clearMods()
paramssetfromGUI<-F
working_directory<-getwd()
limma_output<-"msdiffexp_out"
LabelFree<-F
source("MSdiffexp_definitions.R")

if(GUI){
	getParamsViaGUI()
}

perform_analysis<-function(){
  levellog("",change=1)
  setwd(working_directory)
  rep_structure<<-rep(1:(bioreps*nConditions),each=techreps)
  if(ProteinQuantitation){
    quantitated_items_lbl<<-"Protein"
  }else{
    quantitated_items_lbl<<-"Peptide"
  }
  if(!file.exists(limma_output)){dir.create(limma_output)}
  levellog("Removing double quotes from input data ...")
  tmpdata<-gsub("\"", "", readLines(evidence_fname))
  evidence_fname_cleaned<-file(evidence_fname, open="w")
  writeLines(tmpdata, con=evidence_fname_cleaned)
  close(evidence_fname_cleaned)
  tmpdata<-gsub("\"", "", readLines(pgroups_fname))
  pgroups_fname_cleaned<-file(pgroups_fname, open="w")
  writeLines(tmpdata, con=pgroups_fname_cleaned)
  close(pgroups_fname_cleaned)
  levellog("Reading input data ...")
  if(PDdata){
    #If label definitions are the same, it will be interpreted that the information is not stored in the "modifications" column of the PD PSMs file, which occurs when the labelling is at the MS/MS level.
    #Thus, if the above condition is true, we have multiplexion at the MS/MS level (e.g. iTRAQ, TMTs etc) and the "Label:" part of the regular expression has to be removed from the labels definitions
    if(length(which(conditions.labels.Modifications[[1]]==conditions.labels.Modifications)) == length(conditions.labels.Modifications)){
      conditions.labels.Modifications<-lapply(conditions.labels.Modifications, function(x){sub("Label:","",x)})
    }  
    protein_groups<<-read.pgroups_v2_PD(pgroups_fname,evidence_fname,time.point,rep_structure,keepEvidenceIDs=T,rep_order=rep_order)
    do_generate_Venn3_data_quant_filter_2reps_PD(protein_groups,time.point,evidence_fname,rep_structure,outputFigsPrefix=outputFigsPrefix,rep_order=rep_order)
  }else{
    protein_groups<<-read.pgroups_v2(pgroups_fname,evidence_fname,time.point)
    do_generate_Venn3_data_quant_filter_2reps(protein_groups,time.point,outputFigsPrefix=outputFigsPrefix)
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
  results<-do_analyse_all_2reps_v2(protein_groups,time.point,exp_design_fname,rep_structure,exportFormat="pdf",outputFigsPrefix=outputFigsPrefix)
  levellog("Data analysis finished.")
  if(!PDdata & mqValidation){
    getValidationData(pgroups_fname=pgroups_fname,
                      diffexp_fname=paste(outputFigsPrefix,"-all-2reps_diffexp_",time.point,".txt",sep=""),
                      evidence_fname=evidence_fname,
                      output_fname=paste(outputFigsPrefix,"-all-2reps_diffexpevidence_",time.point,".txt",sep=""))
    
    getProteinPeptideData_2reps_filter(
      pgroups_fname,
      evidence_fname,
      paste(outputFigsPrefix,"-all-2reps_evidence.txt",sep=""),
      time.point,
      filterL=filterL,
      linkLimmaout=T,
      limma_outfname=paste(outputFigsPrefix,"-all-2reps_limmaout_",time.point,".txt",sep=""))			
  }
  levellog("",change=-1)
}

#================ PRODUCTION ===============

if(GUI & !paramssetfromGUI){
  
}else{
  perform_analysis()
}


#================ TESTING ===============
# nParams<-4
# nCombs<-2^nParams
# combs_to_test<-(nCombs-1):0
# combs_to_test<-combs_to_test[1]
# for(ci in combs_to_test){
#   #Binary representation of ci reveals the parameter combination, because each param is a binary variable
#   #Assumes that integer is a 32-bit variable (it doesn't have to do with machine architecture, apparently)
#   paramset<-as.numeric(unlist(strsplit(substring(paste(rev(as.integer(intToBits(ci))), collapse=""),32-nParams+1),"")))
#   PDdata<-paramset[1]
#   ProteinQuantitation<-paramset[2]
#   filterL<-paramset[3]
#   filterL_lvl<-paramset[4]
#   
#   if(PDdata){
#     pgroups_fname <- "PD_psms.txt"
#     evidence_fname <- pgroups_fname
#   }else{
#     pgroups_fname <- "MQ_proteingroups.txt"
#     evidence_fname <- "MQ_evidence.txt"
#     fname<-pgroups_fname  
#   }  
#   cat(paste("Executing with paramset #",(ci+1)," {PDdata, ProteinQuantitation, filterL, filterL_lvl}={",PDdata,",", ProteinQuantitation,",", filterL,",", filterL_lvl, "} ...\n",sep=""))
#   #cat(paste("Execute with paramset #",(ci+1)," {PDdata, ProteinQuantitation, filterL, filterL_lvl}={",PDdata,",", ProteinQuantitation,",", filterL,",", filterL_lvl, "} ?[y/n]",sep=""))
#   #ans <- readline("")
#   #if (substr(ans, 1, 1) != "n"){
#     outputFigsPrefix <- paste(c("TEST-",(ci+1),"__",PDdata, ProteinQuantitation, filterL, filterL_lvl),sep="",collapse="_")
#     perform_analysis()
#     file.copy(c("curr_exp_design.txt"), limma_output)
#     setwd(limma_output)
#     dir.create(outputFigsPrefix)
#     files_produced<-list.files(".", "\\.(pdf|txt)$", full.names = TRUE)
#     file.copy(files_produced, outputFigsPrefix)
#     file.remove(files_produced)
#     setwd("..")
#   #}  
# }
