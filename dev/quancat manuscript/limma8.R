# Based on a Progenesis LC-MS Limma Analysis script

# Load the limma library so we can use it
# Can be installed by running following commands in R
#
# source("http://www.bioconductor.org/biocLite.R")
# biocLite("limma")
# biocLite("statmod")
# -----
# Install ggplot2 for nicer plots
# install.packages("ggplot2")
# -----
# install.packages("plyr") 
#
#

library(limma)
library(reshape)
library(plyr)
library(ggplot2)

# FROM: http://musicroamer.com/blog/2011/01/16/r-tips-and-tricks-modified-pairs-plot/
# Scatterplot matrix functionality

panel.cor.scale <- function(x, y, digits=2, prefix="", cex.cor)
{
usr <- par("usr"); on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r = (cor(x, y,use="pairwise"))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, cex = cex * abs(r))
}


panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
usr <- par("usr"); on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r = (cor(x, y,use="pairwise"))
txt <- format(c(r, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, cex = cex )
}

panel.hist.breaks<--12:12

panel.hist <- function(x, ...)
{
usr <- par("usr"); on.exit(par(usr))
par(usr = c(usr[1:2], 0, 1.5) )
#h <- hist(x, plot = FALSE)
h <- hist(x, breaks=panel.hist.breaks,plot = FALSE)
breaks <- h$breaks; nB <- length(breaks)
y <- h$counts; y <- y/max(y)
rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

# FROM: http://www-personal.umich.edu/~ladamic/presentations/Rtutorial/Rtutorial.R
panel.lmline = function (x, y, col = par("col"), bg = NA, pch = par("pch"), cex = 1, col.smooth = "red", ...){
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) 
       abline(lm(y[ok] ~ x[ok]), 
           col = col.smooth, ...)
}

pairs.panels <- function (x,y,smooth=TRUE,scale=FALSE,lm=FALSE)
{
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



# NOT CURRENTLY USED
# FROM: http://wiki.stdout.org/rcookbook/Graphs/Multiple%20graphs%20on%20one%20page%20(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, by.row=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
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
# calculate mean, sd and N of ratios between available channels, i.e. !NA (possibly mixed replicates, arbitrary order)
# Hcols_idxs, Mcols_idxs the column indexes of Heavy and Medium channels intensities respectively
# x a limma-results data.frame row
calcRowStats<-function(x,Hcols_idxs,Mcols_idxs){
	tmp1<-as.numeric(x[Hcols_idxs])
	tmp2<-as.numeric(x[Mcols_idxs])
	minN<-min(length(which(!is.na(tmp1))),length(which(!is.na(tmp2))))
	#ratios<-tmp1[which(!is.na(tmp1))[1:minN]]-tmp2[which(!is.na(tmp2))[1:minN]]
	ratios<-(tmp1-tmp2)

	m<-mean(ratios,na.rm=T)
	std<-sd(ratios,na.rm=T)
	#TODO: generalize for any replicate strucutre
	#n_techreps<-length(rep_structure)/length(which(!duplicated(rep_structure)))
	#i_bioreps<-which(!duplicated(rep_structure))[1:n_techreps]
	#n_bioreps<-length(i_bioreps)
	#rep_structure_idxs<-lapply(i_bioreps,function(x) seq(from=x,to=x+n_techreps-1))
	
	#log2 ratio CV for each biorep ?, calculated according to "CV for log-normally distributed data" formula: http://en.wikipedia.org/wiki/Coefficient_of_variation
	#biorep_cv<-do.call(cbind, lapply(rep_structure_idxs,function(x) sqrt(sd((tmp2-tmp1)[x],na.rm=T)^2-1))) 
	
	#log2 ratio CV for each techrep ?
	#rep_structure_idxs_2<-lapply(1:n_bioreps,function(x) i_bioreps+x-1)	
	#biorep_cv<-do.call(cbind, lapply(rep_structure_idxs_2,function(x) sqrt(sd((tmp2-tmp1)[x],na.rm=T)^2-1))) 

	#N<-length(ratios)
	N<-length(which(!is.na(ratios)|ratios==0))
	#avg.I<-log2(mean(2^c(tmp1[which(!is.na(tmp1))[1:minN]])+2^c(tmp2[which(!is.na(tmp2))[1:minN]])))
	avg.I<-log2(mean(sapply(1:length(Hcols_idxs),function(x) sum(c(2^tmp1[x],2^tmp2[x]),na.rm=T)),na.rm=T))
	#return(c(m,std,N,avg.I,ratios,rep(NA,length(Hcols_idxs)-N)))
	return(c(m,std,N,avg.I,ratios))
}


# Produces S-plot, Volcano plot, MA plot and Scatterplot (matrix). Called by do_limma_analysis subroutine.
do_results_plots<-function(norm.median.intensities,time.point,exportFormat="pdf",outputFigsPrefix=""){
	results<-read.table(paste(outputFigsPrefix,"_STIMULATED-CTRL_",time.point,".txt",sep=""), header = T, sep = "\t",quote='',stringsAsFactors=F)
	#results$longID<-results$ID
	#results$ID<-sub("^([^;]*).*","\\1",results$ID)
	rownames(results)<-results$ID
	tmp<-as.data.frame(t(norm.median.intensities))
	rownames(tmp)<-colnames(norm.median.intensities)
	
	nsamples<-length(colnames(tmp))/2
	colnames(tmp)<-apply(data.frame(cbind(rep(c("M","H"),each=nsamples),rep(1:nsamples))),1,function(x) paste(x['X1'],x['X2'],sep=""))
	results<-cbind(results,tmp)
	results$N<-apply(results[,colnames(tmp)],1,function(x)(nsamples*2)-length(which(is.na(x))))

	results<-results[!is.na(results$p.value.adj),]
	ndiffexp<-nrow(results[results$p.value.adj<=0.05,])
	#limma volcano
	#volcanoplot(fit2, highlight=as.character(ndiffexp))
	#title(main="Log odds vs fold change STIMULATED-CTRL", sub="Significant proteins highlighted")
	
	#my plots

	# 1 - volcano - log odds vs log ratio
	if(exportFormat == "pdf"){
		pdf(file=paste(outputFigsPrefix,"_volcano_",time.point,".pdf",sep=""),width=10, height=7, family = "Helvetica", pointsize=8)
	}

	Hcols_idxs<-grep("^H",colnames(results))
	Mcols_idxs<-grep("^M",colnames(results))

	d<-data.frame(t(apply(results,1, function(x) calcRowStats(x,Hcols_idxs,Mcols_idxs))))
	colnames(d)<-c("log2.avg.H.M","log2.sd.H.M","log2.N.H.M","log2.avg.I",apply(data.frame(cbind(rep(c("log2.H.M."),each=nsamples),rep(1:nsamples))),1,function(x) paste(x['X1'],x['X2'],sep="")))
	#write.table(file="tmp.txt",cbind(results,d),sep="\t")

	results<-cbind(results,d)

	#sort data frame based on log2.avg.H.M
	results<-results[with(results, order(log2.avg.H.M)),]
	results$ID <- factor(results$ID, levels=unique(as.character(results$ID)))
	results$nID<-1:nrow(results)
	#

	rng<-range(melt(results[,colnames(results)[grep("log2.H.M.",colnames(results))]])[[2]],na.rm=T)
	ratiolim<-ceiling(max(abs(rng)))
	panel.hist.breaks<--ratiolim:ratiolim

	theme_set(theme_bw())
	# customized colorblind-friendly palette from http://wiki.stdout.org/rcookbook/Graphs/Colors%20(ggplot2)/
	cbPalette <- c("#999999", "#D55E00", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#CC79A7")

	p<-ggplot(data=results, aes(x=log2.avg.H.M, y=-log10(p.value.adj), colour=p.value.adj<=0.05)) +
	  geom_point(alpha=0.7, size=1.75) +
	  theme(legend.position = "none", axis.title.y=element_text(vjust=0.2), axis.title.x=element_text(vjust=0), plot.title = element_text(vjust=1.5, lineheight=.8, face="bold")) +
	  xlim(c(-ratiolim, ratiolim)) + ylim(c(0, 6)) + scale_colour_manual(values=cbPalette) +
	  xlab("average log2 H/M") + ylab("-log10 p-value") + ggtitle("P-value vs Fold change") +
	  geom_hline(aes(yintercept=-log10(0.05)), colour="#990000", linetype="dashed") +
	  geom_text(size=2.5, hjust=1, vjust=-0.5,aes(x=-4.2, y=-log10(0.05)), label="P-value=0.05",colour="#990000")# +
	  # geom_text(data=results[results$p.value.adj<=0.05 & abs(results$log2.avg.H.M)>2.9,],size=2.5, hjust=-0.2, vjust=0.2, aes(x=log2.avg.H.M, y=-log10(p.value.adj), label=ID), colour="black")
	print(p)

	if(exportFormat == "emf"){
		savePlot(filename=paste(outputFigsPrefix,"_volcano_",time.point,".emf",sep=""),type="emf")
	}
	dev.off()
	# 2 - value-ordered - log ratio
	if(exportFormat == "pdf"){
		pdf(file=paste(outputFigsPrefix,"_value-ordered-log-ratio_",time.point,".pdf",sep=""),width=10, height=7, family = "Helvetica", pointsize=8)
	}

	p<-ggplot(data=results, aes(x=nID, y=log2.avg.H.M, colour=p.value.adj<=0.05)) +
	  geom_point(alpha=0.7, size=1.5) +
	  geom_errorbar(aes(ymin=log2.avg.H.M-log2.sd.H.M, ymax=log2.avg.H.M+log2.sd.H.M), width=1.5) +
	  theme(legend.position = "none", axis.title.y=element_text(vjust=0.2), axis.title.x=element_text(vjust=0), plot.title = element_text(vjust=1.5, lineheight=.8, face="bold")) +
	  ylim(c(-ratiolim, ratiolim)) + scale_colour_manual(values=cbPalette) +
	  xlab("Protein ID") + ylab("average log2 H/M") + ggtitle("Value-ordered fold change")# +
	  #geom_text(data=results[results$p.value.adj<=0.05 & abs(results$log2.avg.H.M)>2.9,],size=2, hjust=-0.2, vjust=0.2, aes(x=nID, y=log2.avg.H.M, label=ID), colour="black")
	print(p)

	if(exportFormat == "emf"){
		savePlot(filename=paste(outputFigsPrefix,"_value-ordered-log-ratio_",time.point,".emf",sep=""),type="emf")
	}
	dev.off()
	# 3 - MA plot
	if(exportFormat == "pdf"){
		pdf(file=paste(outputFigsPrefix,"_MA_",time.point,".pdf",sep=""),width=10, height=7, family = "Helvetica", pointsize=8)
	}

	p<-ggplot(data=results, aes(x=log2.avg.I, y=log2.avg.H.M, colour=p.value.adj<=0.05)) +
	  geom_point(alpha=0.7, size=1.75) +
	  theme(legend.position = "none", axis.title.y=element_text(vjust=0.2), axis.title.x=element_text(vjust=0), plot.title = element_text(vjust=1.5, lineheight=.8, face="bold")) +
	  ylim(c(-ratiolim, ratiolim)) + scale_colour_manual(values=cbPalette) +
	  xlab("M (average log2 Intensity)") + ylab("A (average log2 H/M)") + ggtitle("MA plot")# +
	  #geom_text(data=results[results$p.value.adj<=0.05 & abs(results$log2.avg.H.M)>2.9,],size=2, hjust=-0.2, vjust=0.2, aes(x=log2.avg.I, y=log2.avg.H.M, label=ID), colour="black")
	print(p)

	if(exportFormat == "emf"){
		savePlot(filename=paste(outputFigsPrefix,"_MA_",time.point,".emf",sep=""),type="emf")
	}
	dev.off()
	# 4 - reproducibility plots & histograms

	allratios<-results[,colnames(results)[grep("log2.H.M.",colnames(results))]]
	colnames(allratios)<-sub("log2.H.M.","log2(H/M) ",colnames(allratios))

	if(exportFormat == "pdf"){
		pdf(file=paste(outputFigsPrefix,"_reproducibility_",time.point,".pdf",sep=""),width=10, height=7, family = "Helvetica", pointsize=8)
	}

	pairs.panels(allratios,scale=T,lm=T)

	if(exportFormat == "emf"){
		savePlot(filename=paste(outputFigsPrefix,"_reproducibility_",time.point,".emf",sep=""),type="emf")
	}
	dev.off()

	#n_techreps<-length(rep_structure)/length(which(!duplicated(rep_structure)))
	#i_bioreps<-which(!duplicated(rep_structure))[1:n_techreps]
	#n_bioreps<-length(i_bioreps)
	#rep_desc<-paste(paste("b",rep_structure[1:(length(rep_structure)/2)],sep=""),paste("t",rep(1:n_techreps),sep=""),sep="")

	#colnames(allratios)<-rep_desc
	
	# write tables and finish	

	write.table(results,file=paste(outputFigsPrefix,"_results_",time.point,".txt",sep=""),sep="\t",col.names=NA)
	print(paste("Quantified proteins: ",nrow(results),", Differentially expressed: ",ndiffexp,sep=""))

	#diffexp<-results[results$p.value.adj <= 0.05,c("p.value.adj","log2.avg.H.M","log2.sd.H.M","log2.N.H.M","log2.avg.I")]
	diffexp<-results[,c("p.value.adj","log2.avg.H.M","log2.sd.H.M","log2.N.H.M","log2.avg.I")]
	diffexp$Protein<-rownames(diffexp)
	colnames(diffexp)<-c("P-value","avg log2 H/M","std log2 H/M","N log2 H/M","avg log2 I","Protein.IDs")
	diffexp<-diffexp[,c("Protein.IDs","avg log2 H/M","std log2 H/M","P-value", "N log2 H/M","avg log2 I")]
	tmp_protein_groups<-protein_groups
	diffexp<-merge(diffexp,tmp_protein_groups[,c("Protein.IDs",sort(colnames(tmp_protein_groups)[grep("Ratio.H.M.count.",colnames(tmp_protein_groups))]))],by="Protein.IDs",all.x=T)
	diffexp$Ratio.H.M.count.total<-rowSums(diffexp[,colnames(diffexp)[grep("Ratio.H.M.count.",colnames(diffexp))]],na.rm=T)
	write.table(diffexp[diffexp$"P-value"<= 0.05,],file=paste(outputFigsPrefix,"_diffexp_",time.point,".txt",sep=""),sep="\t",row.names=F,quote=F)
	write.table(diffexp,file=paste(outputFigsPrefix,"_limmaout_",time.point,".txt",sep=""),sep="\t",row.names=F,quote=F)

	return(results)
}

# Performs the differential expression analysis through limma, after quantile normalization.
do_limma_analysis<-function(working_pgroups,time.point,exp_design_fname,rep_structure,exportFormat="pdf",outputFigsPrefix=""){
	# Read the sample key
	# Assigns sample names (from the data file) to groups
	# Sample order must be the same as the main data file, but excludes technical
	# replicates as we will aggregate into one value per sample.
	
	sample.key <- read.delim(exp_design_fname, header=TRUE,row.names=1)
	
	# Extract protein quantitation columns only from quantitation input file
	# The quantitation data is in columns 10 to 90.

	#prot.intensities <- quantitation[,10:90]
	prot.intensities <- working_pgroups

	# Extract the protein names (imported from the data file) into a
	# separate list for future reference

	prot.names <- rownames(prot.intensities)

	# Take log2 of intensities

	log.intensities  <- log2(prot.intensities)

	if(!file.exists("limma output")){dir.create("limma output")}	
	setwd("limma output")
	write.table(working_pgroups,file=paste(outputFigsPrefix,"_limma-input_proteinGroups.txt",sep=""),sep="\t",row.names = FALSE)

	if(exportFormat == "pdf"){
		pdf(file=paste(outputFigsPrefix,"_limma-graphs_",time.point,".pdf",sep=""),width=10, height=7, family = "Helvetica", pointsize=8)
	}
	# Box plot before normalisation
	boxplot(log.intensities)
	title(main="Intensities Before Normalisation")

	# Perform quantile normalisation

	norm.intensities <- normalizeBetweenArrays(data.matrix(log.intensities), method="quantile");
	#norm.intensities <- cbind(normalizeBetweenArrays(data.matrix(log.intensities[,c(1:(length(rep_structure)/2))]), method="quantile"),normalizeBetweenArrays(data.matrix(log.intensities[,c(((length(rep_structure)/2)+1):length(rep_structure))]), method="quantile"));

	# Box plot after normalisation
	boxplot(norm.intensities)
	title(main="Intensities After Normalisation")

	#---NOT USED---
	# Aggregate technical replicate measurements using median function
	# In this data there are 27 samples with 3 technical replicates each

	#norm.median.intensities <- aggregate(t(as.matrix(norm.intensities)), list(rep(1:27,each=3)), median)[,-1]
	#norm.median.intensities <- aggregate(t(as.matrix(norm.intensities)), list(rep(1:6,each=3)), function(x) median(x))[,-1]
	#norm.median.intensities <- aggregate(t(as.matrix(norm.intensities)), list(rep_structure), median)[,-1]
	#--------------
	
	#norm.median.intensities <- aggregate(t(as.matrix(norm.intensities)), list(1:length(rep_structure)), median)[,-1]
	norm.median.intensities<-as.data.frame(t(as.matrix(norm.intensities)))

	# Assign row names to our aggregated intensities from the sample key

	row.names(norm.median.intensities) <- row.names(sample.key)

	# Setup design matrix
	# This specifies the design of the experiment for limma, replicating
	# the info in the sample key, but representing it in a matrix format

	design <- model.matrix(~0 + factor(sample.key$Category) )
	colnames(design) <- levels(sample.key$Category)

	fit<-""
	n_techreps<-length(rep_structure)/length(which(!duplicated(rep_structure)))
	if(n_techreps > 0){	# if we have technical replicates (duplicated elements in rep_structure)
		# technical replication specification
		corfit <- duplicateCorrelation(t(norm.median.intensities), design=design, ndups=1, block = rep_structure, trim = duplicateCorrelation_trim)
		# Fit the limma model to the data
		# Pass the protein names to limma as the genes option
		fit <- lmFit(t(norm.median.intensities), design, genes=prot.names, block = rep_structure, cor = corfit$consensus)
	}else{
		fit <- lmFit(t(norm.median.intensities), design, genes=prot.names)
	}

	# Setup contrast matrix
	# The contrast matrix specifies what comparisons we want to make between groups.
	# Here there are two groups AS and CONTROL so we compare them. We pass the design
	# matrix as the levels option to the makeContrasts function.

	#contrasts <- makeContrasts(AS-CONTROL, levels=design)
	contrasts <- makeContrasts(STIMULATED-CONTROL, levels=design)

	# Apply contrast matrix and do empirical bayes analysis to get p-values etc.

	fit2 <- contrasts.fit(fit, contrasts)
	fit2 <- eBayes(fit2)

	# Make volcano plot

	#volcanoplot(fit2, highlight="20")
	#title(main="Log odds vs fold change AS-CTRL", sub="Top 20 most significant proteins highlighted")
	#title(main="Log odds vs fold change STIMULATED-CTRL", sub="Top 20 most significant proteins highlighted")

	# Plot a Histogram of co-efficients (log2 ratio)
	#hist(fit2$coefficients,main="Log2 Fold Change AS-CTRL", xlab="Log2 Fold Change", breaks=50 )
	hist(fit2$coefficients,main="Log2 Fold Change STIMULATED-CTRL", xlab="Log2 Fold Change", breaks=50 )
	
	if(exportFormat == "emf"){
		savePlot(filename=paste(outputFigsPrefix,"_limma-graphs_",time.point,"_hist.emf",sep=""),type="emf")
	}

	# Do an MA Plot (mean of log2 intensities vs log2 ratio)
	# Important to inspect the MA plot to ensures that the ratio does not depend on
	# the intensity of the protein. This shouldn't happen if the data was normalised
	# successfully, and should be unusual in SILAC / label-free experiments.

	plotMA(fit2)

	# Add a line at y=0 to the MA plot. The clouds of points should be centred around y=0
	# if most proteins are unchanges and normalisation worked well.

	abline(h=0)

	if(exportFormat == "emf"){
		savePlot(filename=paste(outputFigsPrefix,"_limma-graphs_",time.point,"_MA.emf",sep=""),type="emf")
	}

	dev.off()

	# Output analysis details to file
	# asjust="BH" means adjust the calculated p-values for multiple testing using
	# the Benjamini Hochberg method (FDR)

	#write.fit(fit2, file="AS-CTRL.txt", adjust="BH")
	write.fit(fit2, file=paste(outputFigsPrefix,"_STIMULATED-CTRL_",time.point,".txt",sep=""), adjust="BH")

	results<-do_results_plots(norm.median.intensities, time.point, exportFormat=exportFormat,outputFigsPrefix=outputFigsPrefix)
	setwd("..")
	return(results)
}

# Reads MaxQuant (1.3.0.5) proteinGroups table, discards information not required (for downstream analysis) and returns the table
read.pgroups_v2<-function(fname,time.point,filterL=F,evidence_fname="",keepEvidenceIDs=F){
	if(filterL){
		pgroups<-filter_unlabeled_proteins(fname,evidence_fname)
	}else{
		pgroups<-read.table(fname, header = T, sep = "\t",quote='',stringsAsFactors = FALSE)
	}
	pgroups<-pgroups[pgroups$Reverse != "+" & pgroups$Contaminant != "+", ]
	#pgroups$Protein.IDs<-paste(sub("^([^;]*).*","\\1",pgroups$Protein.names)," [",sub("^([^;]*).*","\\1",pgroups$Gene.names)," ...] [",sub("^([^;]*).*","\\1",pgroups$Protein.IDs)," ...] (",pgroups$Proteins,")",sep="")
	pgroups$Protein.IDs<-paste(sub("^([^;]*).*","\\1",pgroups$Protein.names)," [",sub("^([^;]*).*","\\1",pgroups$Gene.names)," ...] [",sub("^([^;]*).*","\\1",pgroups$Protein.IDs)," ...]",sep="")
	if(keepEvidenceIDs)
	{
		pgroups<-pgroups[,c("Protein.IDs","Protein.names","Proteins",sort(colnames(pgroups)[grep("Razor...unique.peptides.b",colnames(pgroups))]),sort(colnames(pgroups)[grep("Peptides.b",colnames(pgroups))]),sort(colnames(pgroups)[grep("Ratio.H.M.count.b",colnames(pgroups))]),sort(colnames(pgroups)[grep("Intensity.(M|H).b",colnames(pgroups))]),sort(colnames(pgroups)[grep("Ratio.H.M.b",colnames(pgroups))]),"Evidence.IDs")]
	}
	else{
		pgroups<-pgroups[,c("Protein.IDs","Protein.names","Proteins",sort(colnames(pgroups)[grep("Razor...unique.peptides.b",colnames(pgroups))]),sort(colnames(pgroups)[grep("Peptides.b",colnames(pgroups))]),sort(colnames(pgroups)[grep("Ratio.H.M.count.b",colnames(pgroups))]),sort(colnames(pgroups)[grep("Intensity.(M|H).b",colnames(pgroups))]),sort(colnames(pgroups)[grep("Ratio.H.M.b",colnames(pgroups))]))]
	}
	colnames(pgroups[grep("Razor...unique.peptides.b",colnames(pgroups))])<-paste(sub("\\.\\.\\.","\\.",colnames(pgroups[grep("Razor...unique.peptides.b",colnames(pgroups))]))) 
	pgroups$time.point<-time.point
	print(paste("Identified proteins (w/o contaminants): ",nrow(pgroups[pgroups$time.point == time.point,])," (",time.point,")",sep=""))
	return(pgroups)
}

# Reads PD (1.3) psms table*, groups peptides according to the first leading protein of the protein group they belong to
# and returns an Protein ID along with a replicate number. This can be imported in Excel later, do a pivot table and
# obtain the reproducibility of identification between replicates.
#
#* Although PD exports a proteinGroups table, it is not usable
# as it is not known how protein grouping is performed. Because protein group id and peptide id information has to
# be preserved for various kinds of calculations a basic grouping is done manually. Note for quantitation, this
# grouping does not affect any kind of calculation as only unique peptides are used which have always an 1:1
# correspondence with protein IDs (so no grouping).
#TODO: generalize for any replicate structure, as it now assumes 3 biological x 3 technical replicates (nested)
id_Venn3_pgroups_PD_doGroupIDs<-function(fname,evidence_fname,time.point,rep_structure,filterL=F,rep_order=NA){
	evidence<-read.table(evidence_fname, header = T, sep = "\t",quote='',stringsAsFactors=F,comment.char = "")
	#Generate Evidence ID
	evidence$id<-1:(nrow(evidence))	

	tmp<-sub("^([^;]*).*","\\1",evidence$Protein.Group.Accessions)
	tmp1<-unlist(lapply(evidence$Protein.Descriptions, function(x) substr(x,1,gregexpr(" - \\[",x)[[1]][1]-1)))

	evidence$Protein.IDs<-paste(sub("^(.*) OS=.*","\\1",tmp1)," [",sub(".* GN=([^ ]*).*$","\\1",tmp1)," ...]",sep="")
	evidence$Protein.IDs<-paste(evidence$Protein.IDs," [",tmp," ...]",sep="")
	evidence$Protein.Group.Accessions<-evidence$Protein.IDs

	#write.table(evidence,file="tmp.txt",row.names=F,sep="\t")
	evidence$Spectrum.File<-factor(evidence$Spectrum.File)

	#Sum Quan channels individually for each raw file
	#subtotals<-ddply(evidence,c("Spectrum.File","Protein.Group.Accessions"),function(x) c(Light=sum(x$Light,na.rm=T),Medium=sum(x$Medium,na.rm=T),Heavy=sum(x$Heavy,na.rm=T),n.Peptides=sum(!is.na(x$Heavy))))
	#WARNING: below line assumes that ratio (or peptide) quantification counts are the same for all SILAC partners (that's why only one column, the Heavy, is considered as the representative, because it makes no difference). This happens when in PD, in the quantification options the "re-quantify" (it is called something else) option is selected
	subtotals<-ddply(evidence,c("Spectrum.File","Protein.Group.Accessions"),function(x) data.frame(n.Peptides=sum(!is.na(x$Sequence))))

	melted_subtotals<-melt(subtotals)	

	n_techreps<-length(rep_structure)/length(which(!duplicated(rep_structure)))
	i_bioreps<-which(!duplicated(rep_structure))[1:n_techreps]
	n_bioreps<-length(i_bioreps)
	
	melted_subtotals$Spectrum.File<-factor(melted_subtotals$Spectrum.File)

	#WARNING: Here it is assumed that the lexigographic order of raw file names is the order of acquisition (usually true)
	
	rep_desc<-paste(paste("b",rep_structure[1:(length(rep_structure)/2)],sep=""),paste("t",rep(1:n_techreps),sep=""),sep="")
	if(!is.na(rep_order)){
		o<-unlist(lapply(rep_order,function(x)((x-1)*n_techreps+1):(((x-1)*n_techreps+1)+n_techreps-1)))
		rep_desc<-rep_desc[o]
	}

	levels(melted_subtotals$Spectrum.File)<-rep_desc

	pgroups<-as.data.frame(tapply(melted_subtotals$value,list(Protein.IDs=melted_subtotals$Protein.Group.Accessions,RawFile=melted_subtotals$Spectrum.File,LMHn=melted_subtotals$variable),function(x) sum(x,na.rm=T)))
	colnames(pgroups)[grep("\\.n\\.Peptides",colnames(pgroups))]<-sub("(.*)\\.n\\.Peptides","Peptides.\\1",colnames(pgroups)[grep("\\.n\\.Peptides",colnames(pgroups))])
	pgroups$Protein.IDs<-row.names(pgroups)

	pgroups$time.point<-time.point
	
	pgroups<-pgroups[pgroups$Protein.IDs!=" [ ...] [ ...]",]
	print(paste("Identified proteins: ",nrow(pgroups)," (",time.point,")",sep=""))

	b1<-data.frame(Protein.IDs=pgroups[rowSums(pgroups[,colnames(pgroups)[grep("Peptides.b1",colnames(pgroups))]],na.rm=T)>0,c("Protein.IDs")],stringsAsFactors=F)
	b1$rep<-"1"
	b2<-data.frame(Protein.IDs=pgroups[rowSums(pgroups[,colnames(pgroups)[grep("Peptides.b2",colnames(pgroups))]],na.rm=T)>0,c("Protein.IDs")],stringsAsFactors=F)
	b2$rep<-"2"
	b3<-data.frame(Protein.IDs=pgroups[rowSums(pgroups[,colnames(pgroups)[grep("Peptides.b3",colnames(pgroups))]],na.rm=T)>0,c("Protein.IDs")],stringsAsFactors=F)
	b3$rep<-"3"
	venn_data<-rbind(b1,b2,b3)
	return(venn_data)
}


# Like the (see) above function, but without protein grouping.
#TODO: generalize for any replicate structure, as it now assumes 3 biological x 3 technical replicates (nested)
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

	allproteins$Protein.IDs<-paste(sub("^(.*) OS=.*","\\1",tmp1)," [",sub(".* GN=([^ ]*).*$","\\1",tmp1)," ...]",sep="")
	allproteins$Protein.IDs<-paste(allproteins$Protein.IDs," [",tmp," ...]",sep="")
	allproteins<-allproteins[!duplicated(allproteins$Protein.IDs),]
	#write.table(allproteins,file="tmp.txt",row.names=F,sep="\t")

	print(paste("Identified proteins: ",nrow(allproteins)," (",time.point,")",sep=""))

	evidence$Spectrum.File<-factor(evidence$Spectrum.File)

	#Sum Quan channels individually for each raw file
	#subtotals<-ddply(evidence,c("Spectrum.File","Protein.Group.Accessions"),function(x) c(Light=sum(x$Light,na.rm=T),Medium=sum(x$Medium,na.rm=T),Heavy=sum(x$Heavy,na.rm=T),n.Peptides=sum(!is.na(x$Heavy))))
	#WARNING: below line assumes that ratio (or peptide) quantification counts are the same for all SILAC partners (that's why only one column, the Heavy, is considered as the representative, because it makes no difference). This happens when in PD, in the quantification options the "re-quantify" (it is called something else) option is selected
	subtotals<-ddply(evidence,c("Spectrum.File","Protein.Group.Accessions"),function(x) data.frame(n.Peptides=sum(!is.na(x$Sequence))))

	melted_subtotals<-melt(subtotals)	

	#write.table(tmp,file="tmp.txt",row.names=F,sep="\t")
	#write.table(evidence,file="tmp2.txt",row.names=F,sep="\t")

	n_techreps<-length(rep_structure)/length(which(!duplicated(rep_structure)))
	i_bioreps<-which(!duplicated(rep_structure))[1:n_techreps]
	n_bioreps<-length(i_bioreps)
	
	melted_subtotals$Spectrum.File<-factor(melted_subtotals$Spectrum.File)

	#WARNING: Here it is assumed that the lexigographic order of raw file names is the order of acquisition (usually true)
	
	rep_desc<-paste(paste("b",rep_structure[1:(length(rep_structure)/2)],sep=""),paste("t",rep(1:n_techreps),sep=""),sep="")
	if(!is.na(rep_order)){
		o<-unlist(lapply(rep_order,function(x)((x-1)*n_techreps+1):(((x-1)*n_techreps+1)+n_techreps-1)))
		rep_desc<-rep_desc[o]
	}

	levels(melted_subtotals$Spectrum.File)<-rep_desc

	pgroups<-as.data.frame(tapply(melted_subtotals$value,list(Protein.IDs=melted_subtotals$Protein.Group.Accessions,RawFile=melted_subtotals$Spectrum.File,LMHn=melted_subtotals$variable),function(x) sum(x,na.rm=T)))
	colnames(pgroups)[grep("\\.n\\.Peptides",colnames(pgroups))]<-sub("(.*)\\.n\\.Peptides","Peptides.\\1",colnames(pgroups)[grep("\\.n\\.Peptides",colnames(pgroups))])
	pgroups$Protein.IDs<-row.names(pgroups)

	tmpdf<-evidence[,c("Protein.Group.Accessions","Protein.Descriptions")]
	colnames(tmpdf)<-c("Protein.IDs","Protein.Descriptions")
	tmpdf<-tmpdf[!duplicated(tmpdf$Protein.IDs),]
	pgroups<-merge(pgroups,tmpdf,by="Protein.IDs",all.x=T)

	tmp<-sub("^([^;]*).*","\\1",pgroups$Protein.IDs)
	tmp1<-unlist(lapply(pgroups$Protein.Descriptions, function(x) substr(x,1,gregexpr(" - \\[",x)[[1]][1]-1)))

	pgroups$Protein.IDs<-paste(sub("^(.*) OS=.*","\\1",tmp1)," [",sub(".* GN=([^ ]*).*$","\\1",tmp1)," ...]",sep="")
	pgroups$Protein.IDs<-paste(pgroups$Protein.IDs," [",tmp," ...]",sep="")
	pgroups<-pgroups[,!(colnames(pgroups) %in% c("Protein.Descriptions"))]


	pgroups$time.point<-time.point
	print(paste("Identified proteins: ",nrow(pgroups)," (",time.point,")",sep=""))

	pgroups<-pgroups[pgroups$Protein.IDs!=" [ ...] [ ...]",]
	row.names(pgroups)<-pgroups$Protein.IDs
		

	b1<-data.frame(Protein.IDs=pgroups[rowSums(pgroups[,colnames(pgroups)[grep("Peptides.b1",colnames(pgroups))]],na.rm=T)>0,c("Protein.IDs")],stringsAsFactors=F)
	b1$rep<-"1"
	b2<-data.frame(Protein.IDs=pgroups[rowSums(pgroups[,colnames(pgroups)[grep("Peptides.b2",colnames(pgroups))]],na.rm=T)>0,c("Protein.IDs")],stringsAsFactors=F)
	b2$rep<-"2"
	b3<-data.frame(Protein.IDs=pgroups[rowSums(pgroups[,colnames(pgroups)[grep("Peptides.b3",colnames(pgroups))]],na.rm=T)>0,c("Protein.IDs")],stringsAsFactors=F)
	b3$rep<-"3"
	venn_data<-rbind(b1,b2,b3)
	return(venn_data)
}

#Like the above, but for MaxQuant data
#TODO: generalize for any replicate structure, as it now assumes 3 biological x 3 technical replicates (nested)
id_Venn3_pgroups<-function(pgroups){
	b1<-data.frame(Protein.IDs=pgroups[rowSums(pgroups[,colnames(pgroups)[grep("Peptides.b1",colnames(pgroups))]],na.rm=T)>0,c("Protein.IDs")],stringsAsFactors=F)
	b1$rep<-"1"
	b2<-data.frame(Protein.IDs=pgroups[rowSums(pgroups[,colnames(pgroups)[grep("Peptides.b2",colnames(pgroups))]],na.rm=T)>0,c("Protein.IDs")],stringsAsFactors=F)
	b2$rep<-"2"
	b3<-data.frame(Protein.IDs=pgroups[rowSums(pgroups[,colnames(pgroups)[grep("Peptides.b3",colnames(pgroups))]],na.rm=T)>0,c("Protein.IDs")],stringsAsFactors=F)
	b3$rep<-"3"
	venn_data<-rbind(b1,b2,b3)
	return(venn_data)
}

#Like the above, but for quantified proteins (quantified means having a Ratio.H.M.count greater than 0 for each replicate.
#TODO: generalize for any replicate structure, as it now assumes 3 biological x 3 technical replicates (nested)
quant_Venn3_pgroups<-function(pgroups){
	b1<-data.frame(Protein.IDs=pgroups[rowSums(pgroups[,c("Ratio.H.M.count.b1t1","Ratio.H.M.count.b1t2","Ratio.H.M.count.b1t3")], na.rm=T) > 0,c("Protein.IDs")],stringsAsFactors=F)
	b1$rep<-"1"
	b2<-data.frame(Protein.IDs=pgroups[rowSums(pgroups[,c("Ratio.H.M.count.b2t1","Ratio.H.M.count.b2t2","Ratio.H.M.count.b2t3")], na.rm=T) > 0,c("Protein.IDs")],stringsAsFactors=F)
	b2$rep<-"2"
	b3<-data.frame(Protein.IDs=pgroups[rowSums(pgroups[,c("Ratio.H.M.count.b3t1","Ratio.H.M.count.b3t2","Ratio.H.M.count.b3t3")], na.rm=T) > 0,c("Protein.IDs")],stringsAsFactors=F)
	b3$rep<-"3"
	venn_data<-rbind(b1,b2,b3)
	return(venn_data)
}


# MaxQuant (1.3.0.5) only
# Used to generate Venn data for showing protein ID reproducibility between identified and quantified proteins.
# Quantified proteins using this function will be considered only those with a total Ratio.H.M.count > 2 across replicates
#TODO: generalize for any replicate structure for the identified proteins, as it now assumes 3 biological x 3 technical replicates (nested)
do_generate_Venn3_data_quant_filter_1<-function(pgroups,time.point,outputFigsPrefix=""){
	setwd("limma output")
	#venn_data<-quant_Venn3_pgroups(pgroups[pgroups$time.point == time.point,])
	venn_data<-quant_Venn3_pgroups(pgroups_filter_1_v2(pgroups[pgroups$time.point == time.point,],time.point))
	write.table(venn_data,file=paste(outputFigsPrefix,"_quant_venn3-data-relaxed_",time.point,".txt",sep=""),sep="\t",row.names=F)
	venn_data<-id_Venn3_pgroups(pgroups[pgroups$time.point == time.point,])
	write.table(venn_data,file=paste(outputFigsPrefix,"_id_venn3-data_",time.point,".txt",sep=""),sep="\t",row.names=F)
	setwd("..")
}

# MaxQuant (1.3.0.5) only
# Like the above, apart from the quant filter.
# Quantified proteins using this function will be considered only those with a total Ratio.H.M.count > 2 for at least two replicates
#TODO: generalize for any replicate structure for the identified proteins, as it now assumes 3 biological x 3 technical replicates (nested)
do_generate_Venn3_data_quant_filter_2reps<-function(pgroups,time.point,outputFigsPrefix=""){
	setwd("limma output")
	#venn_data<-quant_Venn3_pgroups(pgroups[pgroups$time.point == time.point,])
	venn_data<-quant_Venn3_pgroups(pgroups_filter_2reps_v2(pgroups[pgroups$time.point == time.point,],time.point))
	write.table(venn_data,file=paste(outputFigsPrefix,"_quant_venn3-data-2reps_",time.point,".txt",sep=""),sep="\t",row.names=F)
	venn_data<-id_Venn3_pgroups(pgroups[pgroups$time.point == time.point,])
	write.table(venn_data,file=paste(outputFigsPrefix,"_id_venn3-data_",time.point,".txt",sep=""),sep="\t",row.names=F)
	setwd("..")
}

# Like the above, but for PD (1.3)
do_generate_Venn3_data_quant_filter_2reps_PD<-function(pgroups,time.point,evidence_fname,rep_structure,rep_order=NA,outputFigsPrefix="",grouping=F){
	if(grouping){
		venn_data<-id_Venn3_pgroups_PD_doGroupIDs("",evidence_fname,time.point,rep_structure,rep_order)
	}else{
		venn_data<-id_Venn3_pgroups_PD("",evidence_fname,time.point,rep_structure,rep_order)
	}
	setwd("limma output")
	write.table(venn_data,file=paste(outputFigsPrefix,"_id_venn3-data_",time.point,".txt",sep=""),sep="\t",row.names=F)
	venn_data<-quant_Venn3_pgroups(pgroups_filter_2reps_v2(pgroups[pgroups$time.point == time.point,],time.point))
	write.table(venn_data,file=paste(outputFigsPrefix,"_quant_venn3-data-2reps_",time.point,".txt",sep=""),sep="\t",row.names=F)
	setwd("..")
}

#Basic quant filter, at least 3 Ratio.H.M.counts across all replicates
pgroups_filter_1_v2<-function(pgroups){
	reps_cols<-colnames(pgroups)[grep("Ratio.H.M.count.",colnames(pgroups))]
	pgroups_quant<-pgroups[rowSums(pgroups[,reps_cols],na.rm=T)>2,]

	#replace 0 with NA
	pgroups_quant<-as.data.frame(lapply(pgroups_quant, function(x){replace(x, x == 0, NA)}),stringsAsFactors = FALSE)
	pgroups_quant<-as.data.frame(lapply(pgroups_quant, function(x){replace(x, is.nan(x), NA)}),stringsAsFactors = FALSE)

	print(paste("Quantified proteins (>2 peptides in total): ",nrow(pgroups_quant[pgroups_quant$time.point == time.point,])," (",time.point,")",sep=""))
	return(pgroups_quant)
}

#More stringent quant filter, require quantitation in at least 2 replicates
pgroups_filter_2reps_v2<-function(pgroups,reps){	#reps is dummy here
	n_techreps<-length(rep_structure)/length(which(!duplicated(rep_structure)))
	i_bioreps<-which(!duplicated(rep_structure))[1:n_techreps]
	n_bioreps<-length(i_bioreps)
	reps_cols<-colnames(pgroups)[grep("Ratio.H.M.count.",colnames(pgroups))]
	
	
	ratioRepTruth<-c()
	for(rep_cols_i in i_bioreps){
		curr_techreps_cols<-reps_cols[c(rep(rep_cols_i,n_techreps)+(0:(n_techreps-1)))]
		ratioRepTruth<-cbind(ratioRepTruth,rowSums(pgroups[,curr_techreps_cols],na.rm=T)>2)
	}
	filter<-apply(ratioRepTruth,1,function(x) length(which(x)))
	pgroups_intersect<-pgroups[filter>1,]
	
	#replace 0 with NA
	pgroups_intersect<-as.data.frame(lapply(pgroups_intersect, function(x){replace(x, x == 0, NA)}),stringsAsFactors = FALSE)
	pgroups_intersect<-as.data.frame(lapply(pgroups_intersect, function(x){replace(x, is.nan(x), NA)}),stringsAsFactors = FALSE)

	print(paste("Quantified proteins (>2 peptides/",n_techreps," injections in at least ",2," replicates): ",nrow(pgroups_intersect[pgroups_intersect$time.point == time.point,])," (",time.point,")",sep=""))
	return(pgroups_intersect)
}

#Prepare protein intensity table for differential expression analysis (the format limma requires)
prepare_working_pgroups<-function(working_pgroups){
	rownames(working_pgroups)<-working_pgroups$Protein.IDs
	inten_cols<-c(sort(colnames(working_pgroups)[grep("Intensity.M.b",colnames(working_pgroups))]),sort(colnames(working_pgroups)[grep("Intensity.H.b",colnames(working_pgroups))]))
	working_pgroups<-working_pgroups[,inten_cols]
	colnames(working_pgroups)<-sub("Intensity\\.","",inten_cols)
	return(working_pgroups)
}

# NOT SUPPORTED/MAINTAINED ANY MORE
# Like the above, but just for a specified replicate
prepare_working_pgroups_rep<-function(working_pgroups,rep){
	rownames(working_pgroups)<-working_pgroups$Protein.IDs
	inten_cols<-c(sort(colnames(working_pgroups)[grep(paste("Intensity.M.b",as.character(rep),sep=""),colnames(working_pgroups))]),sort(colnames(working_pgroups)[grep(paste("Intensity.H.b",as.character(rep),sep=""),colnames(working_pgroups))]))
	working_pgroups<-working_pgroups[,inten_cols]
	colnames(working_pgroups)<-sub("Intensity\\.","",inten_cols)
	return(working_pgroups)
}


#Perform the analysis using the basic quant filter (see above pgroups_filter_1_v2)
do_analyse_all_relaxed_v2<-function(pgroups,time.point,exp_design_fname,rep_structure,exportFormat="pdf",outputFigsPrefix=""){
	working_pgroups<-pgroups_filter_1_v2(pgroups[pgroups$time.point == time.point,])
	working_pgroups<-prepare_working_pgroups(working_pgroups)
	outputFigsPrefix<-paste(outputFigsPrefix,"-all-relaxed",sep="")
	return(do_limma_analysis(working_pgroups,time.point,exp_design_fname,rep_structure,exportFormat="pdf",outputFigsPrefix=outputFigsPrefix))
}

#Perform the analysis using the more stringent quant filter (see above pgroups_filter_2reps_v2)
do_analyse_all_2reps_v2<-function(pgroups,time.point,exp_design_fname,rep_structure,exportFormat="pdf",outputFigsPrefix=""){
	working_pgroups<-pgroups_filter_2reps_v2(pgroups[pgroups$time.point == time.point,])
	working_pgroups<-prepare_working_pgroups(working_pgroups)
	outputFigsPrefix<-paste(outputFigsPrefix,"-all-2reps",sep="")
	return(do_limma_analysis(working_pgroups,time.point,exp_design_fname,rep_structure,exportFormat="pdf",outputFigsPrefix=outputFigsPrefix))
}

#MaxQuant (1.3.0.5) only
#Gets rid of proteins identified just by "Light" peptides (100% L).
#Requires to read the both the proteinGroups and the evidence file
filter_unlabeled_proteins<-function(protein_groups_fname,evidence_fname)
{
	protein_groups<-read.table(protein_groups_fname, header = T, sep = "\t",quote='',stringsAsFactors=F)
	N_proteins_before<-nrow(protein_groups)
	evidence<-read.table(evidence_fname, header = T, sep = "\t",quote='',stringsAsFactors=F)
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
	print(paste("Before L peptide filtering (proteins, peptides): ",N_proteins_before,", ",N_peptides_before,". After: ",N_proteins_after,", ",N_peptides_after,".",sep=""))
	return(protein_groups)
}

# PD (1.3) only
getLMH<-function(evidence){
	LMH<-as.data.frame(tapply(evidence$Protein.Group.Accessions,list(Acc=evidence$Protein.Group.Accessions,LMH=evidence$Quan.Channel),length))
	return(LMH)
}

# PD (1.3) only
getLMH_Proteins<-function(LMH){
	return(c(sum(!is.na(LMH$Light)),sum(!is.na(LMH$Medium)),sum(!is.na(LMH$Heavy))))
}

# PD (1.3) only
getLMH_Peptides<-function(LMH){
	return(c(sum(LMH$Light,na.rm=T),sum(LMH$Medium,na.rm=T),sum(LMH$Heavy,na.rm=T)))
}

# PD (1.3) only
# Like the read.pgroups_v2 for MaxQuant data. 
read.pgroups_v2_PD<-function(fname,evidence_fname,time.point,rep_structure,filterL=F,keepEvidenceIDs=F,rep_order=NA){
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

	allproteins$Protein.IDs<-paste(sub("^(.*) OS=.*","\\1",tmp1)," [",sub(".* GN=([^ ]*).*$","\\1",tmp1)," ...]",sep="")
	allproteins$Protein.IDs<-paste(allproteins$Protein.IDs," [",tmp," ...]",sep="")
	allproteins<-allproteins[!duplicated(allproteins$Protein.IDs),]



	subtotals_MH<-ddply(evidence,c("Protein.Group.Accessions"),function(x) data.frame(Light=length(which(!grepl("Label:",x$Modifications))),Medium=length(which(grepl("Label:2H\\(4\\)\\)",x)|grepl("Label:13C\\(6\\)\\)",x$Modifications))),Heavy=length(which(grepl("Label:13C\\(6\\)15N\\(2\\)",x)|grepl("Label:13C\\(6\\)15N\\(4\\)",x$Modifications)))))
	subtotals_MH$MH_percentage<-apply(subtotals_MH,1,function(x){
						LMH<-as.numeric(x[-1])
						if(LMH[1]==0){
							return(100)
						}else{
							return(100*sum(LMH[-1])/sum(LMH))
						}
					}
			)
      colnames(subtotals_MH)[1]<-"Protein.IDs"

	print(paste("Identified proteins: ",nrow(allproteins)," (",time.point,")",sep=""))
	evidence<-evidence[evidence$Quan.Info=="Unique" & evidence$Quan.Usage=="Used",]
	
	#filter out peptides where medium and heavy intensity are identical (Heavy.Medium==1) & (X..Missing.Channels==2) & (Quan.Channel=="Light"), meaning only the light peak was found
	evidence[is.na(evidence$Heavy.Medium),c("Heavy.Medium")]<-0
	evidence[is.na(evidence$X..Missing.Channels),c("X..Missing.Channels")]<-0
	evidence<-evidence[evidence$Heavy.Medium!=1 & evidence$X..Missing.Channels!=2,]
	evidence[evidence$Heavy.Medium==0,c("Heavy.Medium")]<-NA	#restore back to previous format, change was just for the above condition to work (with NAs it doesn't)

	LMH<-getLMH(evidence)
	n_MH_Proteins<-nrow(LMH[rowSums(LMH[,c("Medium","Heavy")],na.rm=T)>0,]) #Sum of M and H peptides > 0
	n_Peptides<-nrow(as.data.frame(tapply(evidence$Sequence,list(Acc=evidence$Sequence),length)))
	
	if(filterL){
		evidence<-evidence[evidence$Quan.Channel!="Light",]
		LMH2<-getLMH(evidence)
		n_MH_Peptides<-nrow(as.data.frame(tapply(evidence$Sequence,list(Acc=evidence$Sequence),length)))
		print(paste("Before L peptide filtering (proteins, peptides): ",nrow(LMH),", ",n_Peptides,". After: ",nrow(LMH2),", ",n_MH_Peptides,".",sep=""))
		LMH<-LMH2
		n_Peptides<-n_MH_Peptides
	}
	print(paste("Subtotals input (proteins, peptides): ",nrow(LMH),", ",n_Peptides,".",sep=""))
	#tmp<-as.data.frame(tapply(evidence$Sequence,list(Seq=evidence$Sequence),length))

	evidence$Spectrum.File<-factor(evidence$Spectrum.File)

	#Sum Quan channels individually for each raw file
	#subtotals<-ddply(evidence,c("Spectrum.File","Protein.Group.Accessions"),function(x) c(Light=sum(x$Light,na.rm=T),Medium=sum(x$Medium,na.rm=T),Heavy=sum(x$Heavy,na.rm=T),n.Peptides=sum(!is.na(x$Heavy))))
	#WARNING: below line assumes that ratio (or peptide) quantification counts are the same for all SILAC partners (that's why only one column, the Heavy, is considered as the representative, because it makes no difference). This happens when in PD, in the quantification options the "re-quantify" (it is called something else) option is selected
	subtotals<-ddply(evidence,c("Spectrum.File","Protein.Group.Accessions"),function(x) data.frame(Evidence.IDs=paste(x$id,collapse=";"),Light=sum(x$Light,na.rm=T),Medium=sum(x$Medium,na.rm=T),Heavy=sum(x$Heavy,na.rm=T),n.Peptides=sum(!is.na(x$Heavy))))
	all_evidence_ids<-ddply(subtotals,c("Protein.Group.Accessions"),function(x) data.frame(Evidence.IDs=paste(x$Evidence.IDs,collapse=";")))
	colnames(all_evidence_ids)<-c("Protein.IDs","Evidence.IDs")

	melted_subtotals<-melt(subtotals)	

	#write.table(tmp,file="tmp.txt",row.names=F,sep="\t")
	#write.table(evidence,file="tmp2.txt",row.names=F,sep="\t")

	n_techreps<-length(rep_structure)/length(which(!duplicated(rep_structure)))
	i_bioreps<-which(!duplicated(rep_structure))[1:n_techreps]
	n_bioreps<-length(i_bioreps)
	
	melted_subtotals$Spectrum.File<-factor(melted_subtotals$Spectrum.File)

	#WARNING: Here it is assumed that the lexigographic order of raw file names is the order of acquisition (usually true)
	
	rep_desc<-paste(paste("b",rep_structure[1:(length(rep_structure)/2)],sep=""),paste("t",rep(1:n_techreps),sep=""),sep="")
	if(!is.na(rep_order)){
		o<-unlist(lapply(rep_order,function(x)((x-1)*n_techreps+1):(((x-1)*n_techreps+1)+n_techreps-1)))
		rep_desc<-rep_desc[o]
	}

	levels(melted_subtotals$Spectrum.File)<-rep_desc

	pgroups<-as.data.frame(tapply(melted_subtotals$value,list(Protein.IDs=melted_subtotals$Protein.Group.Accessions,RawFile=melted_subtotals$Spectrum.File,LMHn=melted_subtotals$variable),function(x) sum(x,na.rm=T)))
	colnames(pgroups)[grep("\\.Light",colnames(pgroups))]<-sub("(.*)\\.Light","Intensity.L.\\1",colnames(pgroups)[grep("\\.Light",colnames(pgroups))])
	colnames(pgroups)[grep("\\.Medium",colnames(pgroups))]<-sub("(.*)\\.Medium","Intensity.M.\\1",colnames(pgroups)[grep("\\.Medium",colnames(pgroups))])
	colnames(pgroups)[grep("\\.Heavy",colnames(pgroups))]<-sub("(.*)\\.Heavy","Intensity.H.\\1",colnames(pgroups)[grep("\\.Heavy",colnames(pgroups))])

	colnames(pgroups)[grep("\\.n\\.Peptides",colnames(pgroups))]<-sub("(.*)\\.n\\.Peptides","Ratio.H.M.count.\\1",colnames(pgroups)[grep("\\.n\\.Peptides",colnames(pgroups))])
	pgroups<-pgroups[,!(colnames(pgroups) %in% colnames(pgroups)[grep("Intensity\\.L\\.",colnames(pgroups))])]
	pgroups$Protein.IDs<-row.names(pgroups)

	if(keepEvidenceIDs)
	{
		pgroups<-merge(pgroups,all_evidence_ids,by="Protein.IDs",all.x=T)
	}


	tmpdf<-evidence[,c("Protein.Group.Accessions","Protein.Descriptions")]
	colnames(tmpdf)<-c("Protein.IDs","Protein.Descriptions")
	tmpdf<-tmpdf[!duplicated(tmpdf$Protein.IDs),]
	pgroups<-merge(pgroups,tmpdf,by="Protein.IDs",all.x=T)

	tmp<-sub("^([^;]*).*","\\1",pgroups$Protein.IDs)
	tmp1<-unlist(lapply(pgroups$Protein.Descriptions, function(x) substr(x,1,gregexpr(" - \\[",x)[[1]][1]-1)))

	pgroups$Protein.IDs<-paste(sub("^(.*) OS=.*","\\1",tmp1)," [",sub(".* GN=([^ ]*).*$","\\1",tmp1)," ...]",sep="")
	pgroups$Protein.IDs<-paste(pgroups$Protein.IDs," [",tmp," ...]",sep="")
	pgroups<-pgroups[,!(colnames(pgroups) %in% c("Protein.Descriptions"))]

	

	pgroups$time.point<-time.point
	pgroups<-merge(pgroups,subtotals_MH,by="Protein.IDs",all.x=T)

	print(paste("Quantifiable proteins: ",nrow(pgroups)," (",time.point,")",sep=""))

	row.names(pgroups)<-pgroups$Protein.IDs
	return(pgroups)
}

# Like the read.pgroups_v2_PD with protein grouping enabled (see above id_Venn3_pgroups_PD_doGroupIDs) 
read.pgroups_v2_PD_doGroupIDs<-function(fname,evidence_fname,time.point,rep_structure,filterL=F,keepEvidenceIDs=F,rep_order=NA){
	evidence<-read.table(evidence_fname, header = T, sep = "\t",quote='',stringsAsFactors=F,comment.char = "")
	#Generate Evidence ID
	evidence$id<-1:(nrow(evidence))	
	tmp<-sub("^([^;]*).*","\\1",evidence$Protein.Group.Accessions)
	tmp1<-unlist(lapply(evidence$Protein.Descriptions, function(x) substr(x,1,gregexpr(" - \\[",x)[[1]][1]-1)))

	evidence$Protein.IDs<-paste(sub("^(.*) OS=.*","\\1",tmp1)," [",sub(".* GN=([^ ]*).*$","\\1",tmp1)," ...]",sep="")
	evidence$Protein.IDs<-paste(evidence$Protein.IDs," [",tmp," ...]",sep="")
	evidence$Protein.Group.Accessions<-evidence$Protein.IDs	# Protein grouping can now take place because the Protein.Group.Accessions value will be the same for groups with the same leading protein
	#M+H% calculation (goes to a different data frame, we bind it in the very end with the result data frame)
	#For all replicates, "Spectrum.File" not pivoted
	evidence$Quan.Channel<-factor(evidence$Quan.Channel)
	subtotals_MH<-ddply(evidence,c("Protein.Group.Accessions"),function(x) data.frame(Light=length(which(!grepl("Label:",x$Modifications))),Medium=length(which(grepl("Label:2H\\(4\\)\\)",x)|grepl("Label:13C\\(6\\)\\)",x$Modifications))),Heavy=length(which(grepl("Label:13C\\(6\\)15N\\(2\\)",x)|grepl("Label:13C\\(6\\)15N\\(4\\)",x$Modifications)))))
	subtotals_MH$MH_percentage<-apply(subtotals_MH,1,function(x){
						LMH<-as.numeric(x[-1])
						if(LMH[1]==0){
							return(100)
						}else{
							return(100*sum(LMH[-1])/sum(LMH))
						}
					}
			)
      colnames(subtotals_MH)[1]<-"Protein.IDs"


	###
	evidence<-evidence[evidence$Quan.Info=="Unique" & evidence$Quan.Usage=="Used",]
	
	#filter out peptides where medium and heavy intensity are identical (Heavy.Medium==1) & (X..Missing.Channels==2) & (Quan.Channel=="Light"), meaning only the light peak was found
	evidence[is.na(evidence$Heavy.Medium),c("Heavy.Medium")]<-0
	evidence[is.na(evidence$X..Missing.Channels),c("X..Missing.Channels")]<-0
	evidence<-evidence[evidence$Heavy.Medium!=1 & evidence$X..Missing.Channels!=2,]
	evidence[evidence$Heavy.Medium==0,c("Heavy.Medium")]<-NA	#restore back to previous format, change was just for the above condition to work (with NAs it doesn't)

	LMH<-getLMH(evidence)
	n_MH_Proteins<-nrow(LMH[rowSums(LMH[,c("Medium","Heavy")],na.rm=T)>0,]) #Sum of M and H peptides > 0
	n_Peptides<-nrow(as.data.frame(tapply(evidence$Sequence,list(Acc=evidence$Sequence),length)))
	
	if(filterL){
		evidence<-evidence[evidence$Quan.Channel!="Light",]
		LMH2<-getLMH(evidence)
		n_MH_Peptides<-nrow(as.data.frame(tapply(evidence$Sequence,list(Acc=evidence$Sequence),length)))
		print(paste("Before L peptide filtering (proteins, peptides): ",nrow(LMH),", ",n_Peptides,". After: ",nrow(LMH2),", ",n_MH_Peptides,".",sep=""))
		LMH<-LMH2
		n_Peptides<-n_MH_Peptides
	}
	print(paste("Subtotals input (proteins, peptides): ",nrow(LMH),", ",n_Peptides,".",sep=""))
	#tmp<-as.data.frame(tapply(evidence$Sequence,list(Seq=evidence$Sequence),length))

	evidence$Spectrum.File<-factor(evidence$Spectrum.File)

	#Sum Quan channels individually for each raw file
	#subtotals<-ddply(evidence,c("Spectrum.File","Protein.Group.Accessions"),function(x) c(Light=sum(x$Light,na.rm=T),Medium=sum(x$Medium,na.rm=T),Heavy=sum(x$Heavy,na.rm=T),n.Peptides=sum(!is.na(x$Heavy))))
	#WARNING: below line assumes that ratio (or peptide) quantification counts are the same for all SILAC partners (that's why only one column, the Heavy, is considered as the representative, because it makes no difference). This happens when in PD, in the quantification options the "re-quantify" (it is called something else) option is selected
	subtotals<-ddply(evidence,c("Spectrum.File","Protein.Group.Accessions"),function(x) data.frame(Evidence.IDs=paste(x$id,collapse=";"),Light=sum(x$Light,na.rm=T),Medium=sum(x$Medium,na.rm=T),Heavy=sum(x$Heavy,na.rm=T),n.Peptides=sum(!is.na(x$Heavy))))
	all_evidence_ids<-ddply(subtotals,c("Protein.Group.Accessions"),function(x) data.frame(Evidence.IDs=paste(x$Evidence.IDs,collapse=";")))
	colnames(all_evidence_ids)<-c("Protein.IDs","Evidence.IDs")

	melted_subtotals<-melt(subtotals)	

	#write.table(row.names(pgroups),file="tmp.txt",row.names=F,sep="\t")
	#write.table(evidence,file="tmp2.txt",row.names=F,sep="\t")

	n_techreps<-length(rep_structure)/length(which(!duplicated(rep_structure)))
	i_bioreps<-which(!duplicated(rep_structure))[1:n_techreps]
	n_bioreps<-length(i_bioreps)
	
	melted_subtotals$Spectrum.File<-factor(melted_subtotals$Spectrum.File)

	#WARNING: Here it is assumed that the lexigographic order of raw file names is the order of acquisition (usually true)
	
	rep_desc<-paste(paste("b",rep_structure[1:(length(rep_structure)/2)],sep=""),paste("t",rep(1:n_techreps),sep=""),sep="")
	if(!is.na(rep_order)){
		o<-unlist(lapply(rep_order,function(x)((x-1)*n_techreps+1):(((x-1)*n_techreps+1)+n_techreps-1)))
		rep_desc<-rep_desc[o]
	}

	levels(melted_subtotals$Spectrum.File)<-rep_desc

	pgroups<-as.data.frame(tapply(melted_subtotals$value,list(Protein.IDs=melted_subtotals$Protein.Group.Accessions,RawFile=melted_subtotals$Spectrum.File,LMHn=melted_subtotals$variable),function(x) sum(x,na.rm=T)))
	colnames(pgroups)[grep("\\.Light",colnames(pgroups))]<-sub("(.*)\\.Light","Intensity.L.\\1",colnames(pgroups)[grep("\\.Light",colnames(pgroups))])
	colnames(pgroups)[grep("\\.Medium",colnames(pgroups))]<-sub("(.*)\\.Medium","Intensity.M.\\1",colnames(pgroups)[grep("\\.Medium",colnames(pgroups))])
	colnames(pgroups)[grep("\\.Heavy",colnames(pgroups))]<-sub("(.*)\\.Heavy","Intensity.H.\\1",colnames(pgroups)[grep("\\.Heavy",colnames(pgroups))])

	colnames(pgroups)[grep("\\.n\\.Peptides",colnames(pgroups))]<-sub("(.*)\\.n\\.Peptides","Ratio.H.M.count.\\1",colnames(pgroups)[grep("\\.n\\.Peptides",colnames(pgroups))])
	pgroups<-pgroups[,!(colnames(pgroups) %in% colnames(pgroups)[grep("Intensity\\.L\\.",colnames(pgroups))])]
	pgroups$Protein.IDs<-row.names(pgroups)

	if(keepEvidenceIDs)
	{
		pgroups<-merge(pgroups,all_evidence_ids,by="Protein.IDs",all.x=T)
	}

	pgroups$time.point<-time.point
	
	pgroups<-merge(pgroups,subtotals_MH,by="Protein.IDs",all.x=T)

	pgroups<-pgroups[pgroups$Protein.IDs!=" [ ...] [ ...]",]
	print(paste("Quantifiable proteins: ",nrow(pgroups)," (",time.point,")",sep=""))
	return(pgroups)
}


setwd("E:\\GE Work\\MaxQuant\\MaxQuant DataAnalysis\\DataAnalysis")

#GLOBAL variables
#duplicateCorrelation_trim<-0.22 # use 0.15 for "nice" datasets (not "too many" missing values)
duplicateCorrelation_trim<-0.15

rep_structure<-rep(1:6,each=3)	# 3 bioreps, 3 injections (techreps) each


time.point<-"2h"
PDdata<-F
outputFigsPrefix<-"QuaNCAT-rev_IV_2h_HM"
protein_groups<-read.pgroups_v2("QuaNCATrev_IV_2h_proteinGroups.txt","2h",filterL=T,evidence_fname="QuaNCATrev_IV_2h_evidence.txt")
#do_generate_Venn3_data_quant_filter_1(protein_groups,time.point,outputFigsPrefix=outputFigsPrefix)
do_generate_Venn3_data_quant_filter_2reps(protein_groups,time.point,outputFigsPrefix=outputFigsPrefix)


time.point<-"4h"
PDdata<-F
outputFigsPrefix<-"QuaNCAT-rev_IV_4h_HM"
protein_groups<-read.pgroups_v2("QuaNCATrev_IV_4h_proteinGroups.txt","4h",filterL=T,evidence_fname="QuaNCATrev_IV_4h_evidence.txt")
#do_generate_Venn3_data_quant_filter_1(protein_groups,time.point,outputFigsPrefix=outputFigsPrefix)
do_generate_Venn3_data_quant_filter_2reps(protein_groups,time.point,outputFigsPrefix=outputFigsPrefix)

#time.point<-"2h"
#PDdata<-F
#outputFigsPrefix<-"QuaNCAT-rev_IV_2h_wo_re-quant_HM"
#protein_groups<-read.pgroups_v2("QuaNCATrev_IV_2h_wo_re-quant_proteinGroups.txt","2h",filterL=T,evidence_fname="QuaNCATrev_IV_2h_wo_re-quant_evidence.txt")


#PD
# grouping: Perform proteins subtotals (whether it is counting peptides or suming peptides intensities) by grouping
# peptides in the psms table whenever the leading Protein (first protein ID in Protein.Group.Accessions values) is the same
grouping<-T

#PD
PDdata<-T
time.point<-"2h"
outputFigsPrefix<-"QuaNCAT-rev_IV_2h_PD"
evidence_fname<-"QuanCAT v3 IV 2h_psms_PD.txt"
grouping = T;
if(grouping){
	protein_groups<-read.pgroups_v2_PD_doGroupIDs("QuanCAT v3 IV 2h_proteingroups_PD.txt",evidence_fname,time.point,rep_structure,keepEvidenceIDs=T)
}else{
	protein_groups<-read.pgroups_v2_PD("QuanCAT v3 IV 2h_proteingroups_PD.txt",evidence_fname,time.point,rep_structure,keepEvidenceIDs=T)
}
write.table(protein_groups,file=paste(outputFigsPrefix,"_proteinGroupsDF.txt",sep=""),row.names=F,sep="\t")
do_generate_Venn3_data_quant_filter_2reps_PD(protein_groups,time.point,evidence_fname,rep_structure,outputFigsPrefix=outputFigsPrefix,grouping=grouping)

#PD
PDdata<-T
time.point<-"4h"
outputFigsPrefix<-"QuaNCAT-rev_IV_4h_PD"
evidence_fname<-"QuanCAT v3 IV 4h_psms_PD.txt"
if(grouping){
	protein_groups<-read.pgroups_v2_PD_doGroupIDs("QuanCAT v3 IV 4h_proteingroups_PD.txt",evidence_fname,time.point,rep_structure,keepEvidenceIDs=T,rep_order=c(2,1,3))
}else{
	protein_groups<-read.pgroups_v2_PD("QuanCAT v3 IV 4h_proteingroups_PD.txt",evidence_fname,time.point,rep_structure,keepEvidenceIDs=T,rep_order=c(2,1,3))
}
write.table(protein_groups,file=paste(outputFigsPrefix,"_proteinGroupsDF.txt",sep=""),row.names=F,sep="\t")
do_generate_Venn3_data_quant_filter_2reps_PD(protein_groups,time.point,evidence_fname,rep_structure,rep_order=c(2,1,3),outputFigsPrefix=outputFigsPrefix,grouping=grouping)



expdesign<-data.frame(rbind(cbind(paste(sub("Intensity\\.","",sort(colnames(protein_groups)[grep("Intensity.M.b",colnames(protein_groups))]))),"CONTROL"),cbind(paste(sub("Intensity\\.","",sort(colnames(protein_groups)[grep("Intensity.H.b",colnames(protein_groups))]))),"STIMULATED")))
colnames(expdesign)<-c("Sample","Category")
write.table(expdesign,file="curr_exp_design.txt",row.names =F,quote=F,sep = "\t")
exp_design_fname<-"curr_exp_design.txt"

#results<-do_analyse_all_relaxed_v2(protein_groups,time.point,exp_design_fname,rep_structure,exportFormat="pdf",outputFigsPrefix=outputFigsPrefix)
results<-do_analyse_all_2reps_v2(protein_groups,time.point,exp_design_fname,rep_structure,exportFormat="pdf",outputFigsPrefix=outputFigsPrefix)


# VALIDATION HELP
# For MaxQuant only, for PD this can be done much better through its GUI
getValidationData<-function(pgroups_fname,diffexp_fname,evidence_fname,output_fname){
	diffexp<-read.table(diffexp_fname, header = T, sep = "\t",quote='',stringsAsFactors=F)
	protein_groups<-read.table(pgroups_fname, header = T, sep = "\t",quote='',stringsAsFactors=F)
	evidence<-read.table(evidence_fname, header = T, sep = "\t",quote='',stringsAsFactors=F)
	
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

getValidationData(pgroups_fname="QuaNCATrev_IV_2h_wo_re-quant_proteinGroups.txt",
diffexp_fname="QuaNCAT-rev_IV_2h_wo_re-quant_HM-all-2reps_diffexp_2h.txt",
evidence_fname="QuaNCATrev_IV_2h_wo_re-quant_evidence.txt",
output_fname="QuaNCATrev_IV_2h_wo_re-quant_HM-2reps_diffexpevidence.txt")

# Again, for MaxQuant only
getProteinPeptideData_2reps_filter<-function(pgroups_fname,evidence_fname,output_fname,time.point, filterL=T,linkLimmaout=F,limma_outfname=""){
	protein_groups<-read.pgroups_v2(pgroups_fname,time.point,filterL=filterL,evidence_fname=evidence_fname,keepEvidenceIDs=T)
	protein_groups<-pgroups_filter_2reps_v2(protein_groups[protein_groups$time.point == time.point,])

	evidence<-read.table(evidence_fname, header = T, sep = "\t",quote='',stringsAsFactors=F)
	
	peptide_evidence<-as.data.frame(do.call(rbind,apply(protein_groups,1,function(x) cbind(x["Protein.IDs"],unlist(strsplit(x["Evidence.IDs"],";"))))),stringsAsFactors=F)
	colnames(peptide_evidence)<-c("Protein.IDs","Evidence.ID")
	peptide_evidence$Protein.IDs<-factor(peptide_evidence$Protein.IDs)
	peptide_evidence$Evidence.ID<-as.numeric(peptide_evidence$Evidence.ID)
	evidence<-evidence[,c("id","Sequence","Labeling.State","Raw.file","MS.MS.Scan.Number","m.z","Charge","Mass.Error..ppm.","K.Count","R.Count","Protein.group.IDs")]
	colnames(evidence)<-c("Evidence.ID","Sequence","Labeling.State","Raw.file","MS.MS.Scan.Number","m.z","Charge","Mass.Error.ppm.","K.Count","R.Count","Protein.group.IDs")
	peptide_evidence<-merge(peptide_evidence,protein_groups[,!names(protein_groups) %in% c("Evidence.IDs")],by="Protein.IDs",all.x=T)
	peptide_evidence<-merge(peptide_evidence,evidence,by="Evidence.ID",all.x=T)
	if(linkLimmaout){
		limmaout<-read.table(limma_outfname, header = T, sep = "\t",quote='',stringsAsFactors=F)
		peptide_evidence<-merge(peptide_evidence,limmaout,by="Protein.IDs",all.x=T)
	}
	write.table(peptide_evidence,file=output_fname,sep="\t",row.names=F)
}

getProteinPeptideData_2reps_filter(
"QuaNCATrev_IV_2h_proteinGroups.txt",
"QuaNCATrev_IV_2h_evidence.txt",
"QuaNCATrev_IV_2h_HM-all-2reps_evidence.txt",
"2h",
filterL=T,
linkLimmaout=T,
limma_outfname="QuaNCAT-rev_IV_2h_HM-all-2reps_limmaout_2h.txt")

getProteinPeptideData_2reps_filter(
"QuaNCATrev_IV_4h_proteinGroups.txt",
"QuaNCATrev_IV_4h_evidence.txt",
"QuaNCATrev_IV_4h_HM-all-2reps_evidence.txt",
"4h",
filterL=T,
linkLimmaout=T,
limma_outfname="QuaNCAT-rev_IV_4h_HM-all-2reps_limmaout_4h.txt")

getProteinPeptideData_2reps_filter(
"QuaNCATrev_IV_2h_proteinGroups.txt",
"QuaNCATrev_IV_2h_evidence.txt",
"QuaNCATrev_IV_2h_all-2reps_evidence.txt",
"2h",
filterL=F)

getProteinPeptideData_2reps_filter(
"QuaNCATrev_IV_4h_proteinGroups.txt",
"QuaNCATrev_IV_4h_evidence.txt",
"QuaNCATrev_IV_4h_all-2reps_evidence.txt",
"4h",
filterL=F)



#DEBUG ABOVE
pgroups_fname<-"QuaNCATrev_IV_2h_proteinGroups.txt"
evidence_fname<-"QuaNCATrev_IV_2h_evidence.txt"
output_fname<-"QuaNCATrev_IV_2h_HM-all-2reps_evidence.txt"
time.point<-"2h"
limma_outfname<-"QuaNCAT-rev_IV_2h_HM-all-2reps_limmaout_2h.txt"


