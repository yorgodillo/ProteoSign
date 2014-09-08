tmp<-regexpr("^b[0-9]+t[0-9]+f[0-9]+",colnames(pgroups))
tmp_repdesc<-regmatches(colnames(pgroups),tmp)
tmp_cindex<-which(tmp==1)
tmp_map<-merge(rep_structure,data.frame(rep_desc=tmp_repdesc,tmp_cindex=tmp_cindex))

new_rep_structure<-ddply(tmp_map,c("biorep","techrep"),function(x){
  ret<-data.frame(rsums=rowSums(pgroups[,x$tmp_cindex], na.rm = T))
  ret$biorep<-x$biorep[1]
  ret$techrep<-x$techrep[1]
  ret$Protein.IDs<-pgroups$Protein.IDs
  #print(head(pgroups[,c(1,x$tmp_cindex)]))
  return(ret)
})

#####
tmp_map<-unique(merge(evidence,data.frame(Raw.file=rep_structure$raw_file,biorep=rep_structure$biorep,techrep=rep_structure$techrep))[,c("biorep","techrep","Experiment")])
tmp_map$rep_desc<-paste("b",tmp_map$biorep,"t",tmp_map$techrep,sep="")
tmp_str<-colnames(pgroups)[grep("Ratio\\.counts$",colnames(pgroups))]
tmp<-regexpr("^[^\\.]+",tmp_str)
tmp_matches<-regmatches(tmp_str,tmp)
paste(merge(data.frame(Experiment=tmp_matches),tmp_map)$rep_desc,".Ratio.counts",sep="")

tmp_str<-colnames(pgroups)[grep("^Intensity",colnames(pgroups))]
tmp<-regexpr("[^\\.]+$",tmp_str)
tmp_matches<-regmatches(tmp_str,tmp)
merge(data.frame(Experiment=tmp_matches),tmp_map)$rep_desc

# if the following is true, then it means we have fractions and we no longer need them, so rep_structure has to be redefined
if(nrow(tmp_orderdf) < nrow(rep_structure)){
  colnames(rep_structure)[grep("rep_desc",colnames(rep_structure))]<-"rep_desc_old"
  rep_structure$rep_desc<-gsub("^b([0-9]+)t([0-9]+).*","b\\1t\\2",rep_structure$rep_desc_old)
  rep_structure<-unique(rep_structure[,c("biorep","techrep","rep_desc")])
}
