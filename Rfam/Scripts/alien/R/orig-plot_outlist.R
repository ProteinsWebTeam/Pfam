#/software/R-2.6.0/bin/R CMD BATCH --no-save  plot_outlist.R

#Read in files:
rin<-read.table("RIN.dat", header = T, sep = "\t")
#bits    type        trueOrFalse       tax
#127.48  SEED    .       Eukaryota; Metazoa

rin2<-read.table("RIN_counts.dat", header = T, sep = "\t")
#cnt     tax
#50      Eukaryota; Alveolata

#find the range and make a vector of binsizes for the histogram:
mx<-ceiling(max(rin$bits)+1)
mn<-floor(min(rin$bits)-1)
breaks <- seq(mn,mx,length.out=40)

#Make the histograms, but don't plot - hist sucks for multi-class data, use barplot instead:
halign<-  hist(rin$bits[rin$type=='ALIGN'],breaks=breaks,plot=F)
hseed <-  hist(rin$bits[rin$type=='SEED'],breaks=breaks,plot=F)
hnot  <-  hist(rin$bits[rin$type=='NOT'],breaks=breaks,plot=F)
hthresh<- hist(rin$bits[rin$type=='THRESH'],breaks=breaks,plot=F)

mxh<-max(halign$counts+hseed$counts+hnot$counts)

hspeciesCounts<-matrix(0, nrow = length(breaks)-1, ncol = length(rin2$tax), dimnames=list(floor(halign$mids),rin2$tax) );
tax<-matrix(rin2$tax)
for (i in seq(1,length(tax))){
   if(nchar(tax[i])<3) next;
   h<-  hist(rin$bits[rin$tax==tax[i]],breaks=breaks,plot=F)
   hspeciesCounts[,tax[i]]<-h$counts
}
hspeciesCounts<-t(hspeciesCounts)


#Make the matrix data structure that barplot likes:
hc<-c(halign$counts,hseed$counts,hnot$counts,hthresh$counts)

hbits<-matrix(hc,nrow = length(breaks)-1, ncol = 4, dimnames=list(floor(halign$mids),c("ALIGN","SEED","NOT","THRESH")))
hbits<-t(hbits)

#Do some work to find where to truncate the matrices. Jen's solution for the distributions with a HUGE number of low scoring hits:
cutoff<-1
for (i in seq(2,length(breaks)-1 )){
    if( hbits["THRESH",i] > 0 ){
             cutoff<-i
	     hbits["THRESH",i]<-1
	     fbits["THRESH",i]<-1
	     break
    }
}

#Truncate 
hbitstrunc<- hbits[,cutoff:length(breaks)-1]

mxht<-max(hbitstrunc["ALIGN",]+hbitstrunc["SEED",]+hbitstrunc["NOT",])
mxh<-max(hbits["ALIGN",]+hbits["SEED",]+hbits["NOT",])

#Make sensible heights for the threshold bar:
hbits["THRESH",]<-max(c(mxh-hsumThresh,0.1*hsumThresh))*hbits["THRESH",]
hbitstrunc["THRESH",]<-max(c(mxht-hbitstrunc[1,2]-hbitstrunc[2,2]-hbitstrunc[3,2],0.1*(hbitstrunc[1,2]+hbitstrunc[2,2]+hbitstrunc[3,2])))*hbitstrunc[4,]

#Make the plots:
pdf(file="out.list.pdf")
op<-par(mfrow=c(2,1),cex=0.75,las=2)
barplot(hbits,col=c("red","blue","purple","black"),xlab="CM score (bits)",ylab="Freq",main="Distribution of bit scores") 
legend("topright",c("ALIGN","SEED","NEITHER","THRESHOLD"),fill=c("red","blue","purple","black"),ncol=2)
dev.off()

pdf(file="out.list.trunc.pdf")
op<-par(mfrow=c(2,1),cex=0.75,las=2)
barplot(hbitstrunc,col=c("red","blue","purple","black"),xlab="CM score (bits)",ylab="Freq",main="Truncated distribution of bit scores") 
legend("topright",c("ALIGN","SEED","NEITHER","THRESHOLD"),fill=c("red","blue","purple","black"),ncol=2)
dev.off()

pdf(file="out.list.bare.pdf")
op<-par(mfrow=c(2,1),cex=0.75,las=2)
barplot(hbits,col=c("red","blue","purple","black"),xlab="CM score (bits)",ylab="Freq",main="Distribution of bit scores") 
dev.off()

pdf(file="out.list.trunc.bare.pdf")
op<-par(mfrow=c(2,1),cex=0.75,las=2)
barplot(hbitstrunc,col=c("red","blue","purple","black"),xlab="CM score (bits)",ylab="Freq",main="Truncated distribution of bit scores") 
dev.off()

library(RColorBrewer)
require(graphics)
Lab.palette <- colorRampPalette(c("red", "orange", "yellow", "green", "cyan", "blue", "violet"),
                                    space = "Lab")
len<-length(tax)
len<-max(len,2)
pcols = rev(Lab.palette(len))
heatcol<-colorRampPalette(pcols)(len)

pdf(file="species.pdf")
op<-par(mfrow=c(1,1),cex=0.75,las=2)
barplot(hspeciesCounts,col=heatcol,xlab="CM score (bits)",ylab="Freq",main="Distribution of bit scores") 
legend("topleft",tax,fill=heatcol,ncol=3,cex=0.55)
dev.off()
