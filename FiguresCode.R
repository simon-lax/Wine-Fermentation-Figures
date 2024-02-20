require(readxl)  
require(ggplot2)
require(gridExtra)

#Figures 2B, 3B, and 3C

Data <- read.table("All_5SP_Data.txt",header=TRUE,sep="\t")
    Final <- Data[Data$Day == 7,]

MakePlotABV <- function(ABV) {
  
  FinalABV <- Final[Final$ABV == ABV & Final$Juice == "GEW",]
  FinalABV$Name <- paste(ABV,seq(1:nrow(FinalABV)))
  FinalABV <- FinalABV[with(FinalABV,order(DomSP,Juice,COMP)),]
  FinalABV$Order <- 1:nrow(FinalABV)
  
  PreMelt <- FinalABV[,c(20,15:19)]
  Melt <- melt(PreMelt)
  Melt$Comp <- FinalABV[match(Melt$Name,FinalABV$Name),1]
  Melt$Juice <- FinalABV[match(Melt$Name,FinalABV$Name),2]
  Melt$DomSP <- FinalABV[match(Melt$Name,FinalABV$Name),6]
  Melt$Order <- FinalABV[match(Melt$Name,FinalABV$Name),21]
  
  P1 <- ggplot(Melt,aes(x=Order,y=value,fill=variable)) + 
    geom_bar(stat="identity") + 
    scale_fill_manual(values=c('#d39f39','#4d8392','#ab8083','#d3772a','#8f9264')) +
    theme_minimal() +
    labs(y="Species Fraction",x="",fill="Species",title=paste(ABV," ABV",sep="")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_blank(),legend.position = 'none') 
  
  P2 <- ggplot(Melt,aes(x=Order,y=1,fill=DomSP)) + geom_bar(stat = "identity", width = 1)  +
    theme_void() + theme(panel.spacing.x = unit(1, "mm"),legend.position = 'none') + scale_fill_manual(values=c('#d39f39','#4d8392','#ab8083','#d3772a','#8f9264')) + labs(y="DomSP")
  
  return( plot_grid(P1, P2, align = "v", ncol = 1, axis = "tb", rel_heights = c(15, 1)) )
  
}

grid.arrange(MakePlotABV(0),MakePlotABV(2),MakePlotABV(5),nrow=1)


#Figure 2E

Data <- read.table("CompScoreCorrelations.txt",sep="\t",header=TRUE)

ggplot(Data0,aes(x=Score_Pairs,y=Score_5SP,col=SP)) + 
  geom_smooth(method="lm",col="grey") + 
  geom_text(aes(label=SP),fontface="bold",size=8) + 
  scale_x_continuous(breaks=c(30,40,50,60,70,80)) + 
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = 'none') +
  scale_color_manual(values=c('#d39f39','#4d8392','#ab8083','#d3772a','#8f9264')) +
  labs(x="Pairwise Competitive Score",y="5 Species Comptitive Score")

#Figure 4A

Data <- read.table("GrowthRateData.txt",header=TRUE,sep="\t")
AllData <- read.table("AllGRMeasurements.txt",header=TRUE,sep="\t")

ggplot(Data,aes(x=as.numeric(as.character(ABV)),y=Mean,col=SP)) + 
  geom_smooth(data=AllData,aes(x=ABV,y=GR,col=SP,fill=SP)) + 
  geom_point() + 
  geom_errorbar(aes(ymin=Mean-SEM,ymax=Mean+SEM),width=0.25) +
  labs(x="ABV",y=bquote('Growth Rate (H'^-1*')')) +
  scale_x_continuous(breaks=0:5) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = 'none') +
  scale_color_manual(values=c('#d39f39','#4d8392','#ab8083','#d3772a','#8f9264')) +
  scale_fill_manual(values=c('#d39f39','#4d8392','#ab8083','#d3772a','#8f9264'))

#Figure 4B

Data <- read.table("CompScoreCorrelations.txt",sep="\t",header=TRUE)

Pal <- c('#fed976',
         '#feb24c',
         '#fd8d3c',
         '#f03b20',
         '#bd0026')

ggplot(Data,aes(x=ABV,y=Score_Pairs,col=SP)) + 
  geom_violin(aes(group=ABV),fill="light grey") + 
  geom_point(size=3) + 
  geom_line(lwd=1.5) + 
  scale_color_manual(values=c('#d39f39','#4d8392','#ab8083','#d3772a','#8f9264')) + 
  theme_bw() + 
  labs(x="% Ethanol by Volume",y="Pairwise Competitive Score") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = 'none') +
  scale_x_continuous(breaks=0:5)

#Figure 4C-E

Data <- read.table("CompScoreCorrelations.txt",header=TRUE,sep="\t")

Pal <- c(
  "#d73027",
  "#fc8d59",
  "#fee090",
  "#e0f3f8",
  "#91bfdb",
  "#4575b4"
)

PA <- ggplot(Data,aes(x=GR_Mean,y=Score_Pairs,col=as.factor(ABV),label=SP)) + 
  geom_point(aes(shape=SP),size=6,stroke=1.5) + 
  scale_shape_manual(values=c(21,22,23,24,25)) +
  scale_color_manual(values=rev(Pal)) + scale_fill_manual(values=rev(Pal)) + theme_bw() + 
  geom_smooth(method="lm",alpha=0,lwd=2,aes(group=ABV,fill=as.factor(ABV))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = 'none') +
  labs(x=bquote('Growth Rate (H'^-1*')'),y='Pairwise Competitive Score') + scale_y_continuous(limits=c(-1,101),breaks=c(0,25,50,75,100))

PB <- ggplot(Data,aes(x=K,y=Score_Pairs,col=as.factor(ABV),label=SP)) + 
  geom_point(aes(shape=SP),size=6,stroke=1.5) + 
  scale_shape_manual(values=c(21,22,23,24,25)) +
  scale_color_manual(values=rev(Pal)) + scale_fill_manual(values=rev(Pal)) + theme_bw() + 
  geom_smooth(method="lm",alpha=0,lwd=2,aes(group=ABV,fill=as.factor(ABV))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = 'none') +
  labs(x=bquote('Carrying Capacity (OD 600)'),y='Pairwise Competitive Score') + scale_y_continuous(limits=c(-1,101),breaks=c(0,25,50,75,100))

PC <- ggplot(Data,aes(x=GR_Mean,y=Score_5SP,col=as.factor(ABV),label=SP)) + 
  geom_point(aes(shape=SP),size=6,stroke=1.5) + 
  scale_shape_manual(values=c(21,22,23,24,25)) +
  scale_color_manual(values=rev(Pal)) + scale_fill_manual(values=rev(Pal)) + theme_bw() + 
  geom_smooth(method="lm",alpha=0,lwd=2,aes(group=ABV,fill=as.factor(ABV))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = 'none') +
  labs(x=bquote('Growth Rate (H'^-1*')'),y='5 Species Competitive Score') + scale_y_continuous(limits=c(-1,101),breaks=c(0,25,50,75,100))

PD <- ggplot(Data,aes(x=K,y=Score_5SP,col=as.factor(ABV),label=SP)) + 
  geom_point(aes(shape=SP),size=6,stroke=1.5) + 
  scale_shape_manual(values=c(21,22,23,24,25)) +
  scale_color_manual(values=rev(Pal)) + scale_fill_manual(values=rev(Pal)) + theme_bw() + 
  geom_smooth(method="lm",alpha=0,lwd=2,aes(group=ABV,fill=as.factor(ABV))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = 'none') +
  labs(x=bquote('Carrying Capacity (OD 600)'),y='5 Species Competitive Score') + scale_y_continuous(limits=c(-1,101),breaks=c(0,25,50,75,100))

PE <- ggplot(Data,aes(x=Score_Pairs,y=Score_5SP,col=as.factor(ABV),label=SP)) + 
  geom_point(aes(shape=SP),size=6,stroke=1.5) + 
  scale_shape_manual(values=c(21,22,23,24,25)) +
  scale_color_manual(values=rev(Pal)) + scale_fill_manual(values=rev(Pal)) + theme_bw() + 
  geom_smooth(method="lm",alpha=0,lwd=2,aes(group=ABV,fill=as.factor(ABV))) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = 'none') +
  labs(x=bquote('Pairwise Competitive Score'),y='5 Species Competitive Score') + scale_y_continuous(limits=c(-1,101),breaks=c(0,25,50,75,100))

PAC <- grid.arrange(PA,PC,nrow=2)
PBD <- grid.arrange(PB,PD,nrow=2)

grid.arrange(PAC,PBD,PE,nrow=1)

# Figure 5

Data <- read.table("CompScoreCorrelations.txt",sep="\t",header=TRUE)

Pal <- c(
  "#d73027",
  "#fc8d59",
  "#fee090",
  "#e0f3f8",
  "#91bfdb",
  "#4575b4"
)

PA <- ggplot(Data,aes(x=Score_Pairs,y=Score_Pairs_MER)) + 
  geom_text(aes(label=SP,col=as.factor(ABV)),fontface = "bold",size=8) +
  theme_bw() + 
  geom_abline(slope=1, intercept = 0,lty=2,col="grey") +
  scale_color_manual(values=rev(Pal)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = 'none') +
  labs(x="Pairwise Competitive Score in Gewürztraminer",y="Pairwise Competitive Score in Merlot")

PB <- ggplot(Data,aes(x=Score_5SP,y=Score_5SP_Merlot)) + 
  geom_text(aes(label=SP,col=as.factor(ABV)),fontface = "bold",size=8) +
  theme_bw() + 
  geom_abline(slope=1, intercept = 0,lty=2,col="grey") +
  scale_color_manual(values=rev(Pal)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = 'none') +
  labs(x="5 Species Competitive Score in Gewürztraminer",y="5 Species Competitive Score in Merlot") +
  scale_x_continuous(limits=c(0,100)) + scale_y_continuous(limits=c(0,100))

grid.arrange(PA,PB,nrow=2)

# Figure 6

Data <- read.table('Ferm2 Kinetics.txt',header=TRUE,sep="\t")
Map <- read.table('Ferm2_Map.txt',header=TRUE,sep="\t")

Data$Juice <- Map[match(Data$Ferment,Map$Ferment),2]
Data$Type <- Map[match(Data$Ferment,Map$Ferment),3]
Data$SP <- Map[match(Data$Ferment,Map$Ferment),4]
Data$DomSP <- Map[match(Data$Ferment,Map$Ferment),5]

Mono <- Data[Data$Type == 'Mono',]

MonoGEW <- Mono[Mono$Juice == "GEW" & Mono$Day < 20,]
MonoMER <- Mono[Mono$Juice == "MER" & Mono$Day < 31,]

GEWMER <- rbind(MonoGEW,MonoMER)

ggplot(GEWMER,aes(x=Day,y=Brix,col=SP,shape=Juice,lty=Juice)) +
  geom_hline(yintercept = 0,col="dark gray",lwd=0.75) + 
  geom_point(size=3) + geom_line(lwd=1) +
  scale_color_manual(values=c('#d39f39','#4d8392','#ab8083','#d3772a','#8f9264')) +
  scale_y_continuous(limits=c(-1,26),breaks=c(0,5,10,15,20,25)) +
  labs(title="") +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = c(0.8,0.8))


Counts <- read.table("Ferm2_5SP_Counts.txt",header=TRUE,sep="\t")

MakePlot <- function(Ferment) {
  
  FermentCounts <- Counts[Counts$Ferment == Ferment,]
  
  Name <- paste(FermentCounts$Juice[1],FermentCounts$DomSP[1],FermentCounts$Rep[1],sep=" ")
  
  FermentCountsKeep <- FermentCounts[,11:15]
  row.names(FermentCountsKeep) <- FermentCounts$Day
  
  
  melted <- melt(as.matrix(FermentCountsKeep))
  
  Plot <-     ggplot(melted,aes(x=as.factor(Var1),y=value,fill=Var2)) + 
    geom_bar(stat="identity") + 
    labs(x="Day",y="Species Fraction",title=Name) + 
    theme_bw() +
    scale_fill_manual(values=c('#d39f39','#4d8392','#ab8083','#d3772a','#8f9264')) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none")
  
  return(Plot)
  
}

P1 <- MakePlot(11)  
P2 <- MakePlot(12)  
P3 <- MakePlot(13)  
P4 <- MakePlot(14)  
P5 <- MakePlot(15)  
P6 <- MakePlot(16)  
P7 <- MakePlot(17)  
P8 <- MakePlot(18)  
P9 <- MakePlot(19)  
P10 <- MakePlot(20)  
P11 <- MakePlot(21)  
P12 <- MakePlot(22)  

grid.arrange(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,nrow=3)





