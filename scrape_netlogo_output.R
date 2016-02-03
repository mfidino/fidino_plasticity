# We are going to make a huge list of lists 

#tables needed

# Model settings colnames line 6, data line 7

# Plot 1. population size of each color. Colnames line 23 and 24. 
# some scrubbing needs to be done here because it is also on line 23

#grep \\"\\"\\"Population size\\"\\"\\"
#colnames are 15 rows down from this.

#we can remove all x values but 1, then keep all the y values.

#black, blue, yellow, orange, red, black 1, black 2, black 3

# each color has x(ticks), y pop size, plus color, to be ignored

# plot 2. total pop and food stuff. same set up.  variables on line 2036, 
# sub-varibles on line 2037. Each one has "x","y","color","pen down?"

#plot 3 mean stress tolerance. same set up variable on line 4047
# sub-variables on line 4048 "x","y","color","pen down?"
# x is time, y is mean stress tolerance, all I really need

#start with the simple stuff

#get only the files that we need

MdlList<-list.files(pattern = "U\\d.*")



library(reshape2)

# we can use these to stop the data table. It is these names - 2

PlotNames<-c("MODEL SETTINGS", "Population size", "total pop and food stuff",
	"Mean stress tolerance", "plot 4", "\"\"stress")

#This is actually where we search for our column names

startingPoint<-c("\"\"black\"\"\"\\,\\,\\,\\," ,"\"\"totalPop", "\"\"meanTol", "\"\"mean stress" )





grepNLogo<-function(REGEXP, netlogoFile){
	REGEXPLoc<-list()
	plotNamesLoc<-list()
	for(i in 1:length(REGEXP)){
	REGEXPLoc[[i]]<-grep(REGEXP[i], netlogoFile)
}
return(REGEXPLoc)
}

# For column names


nLogoColLoc<-function(ndata){
	colNamesLocList<-grepNLogo(startingPoint, ndata)
	intermediate<-unlist(colNamesLocList)
	colNamesLoc<-as.numeric(sort(intermediate[c(1,2,5,8,11)])) + 1
	return(colNamesLoc)
}



#Now to get the data. 


#function to make a single table

nPlot<-function(nData, whichPlot){
	nDataRaw<-readLines(nData)
	nFrame<-data.frame(table = PlotNames[2:6], start = (nLogoColLoc(nDataRaw) + 1), 
		end = c((as.numeric(unlist(grepNLogo(PlotNames[3:6], nDataRaw))) - 2), 
		(length(nDataRaw) - 2)),
		ncol =c(32, 12, 4, 32, 16))

# so that we can name the columns, very bad programming.

		nColnamesList = list(
c("Mdl","blackPop","blackcolor","blackPen",
		"blueTimeStep","bluePop","bluecolor","bluePen","yellowTimeStep",
		"yellowPop","yellowcolor","yellowPen","orangeTimeStep","orangePop",
		"orangecolor","orangePen","redTimeStep","redPop","redcolor","redPen",
		"black1TimeStep","black1Pop","black1color","black1Pen","black2TimeStep",
		"black2Pop","black2color","black2Pen","black3TimeStep","black3Pop",
		"black3color","black3Pen"), 
c("Mdl", "totalPop", "b", "b", "time","greenPatches", "b", "b", "time2", "garbage", "b", "b"),
c("Time", "meanStress", "Mdl", "b"), 
c("Mdl","blackPop","blackcolor","blackPen",
		"blueTimeStep","bluePop","bluecolor","bluePen","yellowTimeStep",
		"yellowPop","yellowcolor","yellowPen","orangeTimeStep","orangePop",
		"orangecolor","orangePen","redTimeStep","redPop","redcolor","redPen",
		"black1TimeStep","black1Pop","black1color","black1Pen","black2TimeStep",
		"black2Pop","black2color","black2Pen","black3TimeStep","black3Pop",
		"black3color","black3Pen"), 
c("Time", "meanPatchStress", "Mdl", "b",
		"b", "meanUrbanStress", "b", "b", "b", "numberForest", "b", "b", 
		"b", "numberUrban", "b", "b"))


whichTable<- grep(whichPlot, nFrame$table)
whichStart<-nFrame[whichTable, 2]
whichEnd<-nFrame[whichTable, 3]
hunch<-matrix(unlist(strsplit(nDataRaw[whichStart:whichEnd], split="\",\"")),
	ncol = nFrame[whichTable, 4], nrow = (whichEnd - whichStart + 1), 
	byrow=TRUE)
hunchFrame<-data.frame(hunch)

colnames(hunchFrame) <- nColnamesList[[whichTable]]


hunchFrame$Mdl<-rep(nData, nrow(hunchFrame))



if(whichPlot %in% PlotNames[2]){
	keeps<-c("blueTimeStep", "Mdl","blackPop","black1Pop", 
	"black2Pop", "black3Pop","bluePop", "yellowPop","orangePop","redPop")
	hunchFrame<-hunchFrame[keeps]
	hunchFrame<-melt(hunchFrame, id=c("Mdl", "blueTimeStep"))
}

if(whichPlot %in% PlotNames[3]){
	keeps<-c("Mdl", "time", "totalPop", "greenPatches", "garbage")
	hunchFrame<-hunchFrame[keeps]
	hunchFrame<-melt(hunchFrame, id=c("Mdl", "time"))
}

if(whichPlot %in% PlotNames[4]){
	keeps<-c("Mdl", "Time", "meanStress")
	hunchFrame<-hunchFrame[keeps]
  hunchFrame$Time<-as.numeric(substring(hunchFrame$Time, 2))
  hunchFrame$meanStress <-as.numeric(as.character(hunchFrame$meanStress))
  
}

if(whichPlot %in% PlotNames[5]){
	keeps<-c("blueTimeStep", "Mdl","blackPop","black1Pop", 
	"black2Pop", "black3Pop","bluePop", "yellowPop","orangePop","redPop")
	hunchFrame<-hunchFrame[keeps]
	hunchFrame<-melt(hunchFrame, id=c("Mdl", "blueTimeStep"))
}

if(whichPlot %in% PlotNames[6]){
	keeps<-c("Mdl", "meanPatchStress", "meanUrbanStress", "numberForest",
		"numberUrban")
	hunchFrame<-hunchFrame[1, keeps]
  hunchFrame$meanPatchStress<-as.numeric(as.character(hunchFrame$meanPatchStress))
  hunchFrame$meanUrbanStress<-as.numeric(as.character(hunchFrame$meanUrbanStress))
  hunchFrame$numberForest<-as.numeric(as.character(hunchFrame$numberForest))
  hunchFrame$numberUrban<-as.numeric(as.character(hunchFrame$numberUrban))
  hunchFrame$propForest<-hunchFrame$numberForest / (hunchFrame$numberForest +
                                                      hunchFrame$numberUrban)
}
return(hunchFrame)
}

### Let's try with a bunch of files now


nGrepAll<-function(REGEXP, whichPlot){
	MdlList<-list.files(pattern = "U\\d.*")
	MdlPart<-grep(REGEXP, MdlList)
	interList<-list()
for(i in 1:length(MdlPart)){
	interList[[i]]<-nPlot(MdlList[MdlPart[i]], whichPlot)	
   }
output<- do.call("rbind", interList)
return(output)
}


# To average the data, starting with mean stress

library(plyr)

lil<-"U[1-3]\\d-"

allStress<-nGrepAll("U", 
  PlotNames[4])

#figure out those that did not go exinct

allCount<-count(allStress, "Mdl")

allOver<-allCount[which(allCount$freq > 2000),]



allStressavgEq<-ddply(allStressEq[which(allStressEq$Mdl %in% allOver$Mdl),]
                    , c("Mdl"), .progress="text", summarize,
                    meanStr = mean(meanStress))

patchInfo<-nGrepAll("U", PlotNames[6])

superTest<-patchInfo[which(patchInfo$Mdl %in% allOver$Mdl),]
superTest$rat<-50/superTest$numberForest


allStressavgEq$propForest<-superTest$propForest
allStressavgEq$rat<-superTest$rat

allOrderedEq<-allStressavgEq[order(allStressavgEq$propForest),]

allOrdered$rank<-seq(1,178,1)

# As of now these two different data frames are ordered the same exact way

library(ggplot2)
library(scales)

pdf("allStressMovingAverage.pdf", family = "Palatino")
p1<-ggplot(data=test, aes(x=propForest, y = average))+
  #stat_smooth(se = FALSE, size = 2)+
  geom_line(size = 1.5)+
  geom_point(data = allOrderedEq, aes(y = meanStr), pch=21, color = "black", fill = "grey")+

  geom_vline(aes(xintercept = 0.28), linetype = 3)+
  ylab("Mean stress tolerance of all\nindividuals in last 500 time steps")+
  xlab("Proportion of forest in model landscape")+
  scale_x_continuous(labels = percent, breaks = seq(0,1,0.1))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),

        panel.background = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))
p1
dev.off()

m1<-loess(meanStr~propForest, data=allOrdered)


#allStress$Mdl<-as.factor(substring(allStress$Mdl,1,3))



m2<-lm(meanStr~Mdl, data=hunch)


ggplot(hmm3, aes(x = as.numeric(blueTimeStep), y = as.numeric(value)
	, color = as.factor(variable)), alpha = 0.2)+
	geom_point(alpha = 0.2)+
	scale_color_manual(values = colorPal)

colorPal<-c("black", "black", "black", "black", "blue", "yellow", "orange", "red")

allStress[which(count(allStress, "Mdl") > 2000),]


indStress<-nGrepAll("U", PlotNames[5])

indStress<-indStress[which(indStress$value > 0),]

indStressavg<-ddply(indStress[which(indStress$Mdl %in% allOver$Mdl),]
                    , c("Mdl", "variable"), .progress="text", summarize,
                    meanStr = mean(as.numeric(value)))

stressRle<-rle(indStressavg$Mdl)

indStressavg$propForest<-rep(superTest$propForest, times = stressRle$length)



indStressOrder<-indStressavg[order(indStressavg$propForest),]

library(RColorBrewer)
myCols<-brewer.pal(8, "RdGy")
myCols<-rev(myCols)

colfunc <- colorRampPalette(c("black","grey", "red"))
myCols<-colfunc(8)




pdf("individualStress2.pdf", family = "Palatino")
ggplot(data = indStressOrder, aes(x = propForest, y = meanStr, color = variable, size = variable))+
  stat_smooth(se = FALSE)+

  xlab("Proportion of forest in model landscape")+
  ylab("Mean stress of individuals")+
  scale_color_manual(values = colfunc(8), labels =
                       c("3.51 - 4.00", "3.01 - 3.50", "2.51 - 3.00",
                         "2.01 - 2.50", "1.51 - 2.00", "1.01 - 1.50",
                         "0.51 - 1.00", "0.01 - 0.50"),
                     name = "Stress intolerance \n      of animal")+
  scale_x_continuous(labels=percent, breaks = seq(0,1,0.1))+
  scale_size_manual(values = c(4, 2, 2, 2, 2, 2, 2, 4), guide = FALSE)+
  guides(color = guide_legend(override.aes=list(size = c(4, 2, 2, 2, 2, 2, 2 ,4))))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),

        panel.background = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))
dev.off()

ggplot(data = popSizeavgEq, aes(x = propForest, y = propPop, color = variable, size = variable))+
  
  stat_smooth(se = FALSE)+
  xlab("Proportion of forest in model landscape")+
  ylab("Proportion of total population\nin last 500 time steps")+
  scale_color_manual(values = colfunc(8), labels =
                       c("3.51 - 4.00", "3.01 - 3.50", "2.51 - 3.00",
                         "2.01 - 2.50", "1.51 - 2.00", "1.01 - 1.50",
                         "0.51 - 1.00", "0.01 - 0.50"),
                     name = "Stress intolerance \n      of animal")+
  scale_x_continuous(labels=percent, breaks = seq(0,1,0.1))+
  scale_y_continuous(labels = percent, limits = c(-0.01, 0.9))+


  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20))

popSize<-nGrepAll("U", PlotNames[2])

popSizeavg<-ddply(popSize[which(popSize$Mdl %in% allOver$Mdl),]
                    , c("Mdl", "variable"), .progress="text", summarize,
                    meanPop = mean(as.numeric(value)))

popRle<-rle(popSizeavg$Mdl)

popSizeavg$propForest<-rep(superTest$propForest, times=popRle$lengths)

popOrder<-popSizeavg[order(popSizeavg$propForest),]
popOrder2<-popOrder[which(popOrder$meanPop> 0),]
ggplot(data = popOrder, aes(x = propForest, y = meanPop, color = variable))+

 stat_smooth(se = FALSE, size = 1)+
  xlab("Percent of forest generated in model")+
  ylab("Mean stress of different stress tolerance groups")+
  scale_color_manual(values = colfunc(8), labels =
                       c("4.0 - 3.51", "3.5 - 3.01", "3.0 - 2.51",
                         "2.5 - 2.01", "2.0 - 1.51", "1.5 - 1.01",
                         "1.0 - 0.51", "0.5 - 0.01"),
                     name = "Stress tolerance \n      of animal")+
  scale_x_continuous(labels=percent, breaks = seq(0,1,0.1))+

  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


popSize$blueTimeStep<-as.numeric(as.character(popSize$blueTimeStep))
popSize$value<-as.numeric(popSize$value)

unqSp<-levels(popSize$variable)
blackPop<-ggplot(popSize[which(popSize$variable==unqSp[1]),], aes(x = blueTimeStep,
                                                         y = as.numeric(value),
                                                         group = Mdl))+
  geom_line(color=myCols[1], alpha = 0.15)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust=0))+
  ylab("population size")+
  xlab("time step")+
  ggtitle("A")

black1Pop<-ggplot(popSize[which(popSize$variable==unqSp[2]),], aes(x = blueTimeStep,
                                                                  y = as.numeric(value),
                                                                  group = Mdl))+
  geom_line(color=myCols[2], alpha = 0.15)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust=0))+
  ylab("population size")+
  xlab("time step")+
  ggtitle("B")

black2Pop<-ggplot(popSize[which(popSize$variable==unqSp[3]),], aes(x = blueTimeStep,
                                                                   y = as.numeric(value),
                                                                   group = Mdl))+
  geom_line(color=myCols[3], alpha = 0.15)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust=0))+
  ylab("population size")+
  xlab("time step")+
  ggtitle("C")

black3Pop<-ggplot(popSize[which(popSize$variable==unqSp[4]),], aes(x = blueTimeStep,
                                                                   y = as.numeric(value),
                                                                   group = Mdl))+
  geom_line(color=myCols[4], alpha = 0.15)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust=0))+
  ylab("population size")+
  xlab("time step")+
  ggtitle("D")

bluePop<-ggplot(popSize[which(popSize$variable==unqSp[5]),], aes(x = blueTimeStep,
                                                                   y = as.numeric(value),
                                                                   group = Mdl))+
  geom_line(color=myCols[5], alpha = 0.15)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust=0))+
  ylab("population size")+
  xlab("time step")+
  ggtitle("E")

yellowPop<-ggplot(popSize[which(popSize$variable==unqSp[6]),], aes(x = blueTimeStep,
                                                                 y = as.numeric(value),
                                                                 group = Mdl))+
  geom_line(color=myCols[6], alpha = 0.15)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust=0))+
  ylab("population size")+
  xlab("time step")+
  ggtitle("F")

orangePop<-ggplot(popSize[which(popSize$variable==unqSp[7]),], aes(x = blueTimeStep,
                                                                   y = as.numeric(value),
                                                                   group = Mdl))+
  geom_line(color=myCols[7], alpha = 0.15)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust=0))+
  ylab("population size")+
  xlab("time step")+
  ggtitle("G")

redPop<-ggplot(popSize[which(popSize$variable==unqSp[8]),], aes(x = blueTimeStep,
                                                                   y = as.numeric(value),
                                                                   group = Mdl))+
  geom_line(color=myCols[8], alpha = 0.15)+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust=0))+
  ylab("population size")+
  xlab("time step")+
  ggtitle("H")

library(gridExtra)

pdf("allPops.pdf", family = "Palatino")
grid.arrange(blackPop, black1Pop, black2Pop, black3Pop, bluePop, yellowPop,
             orangePop, redPop, ncol = 2)
dev.off()

popSize$blueTimeStep<-factor(popSize$blueTimeStep)


indStressavg<-ddply(indStress2[which(indStress2$Mdl %in% allOver$Mdl),]
                    , c("Mdl", "variable"), .progress="text", summarize,
                    meanStr = mean(as.numeric(value)))
hunch<-popSize[which(popSize$value>0),]

popByTime<-ddply(popSize[which(popSize$Mdl %in% allOver$Mdl),],
                 c("blueTimeStep", "variable"), .progress="text", summarize,
                 pop = mean(as.numeric(value)))

pdf("meanPopTimeStep.pdf", family = "Palatino")
ggplot(popByTime, aes(x = as.numeric(as.character(blueTimeStep)), 
                      y = pop, color = variable))+

  geom_line(size=2)+
  scale_color_manual(values = myCols,labels =
                       c("4.0 - 3.51", "3.5 - 3.01", "3.0 - 2.51",
                         "2.5 - 2.01", "2.0 - 1.51", "1.5 - 1.01",
                         "1.0 - 0.51", "0.5 - 0.01"),
                     name = "Stress intolerance \n      of animal")+
  scale_y_log10()+
  scale_x_log10()+
  theme_bw()+
  xlab("time step")+
  ylab("log 10 population size")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
dev.off()

under20<-superTest$Mdl[which(superTest$propForest<0.20)]
over40<-superTest$Mdl[which(superTest$propForest>0.40)]
theMid<-superTest$Mdl[!superTest$Mdl %in% c(under20, over40)]

under20<-under20[which(under20 %in% allOver$Mdl)]
over40<- over40[which(over40 %in% allOver$Mdl)]
theMid<-theMid[which(theMid %in% allOver$Mdl)]

  lowByTime<-ddply(popSize[which(popSize$Mdl %in% under20),],
                   c("blueTimeStep", "variable"), .progress="text", summarize,
                   pop = mean(as.numeric(value)))

medByTime<-ddply(popSize[which(popSize$Mdl %in% theMid),],
                 c("blueTimeStep", "variable"), .progress="text", summarize,
                 pop = mean(as.numeric(value)))

highByTime<-ddply(popSize[which(popSize$Mdl %in% over40),],
                 c("blueTimeStep", "variable"), .progress="text", summarize,
                 pop = mean(as.numeric(value)))

lowForest<-ggplot(lowByTime, aes(x = as.numeric(as.character(blueTimeStep)), 
                      y = pop, color = variable))+
  
  geom_line(size=1.5)+
  scale_color_manual(values = myCols,labels =
                       c("4.0 - 3.51", "3.5 - 3.01", "3.0 - 2.51",
                         "2.5 - 2.01", "2.0 - 1.51", "1.5 - 1.01",
                         "1.0 - 0.51", "0.5 - 0.01"),
                     name = "Stress intolerance \n      of animal")+
  #scale_y_log10(breaks = c(0.1, 10))+
  theme_bw()+
  xlab("time step")+
  ylab("log 10 population size")+
  ggtitle("A")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust=0),
        legend.position = "none")

medForest<-ggplot(medByTime, aes(x = as.numeric(as.character(blueTimeStep)), 
                                 y = pop, color = variable))+
  
  geom_line(size=1.5)+
  scale_color_manual(values = myCols,labels =
                       c("4.0 - 3.51", "3.5 - 3.01", "3.0 - 2.51",
                         "2.5 - 2.01", "2.0 - 1.51", "1.5 - 1.01",
                         "1.0 - 0.51", "0.5 - 0.01"),
                     name = "Stress intolerance \n      of animal")+
  #scale_y_log10(breaks = c(0.1, 10))+
  theme_bw()+
  xlab("time step")+
  ylab("log 10 population size")+
  ggtitle("B")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust=0),
        legend.position = "none")

highForest<-ggplot(highByTime, aes(x = as.numeric(as.character(blueTimeStep)), 
                                  y = pop, color = variable))+
  
  geom_line(size=1.5)+
  scale_color_manual(values = myCols,labels =
                       c("4.0 - 3.51", "3.5 - 3.01", "3.0 - 2.51",
                         "2.5 - 2.01", "2.0 - 1.51", "1.5 - 1.01",
                         "1.0 - 0.51", "0.5 - 0.01"),
                     name = "Stress intolerance \n      of animal")+
  #scale_y_log10(breaks = c(0.1, 10))+
  theme_bw()+
  xlab("time step")+
  ylab("log 10 population size")+
  ggtitle("C")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust=0),
        legend.position = "none")


fakeTable<-data.frame(Stress= factor(c("4.0 - 3.51", "3.5 - 3.01", "3.0 - 2.51",
                                "2.5 - 2.01", "2.0 - 1.51", "1.5 - 1.01",
                                "1.0 - 0.51", "0.5 - 0.01")), num = 1:8)
fakeTable$Stress<-factor(fakeTable$Stress, levels = rev(levels(fakeTable$Stress)))
#create inset table 
my_table<- tableGrob(head(fakeTable)[,1:2], 
                     gpar.coretext =gpar(fontsize=8), gpar.coltext=gpar(fontsize=8),  
                     gpar.rowtext=gpar(fontsize=8)) 

my_hist<-ggplot(fakeTable, aes(num, fill=Stress)) + 
  geom_bar()+
  scale_fill_manual(values=myCols, name = "Stress intolerance \n      of animal")+
 

#Extract Legend 
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

legend <- g_legend(my_hist) 
questing<-grid.draw(legend) 

pdf("popTimeForest.pdf", family = "Palatino")
grid.arrange(lowForest, legend, medForest, highForest, ncol=2)
dev.off()
