library(ggplot2)
library(coda)
library(ggthemes)

# clear workspace
rm(list = ls())

# Set the directory to the directory of the file
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)


log_score <- list.files(path="./xmls_coupled_new_mig_prior_02-05_150seq_6chains", pattern=paste("h3n2_",".*.log", sep=""), full.names = TRUE)

first = T
for (i in seq(1,length(log_score))){
  t_ <- read.table(log_score[[i]], header=TRUE, sep="\t")
  # take a 10% burnin
  t_ <- t_[seq(ceiling(length(t_$posterior))/10,length(t_$posterior)), ]
  
  if (first){
    t.score = t_
    first = F
  }
  else {
    t.score = rbind(t.score, t_) 
  }
}

max_posterior <- t.score[which.max(t.score$posterior),]$posterior

for (i in seq(1,length(log_score))){
  t_ <- read.table(log_score[[i]], header=TRUE, sep="\t")
  # take a 10% burnin
  t_ <- t_[seq(ceiling(length(t_$posterior))/10,length(t_$posterior)), ]
  print(log_score[[i]])
  print(length(t_$posterior))
  print(t_[which(t_$posterior==max_posterior),])
}


log_mascot <- list.files(path="./MASCOT/150_seq", pattern=paste("h3n2_",".*.log", sep=""), full.names = TRUE)

first = T
for (i in seq(1,length(log_mascot))){
  t_ <- read.table(log_mascot[[i]], header=TRUE, sep="\t")
  # take a 10% burnin
  t_ <- t_[seq(ceiling(length(t_$posterior))/10,length(t_$posterior)), ]
  
  if (first){
    t.mascot = t_
    first = F
  }
  else{
    t.mascot = rbind(t.mascot, t_)    
  }
}


rea.score = data.frame(New_Zealand=t.score$reassortmentRate.New_Zealand, 
                       South_East_Asia=t.score$reassortmentRate.South_East_Asia,
                       USA=t.score$reassortmentRate.USA)

clockRates_HA.1 = data.frame(score=(t.score$clockRate.c*t.score$mutationRate.s.HA_1),
                          mascot=(t.mascot$clockRate.c*t.mascot$mutationRate.s.HA_1))
clockRates_HA.3 = data.frame(score=(t.score$clockRate.c*t.score$mutationRate.s.HA_3),
                          mascot=(t.mascot$clockRate.c*t.mascot$mutationRate.s.HA_3))

treeHeight.HA = data.frame(score=t.score$HA.tree.height,
                           mascot=t.mascot$HA.tree.height)

mig.New_Zealand_to_USA = data.frame(score=t.score$b_migrationRate.New_Zealand_to_USA,
                                    mascot=t.mascot$b_migrationRate.New_Zealand_to_USA)

mig.New_Zealand_to_South_East_Asia = data.frame(score=t.score$b_migrationRate.New_Zealand_to_South_East_Asia,
                                                mascot=t.mascot$b_migrationRate.New_Zealand_to_South_East_Asia)

mig.USA_to_New_Zealand = data.frame(score=t.score$b_migrationRate.USA_to_New_Zealand,
                                    mascot=t.mascot$b_migrationRate.USA_to_New_Zealand)

mig.USA_to_South_East_Asia = data.frame(score=t.score$b_migrationRate.USA_to_South_East_Asia,
                                        mascot=t.mascot$b_migrationRate.USA_to_South_East_Asia)

mig.South_East_Asia_to_USA = data.frame(score=t.score$b_migrationRate.South_East_Asia_to_USA,
                                        mascot=t.mascot$b_migrationRate.South_East_Asia_to_USA)

mig.South_East_Asia_to_New_Zealand = data.frame(score=t.score$b_migrationRate.South_East_Asia_to_New_Zealand,
                                                mascot=t.mascot$b_migrationRate.South_East_Asia_to_New_Zealand)


Ne.USA = data.frame(score=t.score$popSize.t.USA, mascot=t.mascot$popSize.USA)

Ne.New_Zealand = data.frame(score=t.score$popSize.t.New_Zealand, mascot=t.mascot$popSize.New_Zealand)

Ne.South_East_Asia = data.frame(score=t.score$popSize.t.South_East_Asia, mascot=t.mascot$popSize.South_East_Asia)



coal.USA = data.frame(score=(1/t.score$popSize.t.USA), mascot=(1/t.mascot$popSize.USA))

coal.New_Zealand = data.frame(score=(1/t.score$popSize.t.New_Zealand), mascot=(1/t.mascot$popSize.New_Zealand))

coal.South_East_Asia = data.frame(score=(1/t.score$popSize.t.South_East_Asia), mascot=(1/t.mascot$popSize.South_East_Asia)) 


p.mig <- ggplot() + 
  geom_violin(data=mig.USA_to_New_Zealand, aes(x=1, y=mascot, fill=" mascot ", colour=" mascot ")) +
  geom_violin(data=mig.USA_to_New_Zealand, aes(x=2, y=score, fill=" score ", colour=" score ")) +
  geom_violin(data=mig.USA_to_South_East_Asia, aes(x=3, y=mascot, fill=" mascot ", colour=" mascot ")) +
  geom_violin(data=mig.USA_to_South_East_Asia, aes(x=4, y=score, fill=" score ", colour=" score ")) +
  geom_violin(data=mig.New_Zealand_to_South_East_Asia, aes(x=5, y=mascot, fill=" mascot ", colour=" mascot ")) +
  geom_violin(data=mig.New_Zealand_to_South_East_Asia, aes(x=6, y=score, fill=" score ", colour=" score ")) +
  geom_violin(data=mig.New_Zealand_to_USA, aes(x=7, y=mascot, fill=" mascot ", colour=" mascot ")) +
  geom_violin(data=mig.New_Zealand_to_USA, aes(x=8, y=score, fill=" score ", colour=" score ")) +
  geom_violin(data=mig.South_East_Asia_to_New_Zealand, aes(x=9, y=mascot, fill=" mascot ", colour=" mascot ")) +
  geom_violin(data=mig.South_East_Asia_to_New_Zealand, aes(x=10, y=score, fill=" score ", colour=" score ")) +
  geom_violin(data=mig.South_East_Asia_to_USA, aes(x=11, y=mascot, fill=" mascot ", colour=" mascot ")) +
  geom_violin(data=mig.South_East_Asia_to_USA, aes(x=12, y=score, fill=" score ", colour=" score ")) +
  theme_hc() +
  scale_fill_brewer("blues")+
  scale_color_brewer("blues", guide="none") +
  theme(legend.title = element_blank(), text = element_text(size=25), axis.text.x = element_text(size = 16), plot.background = element_blank(), legend.position = c(0.8, 0.71),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.ticks.x = element_line(colour = "transparent")) +
  guides(fill = guide_legend(override.aes = list(colour = "transparent"))) +
  scale_x_continuous(breaks=c(1.5,3.5,5.5,7.5,9.5,11.5), labels = c("USA\nto\nNew Zealand","USA\nto\nSEA",
                                                                    "New Zealand\nto\nSEA","New Zealand\nto\nUSA",
                                                                    "SEA\nto\nNew Zealand", "SEA\nto\nUSA"))


plot(p.mig)
ggsave(plot=p.mig,paste("Figures/migration_rate_comparison",".pdf", sep=""),width=11, height=5)


p.coal <- ggplot() + 
  geom_violin(data=coal.New_Zealand, aes(x=1, y=mascot, fill=" mascot ", colour=" mascot ")) +
  geom_violin(data=coal.New_Zealand, aes(x=2, y=score, fill=" score ", colour=" score ")) +
  geom_violin(data=coal.USA, aes(x=3, y=mascot, fill=" mascot ", colour=" mascot ")) +
  geom_violin(data=coal.USA, aes(x=4, y=score, fill=" score ", colour=" score ")) +
  geom_violin(data=coal.South_East_Asia, aes(x=5, y=mascot, fill=" mascot ", colour=" mascot ")) +
  geom_violin(data=coal.South_East_Asia, aes(x=6, y=score, fill=" score ", colour=" score ")) +
  theme_hc() +
  scale_fill_brewer("blues")+
  scale_color_brewer("blues", guide="none") +
  theme(legend.title = element_blank(),text = element_text(size=25), legend.position = c(0.8, 0.85),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.ticks.x = element_line(colour = "transparent")) +
  guides(fill = guide_legend(override.aes = list(colour = "transparent"))) +
  scale_x_continuous(breaks=c(1.5,3.5,5.5), labels = c("New Zealand","USA", "SEA"))

plot(p.coal)
ggsave(plot=p.coal,paste("Figures/coalescent_rate_comparison",".pdf", sep=""),width=6, height=5)


p.Ne <- ggplot() + 
  geom_violin(data=Ne.New_Zealand, aes(x=1, y=mascot, fill=" mascot ", colour=" mascot ")) +
  geom_violin(data=Ne.New_Zealand, aes(x=2, y=score, fill=" score ", colour=" score ")) +
  geom_violin(data=Ne.USA, aes(x=3, y=mascot, fill=" mascot ", colour=" mascot ")) +
  geom_violin(data=Ne.USA, aes(x=4, y=score, fill=" score ", colour=" score ")) +
  geom_violin(data=Ne.South_East_Asia, aes(x=5, y=mascot, fill=" mascot ", colour=" mascot ")) +
  geom_violin(data=Ne.South_East_Asia, aes(x=6, y=score, fill=" score ", colour=" score ")) +
  theme_hc() +
  scale_fill_brewer("blues")+
  scale_color_brewer("blues", guide="none") +
  theme(legend.title = element_blank(),text = element_text(size=25), axis.text.x = element_text(size = 16), legend.position = c(0.2, 0.87),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.ticks.x = element_line(colour = "transparent")) +
  guides(fill = guide_legend(override.aes = list(colour = "transparent"))) +
  scale_x_continuous(breaks=c(1.5,3.5,5.5), labels = c("New Zealand","USA", "SEA"))

plot(p.Ne)
ggsave(plot=p.Ne,paste("Figures/Ne_comparison",".pdf", sep=""),width=6, height=5)

p.rea <- ggplot() + 
  geom_violin(data=rea.score, aes(x=1, y=New_Zealand, fill=" New_Zealand ", colour=" New Zealand ")) +
  geom_violin(data=rea.score, aes(x=2, y=USA, fill=" USA ", colour=" USA ")) +
  geom_violin(data=rea.score, aes(x=3, y=South_East_Asia, fill=" SEA ", colour=" SEA ")) +
  theme_hc() +
  scale_fill_manual(breaks = c("1", "2", "3"),
                    values=c("#DB2D23", "#1524D6", "#03A409"))+
  scale_color_manual(breaks = c("1", "2", "3"),
                    values=c("#DB2D23", "#1524D6", "#03A409"))+
  # scale_fill_brewer("blues")+
  # scale_color_brewer("blues") +
  theme(legend.position = "none",
        axis.title.x=element_blank(), text = element_text(size=25), axis.text.x = element_text(size = 16), plot.background = element_blank(), axis.title.y=element_blank(),
        axis.ticks.x = element_line(colour = "transparent")) +
  scale_x_continuous(breaks=c(1,2,3), labels = c("New Zealand","USA", "SEA"))

plot(p.rea)
ggsave(plot=p.rea,paste("Figures/reassortment_rate",".pdf", sep=""),width=6, height=5)


p.clock_HA.1 <- ggplot() + 
  geom_violin(data=clockRates_HA.1, aes(x=1, y=mascot, fill=" mascot ", colour=" mascot ")) +
  geom_violin(data=clockRates_HA.1, aes(x=2, y=score, fill=" score ", colour=" score ")) +
  theme_hc() +
  scale_fill_brewer("blues")+
  scale_color_brewer("blues", guide="none") +
  theme(legend.title = element_blank(),text = element_text(size=25), axis.text.x = element_text(size = 16), legend.position = c(0.8, 0.8),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(colour = "transparent")))

plot(p.clock_HA.1)
ggsave(plot=p.clock_HA.1,paste("Figures/clock_rate_1_comparison",".pdf", sep=""),width=6, height=5)

p.clock_HA.3 <- ggplot() + 
  geom_violin(data=clockRates_HA.1, aes(x=1, y=mascot, fill=" mascot ", colour=" mascot ")) +
  geom_violin(data=clockRates_HA.1, aes(x=2, y=score, fill=" score ", colour=" score ")) +
  geom_violin(data=clockRates_HA.3, aes(x=3, y=mascot, fill=" mascot ", colour=" mascot ")) +
  geom_violin(data=clockRates_HA.3, aes(x=4, y=score, fill=" score ", colour=" score ")) +
  theme_hc() +
  scale_fill_brewer("blues")+
  scale_color_brewer("blues", guide="none") +
  theme(legend.title = element_blank(), text = element_text(size=25), axis.text.x = element_text(size = 16), legend.position = c(0.8, 0.87),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.ticks.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(colour = "transparent"))) +
  scale_x_continuous(limits=c(-0.5, 5.5), breaks=c(-0.5,1.5,3.5), labels = c("codon position:", "1 and 2", "3"))

plot(p.clock_HA.3)
ggsave(plot=p.clock_HA.3,paste("Figures/clock_rate_1_and_3_comparison",".pdf", sep=""),width=6, height=5)


formatter <- function(x){ 
  x=2005-x 
}

p.treeHeight_HA <- ggplot() + 
  geom_violin(data=treeHeight.HA, aes(x=1, y=mascot, fill=" mascot ", colour=" mascot ")) +
  geom_violin(data=treeHeight.HA, aes(x=2, y=score, fill=" score ", colour=" score ")) +
  theme_hc() +
  scale_fill_brewer("blues")+
  scale_color_brewer("blues", guide="none") +
  scale_y_continuous(labels = formatter) +
  theme(legend.title = element_blank(), text = element_text(size=25), legend.position = c(0.9, 0.88),
        axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(colour = "transparent")))

plot(p.treeHeight_HA)
ggsave(plot=p.treeHeight_HA,paste("Figures/treeHeight_HA_comparison",".pdf", sep=""),width=6, height=5)



rootProb = data.frame(score=c(mean(t.score$RootProbability.New_Zealand),
                              mean(t.score$RootProbability.South_East_Asia),
                              mean(t.score$RootProbability.USA)),
                      location=c(" New Zealand", " South East Asia", " USA"))


blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    plot.background = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

bp<- ggplot(rootProb, aes(x="", y=score, fill=location))+
  geom_bar(width = 1, stat = "identity")

bp


pie <- bp + coord_polar("y", start=0) +   
  scale_fill_manual(
                    values=c("#DB2D23", "#1524D6", "#03A409"))+  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(label = paste0(round(score*100), "%")), color = "white", position = position_stack(vjust = 0.5), size=5)

plot(pie)
ggsave(plot=pie,paste("Figures/root_prob",".pdf", sep=""),width=4, height=2.5)


