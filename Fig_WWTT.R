#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code depicts the distribution of LGRD values for the 
#              Within-person Whole Trait Theory case.

# Clear the environment
rm(list=ls())


# In the within-person WTT, we plot the distribution of LGRD values in  
# the first and last 100 days of simulation for a single agent, i.e., a single  
# run with a specific combination of parameters. As each day is equivalent to 24 
# simulation steps, 100 days equals 2400 steps which for the last days are  
# the steps where we freeze the agent's state space and lift the parental 
# rule.

# The data is the result of running the code Single_Experiment_ForWTT.R

ParentDir = "Output"
OutputPath = paste(ParentDir, "SimulationData", sep = "/")

FileName = "RM_SingleExp_WTT"
FilePath = paste(OutputPath, paste(FileName, "xlsx", sep = "."), sep = "/")


library(readxl)
library(xlsx)
library(ggplot2)
library(ggpubr)

SetupNum = 1
Seed = 2
SN1 = paste("Setup", SetupNum, sep = "=")
SN2 = paste("Seed", Seed, sep = "=")
SheetName = paste(SN1, SN2, sep = "-")
ResponseMatrix = read_excel(FilePath, sheet = SheetName)

ResponseMatrix$LGRD = as.numeric(ResponseMatrix$LGRD)


windowsFonts(Times = windowsFont("Times New Roman"))
WWTT1 = ggplot(ResponseMatrix, aes(x=LGRD, color = TimeTag, fill=TimeTag)) +
       geom_histogram(alpha=0.5, position="identity", binwidth=0.05) +
       scale_color_manual(values=c("#D81B60", "#1E88E5")) + theme_light() +
       scale_fill_manual(values=c("#D81B60", "#1E88E5")) +
       labs(y = "Count", 
            x = "LGR difference",
            fill="Time tag", 
            color="Time tag") +
       theme(text=element_text(size=8,family="Times"))


SetupNum = 1
Seed = 26
SN1 = paste("Setup", SetupNum, sep = "=")
SN2 = paste("Seed", Seed, sep = "=")
SheetName = paste(SN1, SN2, sep = "-")
ResponseMatrix = read_excel(FilePath, sheet = SheetName)

ResponseMatrix$LGRD = as.numeric(ResponseMatrix$LGRD)


windowsFonts(Times = windowsFont("Times New Roman"))
WWTT2 = ggplot(ResponseMatrix, aes(x=LGRD, color = TimeTag, fill=TimeTag)) +
    geom_histogram(alpha=0.5, position="identity", binwidth=0.05) +
    scale_color_manual(values=c("#D81B60", "#1E88E5")) + theme_light() +
    scale_fill_manual(values=c("#D81B60", "#1E88E5")) +
    labs(y = "Count", 
         x = "LGR difference",
         fill="Time tag", 
         color="Time tag") +
    theme(text=element_text(size=8,family="Times"))

MultiPlot = ggarrange(WWTT1, WWTT2, labels = c("", ""), ncol = 2, nrow = 1,
                      common.legend = TRUE, legend="right")

FigurePath = paste(ParentDir, "Figures/fig-WWTT", sep = "/")

ggsave(file = paste(FigurePath, "eps", sep = "."), device = cairo_pdf,
       width=7, height=5, dpi=1000)

ggsave(file=paste(FigurePath, "jpeg", sep = "."), width=7, height=5, dpi=1000)

print(MultiPlot)

