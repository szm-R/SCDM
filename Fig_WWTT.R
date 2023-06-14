#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code depicts the distribution of LGRD values for the 
#              Within-person Whole Trait Theory case.

# Clear the environment
rm(list=ls())


# In the within-person WTT, we plot the distribution of LGRD values in  
# the first and last 13 days of simulation for a single agent, i. e. one 
# with a single combination of parameters. As each day is equivalent to 24 
# simulation steps, 13 days equals 312 steps which for the last days are  
# the steps where we freeze the agent's state space and lift the parental 
# rule.

# The data is the result of running the code Single_Experiment_ForWTT.R

ParentDir = "Output"
OutputPath = paste(ParentDir, "SimulationData", sep = "/")

FileName = "RM_SingleExp_WTT"
FilePath = paste(OutputPath, paste(FileName, "xlsx", sep = "."), sep = "/")

SheetName = "Rule=0.5-CM=FC--A"

library(readxl)
library(xlsx)

ResponseMatrix = read_excel(FilePath, sheet = SheetName)

ResponseMatrix$LGRD = as.numeric(ResponseMatrix$LGRD)

library(ggplot2)

windowsFonts(Times = windowsFont("Times New Roman"))
WWTT = ggplot(ResponseMatrix, aes(x=LGRD, color = TimeTag, fill=TimeTag)) +
       geom_histogram(alpha=0.5, position="identity", binwidth=0.01) +
       scale_color_manual(values=c("#D81B60", "#1E88E5")) + theme_light() +
       scale_fill_manual(values=c("#D81B60", "#1E88E5")) +
       labs(y = "Count", 
            x = "LGR difference",
            fill="Time tag", 
            color="Time tag") +
       theme(text=element_text(size=8,family="Times"))

FigurePath = paste(ParentDir, "Figures/fig-WWTT", sep = "/")

ggsave(file = paste(FigurePath, "eps", sep = "."), device = cairo_pdf,
       width=5, height=5, dpi=1000)

ggsave(file=paste(FigurePath, "jpeg", sep = "."), width=5, height=5, dpi=1000)

print(WWTT)

