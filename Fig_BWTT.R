#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code depicts the distribution of LGRD values for the 
#              Between-person Whole Trait Theory case. 

# Clear the environment
rm(list=ls())

# In the between-person WTT, we plot the distribution of LGRD values in  
# the last 100 days of simulation for two different agents, i.e., two 
# different combination of parameters. As each day is equivalent to 24 
# simulation steps, 100 days equals 2400 steps which are the steps where 
# we freeze the agent's state space and lift the parental rule.

# The data is the result of running the code Single_Experiment_ForWTT.R

ParentDir = "Output"
OutputPath = paste(ParentDir, "SimulationData", sep = "/")

FileName = "RM_SingleExp_WTT"
FilePath = paste(OutputPath, paste(FileName, "xlsx", sep = "."), sep = "/")


SheetName1 = "Setup=2-Seed=4"
SheetName2 = "Setup=1-Seed=26"

library(readxl)
library(xlsx)


ResponseMatrix = read_excel(FilePath, sheet = SheetName1)
RM1_LastDays = subset(ResponseMatrix, TimeTag == "Last days")
RM1_LastDays$TimeTag = "Agent 1"

ResponseMatrix = read_excel(FilePath, sheet = SheetName2)
RM2_LastDays = subset(ResponseMatrix, TimeTag == "Last days")
RM2_LastDays$TimeTag = "Agent 2"

RM1_LastDays$LGRD = as.numeric(RM1_LastDays$LGRD)
RM2_LastDays$LGRD = as.numeric(RM2_LastDays$LGRD)

Aggregated_Output = RM1_LastDays

Aggregated_Output = rbind(Aggregated_Output, RM2_LastDays)

Flag = rep("One", 100)
Flag = as.data.frame(append(Flag, rep("Two", 100)))

Data = cbind(Aggregated_Output, Flag)
ColumnName = c("LGRD", "TimeTag", "Agent")
colnames(Data) = ColumnName

library(ggplot2)

windowsFonts(Times = windowsFont("Times New Roman"))
BWTT = ggplot(Data, aes(x=LGRD, color = Agent, fill=Agent)) +
       geom_histogram(alpha=0.5, position="identity", binwidth=0.05) +
       scale_color_manual(values=c("#D81B60", "#1E88E5")) + theme_light() +
       scale_fill_manual(values=c("#D81B60", "#1E88E5")) +
       labs(y = "Count", 
            x = "LGR difference",
            fill="Agent", 
            color="Agent") +
       theme(text=element_text(size=8,family="Times"))

FigurePath = paste(ParentDir, "Figures/fig-BWTT", sep = "/")

ggsave(file = paste(FigurePath, "eps", sep = "."), device = cairo_pdf,
       width=5, height=5, dpi=1000)

ggsave(file=paste(FigurePath, "jpeg", sep = "."), width=5, height=5, dpi=1000)

print(BWTT)

