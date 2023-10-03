#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code plots two LGR-Days time series from different runs of 
#              similar parameter sets

# Clear the environment
rm(list=ls())

# The input data is the result of running the TS-Exp_LD-SSG.R code, stored in the
# Time series directory.

library(readxl)
library(xlsx)
library(ggplot2)
library(ggpubr)

Setup_Num = 1
Run_Idx1 = 26
Run_Idx2 = 53
Num_Days = 100


ParentDir = "Output"
SubDir = paste(ParentDir, "SimulationData/TimeSeries", sep = "/")
OutputPath = paste(SubDir, paste("Setup", Setup_Num, sep = " "), sep = "/")
FileName = paste("LD_SeparateSheet", "xlsx", sep = ".")
FilePath = paste(OutputPath, FileName, sep = "/")

Run_Idx = Run_Idx1
SheetName = paste("Seed", Run_Idx, sep = "=")
TimeSeries = read_excel(FilePath, sheet = SheetName, col_names = TRUE)

windowsFonts(Times = windowsFont("Times New Roman"))

TS1 = ggplot(TimeSeries, aes(x=Day, y=LGR)) +
    geom_point(size = 0.1) +
    scale_x_continuous(limits = c(0, Num_Days)) +
    geom_smooth(method=lm) +
    labs(y = "LGR", 
         x = "Days") + 
    theme_light() +
    theme(text=element_text(size=8,family="Times"))

Run_Idx = Run_Idx2
SheetName = paste("Seed", Run_Idx, sep = "=")
TimeSeries = read_excel(FilePath, sheet = SheetName, col_names = TRUE)

windowsFonts(Times = windowsFont("Times New Roman"))

TS2 = ggplot(TimeSeries, aes(x=Day, y=LGR)) +
    geom_point(size = 0.1) +
    scale_x_continuous(limits = c(0, Num_Days)) +
    geom_smooth(method=lm) +
    labs(y = "LGR",
         x = "Days") +
    theme_light() +
    theme(text=element_text(size=8,family="Times"))

MultiPlot = ggarrange(TS1, TS2, labels = c("", ""), ncol = 1, nrow = 2)

print(MultiPlot)

ParentDir = "Output"
NameTag1 = paste("S", Setup_Num, sep = "")
NameTag2 = paste("Run", paste(Run_Idx1, Run_Idx2, sep = "-"), sep = "_")
FileName = paste("fig_TS", NameTag1, NameTag2, sep = "_")
FigurePath = paste(ParentDir, "Figures", FileName, sep = "/")

ggsave(file = paste(FigurePath, "eps", sep = "."), device = cairo_pdf,
       width=5, height=5, dpi=1000)

ggsave(file=paste(FigurePath, "jpeg", sep = "."), width=4, height=3.0, dpi=300)













