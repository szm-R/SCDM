#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code plots the LGRD vs. parental presence probability for 
#              lax, medium, and strict rules in the PC_P_NC_A compliance mode.

# Clear the environment
rm(list=ls())

library(readxl)
library(xlsx)

# The data is the result of running the codes RunPPs_LGRD.R and 
# AnalyzePPs_LGRD.R

ParentDir = "Output"
OutputPath = paste(ParentDir, "SimulationData/PP", sep = "/")

FilePath = paste(OutputPath, paste("RS_LGRD", "xlsx", 
                                   sep = "."), sep = "/")

Aggregated_Output = data.frame()

SheetName = "Rule=0.875"
RS1 = read_excel(FilePath, sheet = paste(SheetName, "P", sep = '--'), 
                 col_names = TRUE)
Aggregated_Output = RS1
Aggregated_Output$PR[1:5] = "0.875"


SheetName = "Rule=0.5"
RS2 = read_excel(FilePath, sheet = paste(SheetName, "P", sep = '--'), 
                 col_names = TRUE)
Aggregated_Output = rbind(Aggregated_Output, RS2)
Aggregated_Output$PR[6:10] = "0.5"


SheetName = "Rule=0.125"
RS2 = read_excel(FilePath, sheet = paste(SheetName, "P", sep = '--'), 
                 col_names = TRUE)
Aggregated_Output = rbind(Aggregated_Output, RS2)
Aggregated_Output$PR[11:15] = "0.125"


library(ggplot2)

windowsFonts(Times = windowsFont("Times New Roman"))

PP = ggplot(Aggregated_Output, aes(x=PP, y=LGRD, color=PR)) +
     geom_errorbar(aes(ymin=LGRD-ME, ymax=LGRD+ME), width=.03, linewidth = 1) +
     geom_line(linewidth = 1) + 
     geom_point() +
     labs(y = "LGR difference", 
          x = "Parental presence probability",
          color="Parental rule") +
     scale_color_manual(values=c("#1E88E5", "#DC267F", "#FFB000", "#FE6100")) +
     theme_light() +
     theme(text=element_text(size=8,family="Times"))


FigurePath = paste(ParentDir, "Figures/fig-PP", sep = "/")

ggsave(file = paste(FigurePath, "eps", sep = "."), device = cairo_pdf,
       width=5, height=5, dpi=1000)

ggsave(file=paste(FigurePath, "jpeg", sep = "."), width=5, height=5, dpi=1000)

print(PP)




