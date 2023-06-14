#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code plots the LGRD vs. parental rules for different 
#              compliance modes of PC_P_NC_A and FC_P_PC_A.

# Clear the environment
rm(list=ls())

# The data is the result of running the codes RunRules_LGRD.R and 
# AnalyzeRules_LGRD.R


ParentDir = "Output"
OutputPath = paste(ParentDir, "SimulationData/LGPP-CM", sep = "/")

FilePath = paste(OutputPath, paste("RS_LGRD", "xlsx", 
                                   sep = "."), sep = "/")


library(readxl)
library(xlsx)

Aggregated_Output = data.frame()

SheetName = "LGPP=0.1-CM=FC"
RS1 = read_excel(FilePath, sheet = paste(SheetName, "P", sep = '--'), 
                 col_names = TRUE)
Aggregated_Output = RS1

SheetName = "LGPP=0.1-CM=PC"
RS2 = read_excel(FilePath, sheet = paste(SheetName, "P", sep = '--'), 
                 col_names = TRUE)

Aggregated_Output = rbind(Aggregated_Output, RS2)

Iter = rep("FC_P_PC_A", 18)
Aggregated_Output = cbind(Aggregated_Output, Iter)
Aggregated_Output$Iter[10:18] = "PC_P_NC_A"

library(ggplot2)

windowsFonts(Times = windowsFont("Times New Roman"))

FCPC = ggplot(Aggregated_Output, aes(x=PR, y=LGRD, color=Iter)) +
       geom_errorbar(aes(ymin=LGRD-ME, ymax=LGRD+ME), width=.02, linewidth = 1) +
       geom_line(linewidth = 1) + 
       geom_point() +
       labs(y = "LGR difference", 
            x = "Parental rule",
            color="Compliance mode") +
        scale_color_manual(values=c("#D81B60", "#FFC107")) + 
        theme_light() +
        theme(text=element_text(size=8,family="Times"))

FigurePath = paste(ParentDir, "Figures/fig-FCPC", sep = "/")

ggsave(file = paste(FigurePath, "eps", sep = "."), device = cairo_pdf,
       width=5, height=5, dpi=1000)

ggsave(file=paste(FigurePath, "jpeg", sep = "."), width=5, height=5, dpi=1000)

print(FCPC)




