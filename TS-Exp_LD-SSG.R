#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code generates times series

# Clear the environment
rm(list=ls())
library(xlsx)

debugSource("Run_Simulation.R")
debugSource("Record_Response.R")
debugSource("Init.R")

Num_Runs = 5
Setup_Num = 4

ParentDir = "Output"
SubDir = paste(ParentDir, "SimulationData/TimeSeries", sep = "/")
OutputPath = paste(SubDir, paste("Setup", Setup_Num, sep = " "), sep = "/")
FileName_LD1 = paste("LD_SingleSheet", "csv", sep = ".")
FileName_LD2 = paste("LD_SeparateSheet", "xlsx", sep = ".")
FileName_SSG = paste("SSG_SeparateSheet", "xlsx", sep = ".")


PR = 0.125
PP = 0.8
LGPP = 0.1
SGLW = 10.0
CM = "FC_P_PC_A"
# CM = "PC_P_NC_A"
SD = 16968


for (i in 1:Num_Runs)
{
    # Run_Idx = i
    Run_Idx = i + 1*Num_Runs
    print(paste("Run number", Run_Idx))
    set.seed(Run_Idx)
    
    Init()
    Run_Simulation()
    Record_Response()
    
    FilePath = paste(OutputPath, FileName_LD1, sep = "/")
    write.table(LD_TimeSeries, file = FilePath, col.names=!file.exists(FilePath),
                sep = ",", append = TRUE, row.names = FALSE)
    
    FilePath = paste(OutputPath, FileName_LD2, sep = "/")
    SheetName = paste("Seed", Run_Idx, sep = "=")
    write.xlsx(LD_TimeSeries, file = FilePath, sheetName = SheetName,
               append = TRUE, row.names = FALSE)
    
    FilePath = paste(OutputPath, FileName_SSG, sep = "/")
    write.xlsx(SSG_TimeSeries, file = FilePath, sheetName = SheetName,
               append = TRUE, row.names = FALSE)
}




