#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code converts the simulation output to Gridware-compatible
#              trajectory file

# Clear the environment
rm(list=ls())

# The input data is the result of running the TS-Exp_LD-SSG.R code, stored in the
# Time series directory. 

library(readxl)
library(xlsx)

Num_Runs = 20
Setup_Num = 1
Run_Idx = 53

ParentDir = "C:/Users/szmoo/Dropbox/PhD/My Manuscripts/12-AB_SCD_M/Code/Output"
SubDir = paste(ParentDir, "Systematic Experiments/TimeSeries", sep = "/")
OutputPath = paste(SubDir, paste("Setup", Setup_Num, sep = " "), sep = "/")
FileName = paste("SSG_SeparateSheet", "xlsx", sep = ".")


FilePath = paste(OutputPath, FileName, sep = "/")
SheetName = paste("Seed", Run_Idx, sep = "=")
TimeSeries = read_excel(FilePath, sheet = SheetName, col_names = TRUE)

NumWeeks = 100
TimeSeries = TimeSeries[1:NumWeeks,]
TimeSeries$LGR[TimeSeries$LGR == 0] = 0.0001
TimeSeries$LGRlagged[TimeSeries$LGRlagged == 0] = 0.0001

SSG_Trajectory = TimeSeries
colnames(SSG_Trajectory) = c("ONSET", "LGR", "LGRlagged")

SSG_Trajectory$LGR <- cut(TimeSeries$LGR,
                          breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, Inf),
                          labels=c(1, 2, 3, 4, 5, 6, 7))

SSG_Trajectory$LGRlagged <- cut(TimeSeries$LGRlagged,
                                breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, Inf),
                                labels=c(1, 2, 3, 4, 5, 6, 7))


SimilarEvents = rep(FALSE, NumWeeks)
for (i in 2:NumWeeks)
{
    LGR_1 = as.numeric(SSG_Trajectory$LGR[i-1])
    LGR_2 = as.numeric(SSG_Trajectory$LGR[i])
    
    LGRlagged_1 = SSG_Trajectory$LGRlagged[i-1]
    LGRlagged_2 = SSG_Trajectory$LGRlagged[i]
    if (LGR_1 == LGR_2 && LGRlagged_1 == LGRlagged_2)
    {
        SimilarEvents[i] = TRUE
    }
}

Final_Trajectory = SSG_Trajectory[!SimilarEvents,]

FilePath = paste(OutputPath, paste("LGR_LGRlagged", "trj", sep = "."), sep = "/")
write.table(Final_Trajectory, file = FilePath,
            sep = "\t", row.names=FALSE, col.names=TRUE, quote=FALSE)
write(NumWeeks+1,                                            
      file = FilePath,
      append = TRUE)









