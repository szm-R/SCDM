#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code implements a single experiment to test the WWT

# Clear the environment
rm(list=ls())

debugSource("Run_Simulation.R")
debugSource("Measure_Response_ForWTT.R")

Num_Runs = 1
SeparateDays = TRUE


#############################################################################
#############                                                  ##############   
#############     Auxiliary Variables And Model Parameters     ##############
#############                                                  ##############   
#############################################################################

PR = 0.125
PP = 0.8
LGPP = 0.1
SGLW = 10.0
CM = "FC_P_PC_A"
# CM = "PC_P_NC_A"
SD = 3600

SetupNum = 2
Seed = 4

############################################
####  Auxiliary Variables and Constants ####    
############################################

# LG and SG indices
LG_Idx = 1
SG_Idx = 2


############################################
########         Parameters         ########    
############################################

############################################   
############################################

## General 

# Number of steps in the main body of simulation
SimDuration = SD

# Number of steps where we calculate the model 
# response (the LGRD)
Response_Steps = 2400

############################################   
############################################

## Parent agent

# Parental rule
Rule = PR

# Parental presence probability
P_Probability = PP


############################################   
############################################

## Adolescent agent

# The Percieved Autonomy decreases as the P_Probability
# increases and starts lower for strict rules
Perceived_Autonomy = Rule - P_Probability*(1 - Rule)
if (Perceived_Autonomy < 0)
    Perceived_Autonomy = 0

# The range of the state of space
State_Range = 101

# The feedback value of a self-initiated action
Self_Initiation_Impact = 1.0

# Whether to normalize the State-Goal link before
# using it as a goal criterion
Use_Normalized_SGL = TRUE

# Set the compliance likelihood for each compliance
# mode in the Presence (CLP) and Absence (CLA) steps
if (CM == "PC_P_NC_A")
{
    CLP = 1 - P_Probability*(1 - Rule)
    if (CLP < 0)
        CLP = 0
    CLA = 0
} else if (CM == "FC_P_PC_A") {
    CLP = 1
    CLA = Rule
}

# The LG preference probability
LGP_Probability = as.numeric(LGPP)

# The weight of the State-Goal link as a criterion  
# in the goal score
SGL_Weight = as.numeric(SGLW)


#############################################################################
#############                                                   #############   
#############               Running the Simulation              #############
#############                                                   #############   
#############################################################################

if (SeparateDays)
{
    ResponseMatrix = data.frame()
} else
{
    ResponseMatrix = matrix(0, nrow = Num_Runs*2, ncol = 2)
    Column_Names = c("LGRD", "TimeTag")
    colnames(ResponseMatrix) = Column_Names
}


set.seed(Seed)
for (Run_Idx in 1:Num_Runs)
{
    # Number of compliance steps for counting the
    # number of executed SG actions
    Reset_Steps = 24
    
    # The parental rule is converted to a limit on
    # the number of SG actions the adolescent can 
    # execute in every Reset_Steps steps
    SG_Limit = floor(Reset_Steps*Rule)
    
    # The adolescent agent counts the number of
    # SG actions and compliance steps using these
    # variables
    SG_Counter = 0
    CS_Counter = 0
    
    
    # Choose a random point as the initial position of the agent
    # on either of the state spaces
    LG_SS_Position = sample.int(State_Range, 2)
    SG_SS_Position = sample.int(State_Range, 2)


    ############################################
    ########      Adolescent States     ########    
    ############################################
    
    # State-Goal link of the long-term goal
    SLG_Links = as.data.frame(matrix(0, State_Range, State_Range))
    
    # State-Goal link of the short-term goal
    SSG_Links = as.data.frame(matrix(0, State_Range, State_Range))
   
    
    
    ############################################
    ########       Observer state       ########    
    ############################################
    
    Actions_History = data.frame(matrix(ncol = 6, nrow = 0))
    Column_Names = c("Step", "Selected_Action", "Executed_Action", 
                     "Forced_Choice", "P_Presence", "Compliance_State")
    colnames(Actions_History) = Column_Names
    
    
    
    ############################################
    ########         Simulation         ########    
    ############################################
    
    print(paste("Run number", Run_Idx))
    Run_Simulation()
    Measure_Response_ForWTT()
}

library(xlsx)

OutputPath = "Output/SimulationData"

FilePath = paste(OutputPath, paste("RM_SingleExp_WTT", "xlsx", sep = "."), sep = "/")
SN1 = paste("Setup", SetupNum, sep = "=")
SN2 = paste("Seed", Seed, sep = "=")
write.xlsx(ResponseMatrix, file = FilePath, sheetName = paste(SN1, SN2, sep = "-"),
           append = TRUE, row.names = FALSE)
















