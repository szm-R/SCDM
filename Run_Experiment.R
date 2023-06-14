#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This code simulates the model with a specific set of parameters
#              and records the ratio of executed LG actions in the Response Steps.

debugSource("Run_Simulation.R")
debugSource("Measure_Response.R")

Run_Experiment <- function()
{
    #############################################################################
    #############                                                  ##############   
    #############     Auxiliary Variables And Model Parameters     ##############
    #############                                                  ##############   
    #############################################################################
    
    DesignPoint = DesignMatrix_Value[DP,]
    
    ############################################
    ####  Auxiliary Variables and Constants ####    
    ############################################
    
    # LG and SG indices
    LG_Idx <<- 1
    SG_Idx <<- 2
    
    
    ############################################
    ########         Parameters         ########    
    ############################################
    
    ############################################   
    ############################################
    
    ## General
    
    # Number of steps in the main body of simulation
    SimDuration <<- as.numeric(DesignPoint[SD])
    
    # Number of steps where we calculate the model 
    # response (the LGRD)
    Response_Steps <<- 312
    
    
    ############################################   
    ############################################
    
    ## Parent agent
    
    # Parental rule
    Rule = PR
    
    # Parental presence probability
    P_Probability <<- as.numeric(DesignPoint[PP])
    

    ############################################   
    ############################################
    
    ## Adolescent agent
    
    # The Percieved Autonomy decreases as the P_Probability
    # increases and starts lower for strict rules
    Perceived_Autonomy <<- Rule - P_Probability*(1 - Rule)
    if (Perceived_Autonomy < 0)
        Perceived_Autonomy <<- 0
    
    # The range of the state of space
    State_Range <<- 101
    
    # Choose a random point as the initial position of the agent
    # on either of the state spaces 
    LG_SS_Position <<- sample.int(State_Range, 2)
    SG_SS_Position <<- sample.int(State_Range, 2)
    
    # The feedback value of a self-initiated action
    Self_Initiation_Impact <<- 1.0
    
    # Whether to normalize the State-Goal link before
    # using it as a goal criterion
    Use_Normalized_SGL <<- TRUE
    
    # Set the compliance likelihood for each compliance
    # mode in the Presence (CLP) and Absence (CLA) steps
    if (DesignPoint[CM] == "PC_P_NC_A")
    {
        # In this mode, the CLP decreases as the the
        # P_Probability increases and the CLA is fixed
        # at zero
        
        CLP <<- 1 - P_Probability*(1 - Rule)
        if (CLP < 0)
            CLP <<- 0
        CLA <<- 0
        
    } else if (DesignPoint[CM] == "FC_P_PC_A") {
        
        # In this mode, the CLP is always 1 and the 
        # CLA equals the Rule
        
        CLP <<- 1
        CLA <<- Rule
    }
    
    # Number of compliance steps for counting the
    # number of executed SG actions
    Reset_Steps <<- 24
    
    # The parental rule is converted to a limit on
    # the number of SG actions the adolescent can 
    # execute in every Reset_Steps steps
    SG_Limit <<- floor(Reset_Steps*Rule)
    
    # The adolescent agent counts the number of
    # SG actions and compliance steps using these
    # variables
    SG_Counter <<- 0
    CS_Counter <<- 0
    
    
    # The LG preference probability
    LGP_Probability <<- as.numeric(DesignPoint[LGPP])
    
    # The weight of the State-Goal link as a criterion  
    # in the goal score
    SGL_Weight <<- as.numeric(DesignPoint[SGLW])
    
    
    
    #############################################################################
    #############                                                   #############   
    #############            Initializing State Variables           #############
    #############                                                   #############   
    #############################################################################
    
    
    ############################################
    ########      Adolescent States     ########    
    ############################################
    
    # State-Goal link of the long-term goal
    SLG_Links <<- as.data.frame(matrix(0, State_Range, State_Range))
    
    # State-Goal link of the short-term goal
    SSG_Links <<- as.data.frame(matrix(0, State_Range, State_Range))
    
    
    ############################################
    ########       Observer state       ########    
    ############################################
    
    Actions_History <<- data.frame(matrix(ncol = 6, nrow = 0))
    Column_Names = c("Step", "Selected_Action", "Executed_Action", 
                     "Forced_Choice", "P_Presence", "Compliance_State")
    colnames(Actions_History) <<- Column_Names
    
    
    
    #############################################################################
    #############                                                   #############   
    #############               Running the Simulation              #############
    #############                                                   #############   
    #############################################################################
    
    Run_Simulation()
    
    LGR = Measure_Response()
    
    return(LGR)
}























