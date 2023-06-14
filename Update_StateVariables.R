#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This submodel updates the State-Goal links.

Update_StateVariables <- function()
{
 
    ###########################################################################
    # Each goal can form a link with each of the states in the agent's state  #
    # space. This link is the value of the states which determine the shape   #
    # of "valleys".                                                           #
    #                                                                         #
    # We have implemented the State_Goal links as a State_Range by            #
    # State_Range matrix representing the agent's state space. The matrices   #
    # are initialized with zero, indicating no initial link between the goals #
    # and the positions in the state space.                                   #
    #                                                                         #
    # SLG_Links contains the links to the LG and SSG_Links contains the       #
    # links to the SG.                                                        #
    ###########################################################################
    
    ###########################################################################
    # At each step, we update the element of the corresponding state space    #
    # matrix specified by the SS_Position by subtracting the internal         #
    # feedback of volition (IF_Volition) from the states current value.       #
    #                                                                         #
    # The IF_Volition is either set to the Self_Initiation_Impact constant,   #
    # if the action is self-selected, or to the value of the agent's          #
    # Perceived_Autonomy, if the action is forced.                            #
    ###########################################################################
    
    if (Update_StateSpace)
    {
        Selected_Action = EA_Output[[1]]
        Executed_Action = EA_Output[[2]]
        Forced_Choice = EA_Output[[3]]
        
        # Calculate the volitional feedback
        if (Forced_Choice == FALSE)
        {
            # Forced_Choice is FALSE and therefore, the agent has  
            # executed the self-selected action. 
            IF_Volition = Self_Initiation_Impact
        }
        else
        {
            # Forced_Choice is TRUE and therefore, the executed 
            # action differs from the selected one. 
            IF_Volition = Perceived_Autonomy
            
        }
        
        # Get the agent's position on the LG state space
        LG_Sx = LG_SS_Position[1]
        LG_Sy = LG_SS_Position[2]
        
        # Get the agent's position on the SG state space
        SG_Sx = SG_SS_Position[1]
        SG_Sy = SG_SS_Position[2]
        
        # Update the corresponding state space
        if (Executed_Action == LG_Idx)
        {
            SLG_Links[LG_Sx, LG_Sy] <<- SLG_Links[LG_Sx, LG_Sy] - IF_Volition
        }
        else
        {
            SSG_Links[SG_Sx, SG_Sy] <<- SSG_Links[SG_Sx, SG_Sy] - IF_Volition
        }
    }
}












