#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This function enforces the parental Rule under the 
#              Execute_Action submodel.

Enforce_Parental_Rule <- function(Selected_Action)
{
  
    ###########################################################################
    # This function outputs a binary variable in response to the adolescent   #
    # agent's selected action:                                                #
    #                                                                         #
    #     - TRUE indicates that the parents "approve" the adolescent's        #
    #       selected action.                                                  #
    #     - FALSE indicates that the parents "disapprove" the adolescent's    #
    #       selected action.                                                  #
    ###########################################################################
    

    # First, we check for compliance.
    # In steps with a False compliance state, the function approves the action
    # without first checking it against the parental rule
    
    if (!Compliance_State)
        return(TRUE)
    
  
    ###########################################################################
    # If the adolescent agent is in the compliance mode, it should check its  #
    # selected action against the parental rule                               #
    #                                                                         #
    # The parental rule is converted to a limit on the number of SG actions   #
    # the adolescent can execute in every Reset_Steps steps where             #
    # Compliance_State is true                                                #
    #                                                                         #
    # The agent counts the SG actions using SG_Counter and compliance steps   #
    # using CS_Counter                                                        #
    ###########################################################################
  
  
    # First, we reset the SG_Counter and CS_Counter if the latter has passed
    # Reset_Steps. Otherwise, CS_Counter is incremented.

    if (CS_Counter == Reset_Steps)
    {
        SG_Counter <<- 0
        CS_Counter <<- 1
    }
    else
    {
        CS_Counter <<- CS_Counter + 1
    }
      
    # Next, we check the selected goal's type: 
    #
    #    - If it is an LG no further steps are required and the action is approved. 
    #    - Else, if the selected goal is an SG, we check the SG_Counter against SG_Limit:
    #        - If SG_Counter has not reached SG_Limit, the action is approved,
    #        - Otherwise, the action is disapproved.
    
    if (Selected_Action == LG_Idx)
        return(TRUE)
    else 
    {
        if (SG_Counter < SG_Limit)
        {
            SG_Counter <<- SG_Counter + 1
            return(TRUE)
        }
        else
            return(FALSE)
    }
}