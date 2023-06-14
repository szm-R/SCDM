#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This function generates the final output of the 
#              Execute_Action submodel.

Generate_Output <- function(Selected_Action, Approval_Status)
{
    ###########################################################################
    # When the parent agent disapproves the adolescent's action               #
    # (Approval_Status == FALSE), it has to choose another one.               #
    #                                                                         #
    # In the current version, there are only two goals, one LG and one SG:    #
    #                                                                         #
    #    - In the case of parental approval, the Executed_Action would be     #
    #      the same as the Selected_Action.                                   #
    #    - Otherwise, we set the Executed_Action to LG and the Forced_Choice  #
    #      to TRUE.                                                           #
    ###########################################################################
    
    Executed_Action = Selected_Action
    Forced_Choice = FALSE
    if (Approval_Status == FALSE)
    {
        # The selected action does not satisfy the parent agent's rule
        Executed_Action = LG_Idx
        Forced_Choice = TRUE
    }
    
    return(list(Selected_Action, Executed_Action, Forced_Choice))
}