#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This submodel assigns a TRUE/FAlSE value to the Presence variable.

Determine_Presence <- function()
{
    Presence = sample(c(FALSE, TRUE), size = 1,  
                        prob = c((1-P_Probability), P_Probability))
    
    return(Presence)
}