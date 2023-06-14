#### In the Name of Allah
# Author: Seyyedeh Zeinab Mousavi
# Description: This submodel chooses the next state space position.

debugSource("Check_LocalMin.R")

Choose_Next_SSP <- function()
{
    ###########################################################################
    # To change the SS_Position, first a random process draws the coordinates #
    # of the new position from a normal distribution.                         #
    #                                                                         #
    # Next, at the destination point, we check the surrounding positions for  #
    # local minimums. The search continues (recursively) until one of these   #
    # conditions are satisfied:                                               #
    #                                                                         #
    #    - The position is lower than all its surrounding points.             #
    #    - The destination is a plateau where all points have equal values.   #
    ###########################################################################
    
    
    minVal <- 1
    maxVal <- State_Range
    sd = (maxVal - minVal)/6
    mean_x = (maxVal - minVal)/2
    mean_y = (maxVal - minVal)/2
    
    
    ###########################################################################
    ##########                                                       ##########   
    ##########  Specifying the next position in the LG state space   ##########
    ##########                                                       ##########   
    ###########################################################################
    
    # Generate numbers from min to max
    Temp_DestPoint = c(0,0)
    Temp_DestPoint[1] <- floor(rnorm(1, mean = mean_x, sd = sd))
    Temp_DestPoint[2] <- floor(rnorm(1, mean = mean_y, sd = sd))
    
    # Trim the out-of-bounds values
    Temp_DestPoint[1] <- pmax(minVal, Temp_DestPoint[1])
    Temp_DestPoint[1] <- pmin(maxVal, Temp_DestPoint[1])
    Temp_DestPoint[2] <- pmax(minVal, Temp_DestPoint[2])
    Temp_DestPoint[2] <- pmin(maxVal, Temp_DestPoint[2])
    
    # Search recursively for the closest local minimum
    Dest_Point = Check_LocalMin(Temp_DestPoint[1], 
                                Temp_DestPoint[2], 
                                SLG_Links)
    
    LG_SS_Position[1] <<- Dest_Point[[1]]
    LG_SS_Position[2] <<- Dest_Point[[2]]
    
    
    ###########################################################################
    ##########                                                       ##########   
    ##########  Specifying the next position in the SG state space   ##########
    ##########                                                       ##########   
    ###########################################################################
    
    # Generate numbers from min to max
    Temp_DestPoint = c(0,0)
    Temp_DestPoint[1] <- floor(rnorm(1, mean = mean_x, sd = sd))
    Temp_DestPoint[2] <- floor(rnorm(1, mean = mean_y, sd = sd))
    
    # Trim the out-of-bounds values
    Temp_DestPoint[1] <- pmax(minVal, Temp_DestPoint[1])
    Temp_DestPoint[1] <- pmin(maxVal, Temp_DestPoint[1])
    Temp_DestPoint[2] <- pmax(minVal, Temp_DestPoint[2])
    Temp_DestPoint[2] <- pmin(maxVal, Temp_DestPoint[2])
    
    # Search recursively for the closest local minimum
    Dest_Point = Check_LocalMin(Temp_DestPoint[1], 
                                Temp_DestPoint[2], 
                                SSG_Links)
    
    SG_SS_Position[1] <<- Dest_Point[[1]]
    SG_SS_Position[2] <<- Dest_Point[[2]]
}