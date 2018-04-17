# Define a function that adds up numbers and return sum - in R
numberAdding <- function(start, end)
{
   sum(start, end)
}
numberAdding(4, 13)
#=====================
# define a function that adds up based on target and size
numberAdding <- function(start, end, target)
{ 
	paste("The output:", start, end, target)
   	if(target != start + end AND start != target - end)
   	{
   		"start and end not equal to Target! Pls try again"
    	}
}
numberAdding(4, 13, 5)
