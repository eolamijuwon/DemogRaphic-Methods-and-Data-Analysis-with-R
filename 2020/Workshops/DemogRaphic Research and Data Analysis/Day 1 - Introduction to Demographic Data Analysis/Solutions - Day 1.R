
## Solutions

#### Exercise 1A.1


## Find the difference between 125 and its 3/5. 

    diff_quiz <- function (num = 125, x1 = 3/5){
      
      prod = x1 * num
      diff = num - prod
      
      print(paste0("The difference between ",
                   num, " and it's ",
                   x1, " is = ", diff)) 
    
    }

    diff_quiz(num = 125, x1 = 3/5)
    
## Using the same function, find the difference between 50 and it's 1/5.
    
    diff_quiz(num = 50, x1 = 1/5)



## Find a number which is greater than 17 as much as it is less than 31.
    
    quiz <- function (x1 = 17, x2 = 31){
      
      num <- (x1 + x2)/2
      print (
        paste0("A number that is greater than ", 
               x1, " as much as is greater than ",
               x2, " is: ", num))
      
    }
    
    quiz(x1 = 17, x2 = 31)

##  Using the same function, find the number 
##  which is greater than 16 as much as it is less than 50.
    
    quiz(16, 50)
