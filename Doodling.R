set.seed(1337)
a = runif(1000)
a
hist(a)

b = -log(1-a)/3 #exponential distribution with lambda=3
b
hist(b)


#Generating discrete probability distributions
U = runif(n, min = 0, max = 1) #code for generating uniform r.v

p = 0:5; #a bernoulli dist, tossing a coin simulation
U = runif(1, min = 0, max = 1);
X = (U < p);

#law of large numbers through coin toss simulation
n = 1000
U =runif(n, min=0, max=1);
toss = U < 0.6;
a = numeric(n+1);
avg = numeric(n);
for(i in 2:n+1)
{
  a[i] = a[i-1]+toss[i-1];
  avg[i-1] = a[i]/(i-1);
}
plot(1:n,avg[1:n], type ="l",lwd =5, col ="blue", ylab ="Proportion of Heads", xlab= "CoinTossNumber", cex.main =1.25, cex.lab =1.5, cex.axis =1.75)

#for generating arbitrary discrete distributions
#Solution Discovered (NOT inverse transform method)
P= c(0.10,0.80,0.33);
X = c(1,2,3);
counter = 1;
r = runif(1, min = 0, max = 1);
while(r > P[counter])
  counter =counter + 1;
end
X[counter]

#Poisson dist
Lambda = 2;
i = 0;
U = runif(1, min = 0, max = 1);
Y = -(1/Lambda)*log(U);
sum = Y ;
while(sum < 1)
{U = runif(1, min = 0, max = 1);
Y = -(1/Lambda)*log(U);
sum = sum + Y ;
i = i + 1;}
X = i




###{(b)Gaussian elimination with partial pivoting
gauss_elimination = function(matrix,diagonal = T){
  #This function performs the gaussian elimination for one column
  for_one_column = function(matrix, starting_row = 1, column = 1){
    for (i in (starting_row + 1):nrow(matrix)){
      L_table[i,column] <<-  matrix[i,column]/matrix[starting_row,column]
      matrix[i,] = matrix[i,] - matrix[i,column]/
        matrix[starting_row,column]*
        matrix[starting_row,]
    }
    return(matrix)
  }
  
  #This function changes a row of a matrix with another one
  change_lines = function(matrix,line_1,line_2,column_1 = 1,
                          column_2 =  ncol(matrix)){
    scapegoat = matrix[line_1,column_1:column_2]
    matrix[line_1,column_1:column_2] = matrix[line_2,column_1:column_2]
    matrix[line_2,column_1:column_2] = scapegoat
    return(matrix)
  }
  
  #This function checks if alternations need to be made
  checking_zeroes = function(matrix,starting_row,column){
    #If pilot is not zero
    if (matrix[starting_row,column] != 0){
      return ("No alternation needed")
      #If pilot is zero
    }else{
      row_to_change = 0
      for (i in starting_row:nrow(matrix)){
        if (matrix[i,column] != 0){
          row_to_change = i
          break
        }
      }
      #If both the pilot and all the elements below it are zero
      if (row_to_change == 0){
        return("Skip this column")
        #If the pilot is zero but at least one below it is not
      }else{
        return(c("Alternation needed",row_to_change))
      }
    }
  }
  
  #The main program
  row_to_work = 1 ; alternation_table = diag(nrow(matrix))
  L_table = diag(nrow(matrix))
  for (column_to_work in 1:ncol(matrix)){
    a = checking_zeroes(matrix,row_to_work,column_to_work)
    if (a[1] == "Alternation needed"){
      matrix = change_lines(matrix,as.numeric(a[2]),row_to_work)
      alternation_table = change_lines(alternation_table,row_to_work,
                                       as.numeric(a[2]))
      if (as.numeric(a[2]) != 1){
        L_table = change_lines(L_table,row_to_work,
                               as.numeric(a[2]),1,column_to_work - 1)
      }
    }
    if (a[1] == "Skip this column"){
      next()
    }
    matrix = for_one_column(matrix,row_to_work,column_to_work)
    if(row_to_work + 1 == nrow(matrix)){
      break
    }
    row_to_work = row_to_work + 1
  }
  if (diagonal == FALSE){
    return(list("P" = as.fractions(alternation_table),
                "L" = as.fractions(L_table),
                "U : The matrix after the elimination"=as.fractions(matrix)))
  }else{
    D = diag(nrow(matrix))
    diag(D) = diag(matrix)
    for (i in 1:nrow(D)){
      matrix[i,] = matrix[i,]/diag(D)[i]
    }
    return(list("P" = as.fractions(alternation_table),
                "L" = as.fractions(L_table),
                "D" = as.fractions(D),
                "U : The matrix after the elimination"=as.fractions(matrix)))
  }
}

#Example 1
A <- matrix(c(2, 1, -1,
              -3, -1, 2,
              -2,  1, 2), 3, 3, byrow=TRUE)
b <- c(8, -11, -3)
solve(A,b) #[1]  2  3 -1
gauss_elimination(A, b)
gauss_elimination(A, b, verbose=TRUE, fractions=TRUE)
gauss_elimination(A, b, verbose=TRUE, fractions=TRUE, latex=TRUE)

# determine whether matrix is solvable
gauss_elimination(A, numeric(3)) 
#}