mydata <- read.table("D:\\workdir\\breast_cancer_wisconsin_data.txt", header = F, sep =",")
#------------delete the ID colcum--------------
mydata <- mydata[, -1]
#------------transform the data--------------
mydata <- data.frame(matrix(as.numeric(unlist(mydata)),ncol = length(mydata[1,])))
amat <- matrix(0, nrow = 10, ncol = 10)
best_score = -10000000
fathernum <- matrix(0, nrow = 1, ncol = 10)
father <- matrix(0, nrow = 1, ncol = 5)
debug = 0
formula1 = 0
formula2 = 0
samplenum = 0
samplenum2 = 0

sink("test.txt")







#------------divide BIC into 2 function-------------
formula_1 <- function (i, k) {
  if (fathernum[1, i] == 0) {
    samplenum = 0
    for (re in 1:nrow(mydata)) {
      if (mydata[re, i] == k)
        samplenum = samplenum + 1
    }
    if (samplenum == 0)
      count = 0
    else count = samplenum * log10(samplenum/nrow(mydata))
    return(count)
  }
  if (fathernum[1, i] == 1) {
    samplenum = 0
    samplenum2 = 0
    for (re in 1:nrow(mydata)) {
      for (fa in 1:10) {
        if (mydata[re, father[1, 1]] == fa & mydata[re, i] == k)  
          samplenum = samplenum + 1
        if (mydata[re, father[1, 1]] == fa)
          samplenum2 = samplenum2 + 1
      }
    }
    # cat("samplenum is ", samplenum, "\n")
    # cat("samplenum2 is ", samplenum2, "\n")
    if (samplenum == 0)
      count = 0
    else count = samplenum * log10(samplenum / samplenum2)
    return(count)
  }
  if (fathernum[1, i] == 2) {
    samplenum = 0
    samplenum2 = 0
    for (re in 1:nrow(mydata)) {
      for (fa1 in 1:10) {
        for (fa2 in 1:10) {
          if (mydata[re, father[1, 1]] == fa1 & mydata[re, father[1,2]] == fa2 & mydata[re, i] == k)
            samplenum = samplenum + 1
          if (mydata[re, father[1, 1]] == fa1 & mydata[re, father[1,2]] == fa2)
            samplenum2 = samplenum + 1
        }
      }
    }
    # cat("samplenum is ", samplenum, "\n")
    # cat("samplenum2 is ", samplenum2, "\n")
    if (samplenum == 0)
      count = 0
    else count = samplenum * log10(samplenum / samplenum2)
    return(count)
  }
  if (fathernum[1, i] == 3) {
    samplenum = 0
    samplenum2 = 0
    for (re in 1:nrow(mydata)) {
      for (fa1 in 1:10) {
        for (fa2 in 1:10) {
          for (fa3 in 1:10) {
            if (mydata[re, father[1, 1]] == fa1 & mydata[re, father[1,2]] == fa2 & mydata[re, father[1, 3]] == fa3 & mydata[re, i] == k)
              samplenum = samplenum + 1
            if (mydata[re, father[1, 1]] == fa1 & mydata[re, father[1,2]] == fa2 & mydata[re, father[1, 3]] == fa3)
              samplenum2 = samplenum + 1
          }
        }
      }
    }
    # cat("samplenum is ", samplenum, "\n")
    # cat("samplenum2 is ", samplenum2, "\n")
    if (samplenum == 0)
      count = 0
    else count = samplenum * log10(samplenum / samplenum2)
    return(count)
  }
}

formula_2 <- function (i) {
  return((10 - 1) * (10^fathernum[1, i]))
}









#----------------adding an arc------------------
for (hp in 1:10) {
  cat("----------------begin the NO.", hp, "iter---------------\n")
  for (mi in 1:10) {
    for (mj in 1:10) {
      if (mi == mj | amat[mi, mj] == 1 | amat[mj, mi] == 1) next
      else {                                                           #还需要考虑是否构成环
        amat[mi, mj] = 1
        debug = 1
        cat("-------------------------------------\n")
        cat("trying to add arc ", mi," to ", mj, "\n")
        # cat(amat[mi, mj],"\n")
      }
      #--------------if add an arc successfully--------------
      if(debug == 1) {
        #------------BIC function------------
        for (i in 1:10) {
          #count every node father number, and record it
          t = 1
          for (n in 1:10) {
            if (amat[n, i] == 1) {
              fathernum[1, i] = fathernum[1, i] + 1
              father[1, t] = n
              t = t + 1
            }
          }
          cat("----------------------------------------------\n")
          # cat("node ",i," father number is ", fathernum[1, i],"\n")
          for (j in 1:10^fathernum[1, i]) {
            for (k in 1:10) {
              med1 = i
              med2 = j
              med3 = k
              cat("------------------------------------------\n")
              # cat("i is", med1, "; j is", med2, "; k is", med3, "\n")
              formula1 = formula1 + formula_1(med1, med3)
              cat("formula1 is ", formula1, "\n")
              formula2 = formula2 + formula_2(med1)
              cat("formula2 is ", formula2, "\n")
            }
          }
          father <- matrix(0, nrow = 1, ncol = 5)
        }
        debug = 0
        reference.score = formula1 - log10(nrow(mydata)) / 2 * formula2
        cat("the current network score is ",reference.score,"\n")
        if (best_score < reference.score) {
          best_score = reference.score
          best_op1 = mi
          best_op2 = mj
        }
        amat[mi, mj] = 0
        fathernum <- matrix(0, nrow = 1, ncol = 10)
        formula1 = 0
        formula2 = 0
      }
      
    }
    
  }
  cat("the best operation is adding arc ", best_op1, "to", best_op2, "\n") 
  amat[best_op1, best_op2] = 1 
  best_score = -10000000        #and when you start a new iter, then the best score should be set as zero

}



sink()

































# #------------divide BIC into 2 function-------------
# formula_1 <- function (i, k) {
#   if (fathernum[1, 1] == 0) {
#     samplenum = 0
#     for (re in 1:nrow(mydata)) {
#       if (mydata[re, 1] == 1)
#         samplenum = samplenum + 1
#     }
#     cat(samplenum)
#     return(samplenum * log10(samplenum/nrow(mydata)))
#   }
#   # if (fathernum[1, i] == 1) {
#   #   samplenum2 = 0
#   #   for (re in 1:nrow(mydata)) {
#   #     if (mydata[re, father] == j && mydata[re, i] == k)
#   #       samplenum = samplenum + 1
#   #     if (mydata[re, father] == j)
#   #       samplenum2 = samplenum2 + 1
#   #   }
#   #   return(samplenum * log(samplenum / samplenum2))
#   # }
# }
# 
# formula_2 <- function (i) {
#   return(log10(nrow(mydata)) / 2 * (10 - 1) * (10^fathernum[1, i]) * 10)
# }


















