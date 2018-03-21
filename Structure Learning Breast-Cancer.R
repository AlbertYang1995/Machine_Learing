mydata <- read.table("D:\\workdir\\breast_cancer_wisconsin_data.txt", header = F, sep =",")
#------------delete the ID colcum--------------
mydata <- mydata[, -1]
#------------transform the data--------------
mydata <- data.frame(matrix(as.numeric(unlist(mydata)),ncol = length(mydata[1,])))
amat <- matrix(0, nrow = 10, ncol = 10)
tempamat <- matrix(0, nrow = 6, ncol = 6)
best_score = -10000000
fathernum <- matrix(0, nrow = 1, ncol = 10)
father <- matrix(0, nrow = 1, ncol = 5)
toponode <- matrix(0, nrow = 1, ncol = 10)
nodenum = 0
formula1 = 0
formula2 = 0
samplenum = 0
samplenum2 = 0
best_op1 = 0
best_op2 = 0

sink("testtt.txt")






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







for (hp in 1:15) {
  cat("------------begin the NO.", hp, "iter------------\n")
  
  
  #***************************adding an arc*****************************
  for (mi in 1:10) {
    for (mj in 1:10) {
      if (mi == mj | amat[mi, mj] == 1 | amat[mj, mi] == 1) next
      
      #------------initialization process----------------
      cd = 0
      nodenum = 0
      iter = 1
      toponode <- matrix(0, nrow = 1, ncol = 10)
      #--------------------------------------------------
      
      amat[mi, mj] = 1
      # cat(amat, "\n")
      tempamat = amat
      cat("-------------------------------------\n")
      cat("trying to add arc ", mi," to ", mj, "\n")
      # cat(amat[mi, mj],"\n")
      
      #--------------this is topological-sort--------------      
      repeat {
        
        for (dd in 1:10) {
          tike = 0
          for (hc in 1:10) {
            if (toponode[1, hc] == dd) {
              tike = 1
              break
            }
            
          }
          if (tike) next
          # cat("this time sort node", dd, "\n")
          for (mp in 1:10) {
            if (dd == mp) next
            if (tempamat[mp, dd] == 0) {
              level = 0
              # cat("there is no arc between", mp, "to", dd, "\n")
            }
            
            else {
              # cat("there exist the arc", mp, "to", dd, "\n")
              level = 1
              break
            }
          }
          if (level) next
          if (level == 0) {
            cd = cd + 1
            toponode[1, cd] = dd
            # cat(toponode[1, cd], "\n")
            nodenum = nodenum + 1
            for (jb in 1:10) {
              if (jb == dd) next
              else {
                tempamat[dd, jb] = 0
                # cat(tempamat[dd, jb], "\n")
              } 
              
            }
          }
          
        }
        iter = iter + 1
        # cat(toponode, "\n")
        # cat(cd, "\n")
        # cat("nodenum is", nodenum, "\n")
        if (nodenum == 10) {
          sign = 0
          break
        }
        if (iter > 10 & nodenum != 10) {
          sign = 1
          amat[mi, mj] = 0
          cat("-----------------------------------\n")
          cat("we will not add the arc", mi, "to", mj, "because of introducing cycle\n")
          break
        }
        
      }
      if (sign) next
      
      #--------------if add an arc successfully--------------
      
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
        # cat("----------------------------------------------\n")
        # cat("node ",i," father number is ", fathernum[1, i],"\n")
        for (j in 1:10^fathernum[1, i]) {
          for (k in 1:10) {
            med1 = i
            med2 = j
            med3 = k
            # cat("------------------------------------------\n")
            # cat("i is", med1, "; j is", med2, "; k is", med3, "\n")
            formula1 = formula1 + formula_1(med1, med3)
            # cat("formula1 is ", formula1, "\n")
            formula2 = formula2 + formula_2(med1)
            # cat("formula2 is ", formula2, "\n")
          }
        }
        father <- matrix(0, nrow = 1, ncol = 5)
      }
      reference.score = formula1 - log10(nrow(mydata)) / 2 * formula2
      cat("the current network score is ",reference.score,"\n")
      if (best_score < reference.score) {
        best_score = reference.score
        best_op1 = mi
        best_op2 = mj
        piggy = 1
      }
      amat[mi, mj] = 0
      fathernum <- matrix(0, nrow = 1, ncol = 10)
      formula1 = 0
      formula2 = 0
    }
    
  }
  
  
  
  #******************delete an arc**********************
  for (mi in 1:10) {
    for (mj in 1:10) {
      
      if (mi == mj | amat[mi, mj] != 1) next
      if (amat[mi, mj] == 1) {
        amat[mi, mj] = 0
        cat("-------------------------------------\n")
        cat("trying to delete arc ", mi," to ", mj, "\n")
      }
      
      #--------------if delete an arc successfully--------------
      #--------------caculate the current score-----------------
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
        # cat("----------------------------------------------\n")
        # cat("node ",i," father number is ", fathernum[1, i],"\n")
        for (j in 1:10^fathernum[1, i]) {
          for (k in 1:10) {
            med1 = i
            med2 = j
            med3 = k
            # cat("------------------------------------------\n")
            # cat("i is", med1, "; j is", med2, "; k is", med3, "\n")
            formula1 = formula1 + formula_1(med1, med3)
            # cat("formula1 is ", formula1, "\n")
            formula2 = formula2 + formula_2(med1)
            # cat("formula2 is ", formula2, "\n")
          }
        }
        father <- matrix(0, nrow = 1, ncol = 5)
      }
      reference.score = formula1 - log10(nrow(mydata)) / 2 * formula2
      cat("the current network score is ",reference.score,"\n")
      if (best_score < reference.score) {
        best_score = reference.score
        best_op1 = mi
        best_op2 = mj
        piggy = 2
      }
      amat[mi, mj] = 1
      fathernum <- matrix(0, nrow = 1, ncol = 10)
      formula1 = 0
      formula2 = 0
    }
    
  }
  
  
  #*******************reverse an arc**********************
  for (mi in 1:10) {
    for (mj in 1:10) {
      if (mi == mj | amat[mi, mj] == 0) next
      
      amat[mi, mj] = 0
      amat[mj, mi] = 1
      
      #------------initialization process----------------
      cd = 0
      nodenum = 0
      iter = 1
      toponode <- matrix(0, nrow = 1, ncol = 10)
      #--------------------------------------------------
      
      # cat(amat, "\n")
      tempamat = amat
      cat("-------------------------------------\n")
      cat("trying to reverse arc ", mi," to ", mj, "\n")
      # cat(amat[mi, mj],"\n")
      
      #--------------this is topological-sort--------------      
      repeat {
        
        for (dd in 1:10) {
          tike = 0
          for (hc in 1:10) {
            if (toponode[1, hc] == dd) {
              tike = 1
              break
            }
            
          }
          if (tike) next
          # cat("this time sort node", dd, "\n")
          for (mp in 1:10) {
            if (dd == mp) next
            if (tempamat[mp, dd] == 0) {
              level = 0
              # cat("there is no arc between", mp, "to", dd, "\n")
            }
            
            else {
              # cat("there exist the arc", mp, "to", dd, "\n")
              level = 1
              break
            }
          }
          if (level) next
          if (level == 0) {
            cd = cd + 1
            toponode[1, cd] = dd
            # cat(toponode[1, cd], "\n")
            nodenum = nodenum + 1
            for (jb in 1:10) {
              if (jb == dd) next
              else {
                tempamat[dd, jb] = 0
                # cat(tempamat[dd, jb], "\n")
              } 
              
            }
          }
          
        }
        iter = iter + 1
        # cat(toponode, "\n")
        # cat(cd, "\n")
        # cat("nodenum is", nodenum, "\n")
        if (nodenum == 10) {
          sign = 0
          break
        }
        if (iter > 10 & nodenum != 10) {
          sign = 1
          amat[mi, mj] = 1
          amat[mj, mi] = 0
          cat("-----------------------------------\n")
          cat("we will not reverse the arc", mi, "to", mj, "because of introducing cycle\n")
          break
        }
        
      }
      if (sign) next
      
      #--------------if reverse an arc successfully--------------
      #--------------then caculate the current score-------------
      
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
        # cat("----------------------------------------------\n")
        # cat("node ",i," father number is ", fathernum[1, i],"\n")
        for (j in 1:10^fathernum[1, i]) {
          for (k in 1:10) {
            med1 = i
            med2 = j
            med3 = k
            # cat("------------------------------------------\n")
            # cat("i is", med1, "; j is", med2, "; k is", med3, "\n")
            formula1 = formula1 + formula_1(med1, med3)
            # cat("formula1 is ", formula1, "\n")
            formula2 = formula2 + formula_2(med1)
            # cat("formula2 is ", formula2, "\n")
          }
        }
        father <- matrix(0, nrow = 1, ncol = 5)
      }
      reference.score = formula1 - log10(nrow(mydata)) / 2 * formula2
      cat("the current network score is ",reference.score,"\n")
      if (best_score < reference.score) {
        best_score = reference.score
        best_op1 = mi
        best_op2 = mj
        piggy = 3
      }
      amat[mi, mj] = 1
      amat[mj, mi] = 0
      fathernum <- matrix(0, nrow = 1, ncol = 10)
      formula1 = 0
      formula2 = 0
    }
    
  }
  
  
  
  #-------------------the final operation--------------------------
  if (piggy == 1) {
    cat("the best operation is adding arc ", best_op1, "to", best_op2, "\n") 
    amat[best_op1, best_op2] = 1
  }
  
  if (piggy == 2) {
    cat("the best operation is deleting arc ", best_op1, "to", best_op2, "\n") 
    amat[best_op1, best_op2] = 0
  }
  
  if (piggy == 3) {
    cat("the best operation is reversing arc ", best_op1, "to", best_op2, "\n") 
    amat[best_op1, best_op2] = 0
    amat[best_op1, best_op2] = 1
  }
  
  
  best_score = -10000000        #and when you start a new iter, then the best score should be set as zero
}


sink()