

# Input Variables
# Specify all minority/comparison groups, Majority/reference group will be specified later
races <- c("Black","Asian","NH.PI","AI.AN","TwoorMore")
# Specify names of columns that contain scores to be summarized
scores <- c('ScorVar1', 'ScorVar2', 'ScorVar3', 'ScorVar4')


# Function
cohensD <- function(data,group_var,groups,scores,ref_group){
  output <- matrix(ncol=length(scores), nrow=length(groups))
  for(i in groups){
    for(j in scores){
      
      m1 <- mean(data.matrix(data[,j][which(data[group_var]==i), ]),na.rm = T)
      m2 <- mean(data.matrix(data[,j][which(data[group_var]==ref_group), ]),na.rm = T)
      s1 <- sd(data.matrix(data[,j][which(data[group_var]==i), ]),na.rm = T)
      s2 <- sd(data.matrix(data[,j][which(data[group_var]==ref_group), ]),na.rm = T)
      n1 <- length(as.matrix(data[,j][which(data[group_var]==i), ]))
      n2 <- length(as.matrix(data[,j][which(data[group_var]==ref_group), ]))
      lx <- n1- 1
      ly <- n2- 1
      md  <- m1-m2        ## mean difference (numerator)
      csd <- lx * s1^2 + ly * s2^2
      csd <- csd/(lx + ly)
      csd <- sqrt(csd)                     ## common sd computation
      cd  <- md/csd; # Cohens D
      rownames(output) <- groups
      colnames(output) <- scores
      output[i,j] <- cd
    }
    }
  return(output)
  }



# Run the function
# Data assumes a wide format where all demographic variables are string labels in the same column
# Scores are expected to be in several columns
cohensD(data,'race_group',races,scores,"White")




# Smaller Version for demonstration ---------------------------------------
# Gender
cohensD <- function(data,group_var,groups,scores,ref_group){
  output <- matrix(ncol=length(scores), nrow=length(groups))
  for(i in groups){
    for(j in scores){
      
      m1 <- mean(data.matrix(data[,j][which(data[group_var]==i), ]),na.rm = T)
      m2 <- mean(data.matrix(data[,j][which(data[group_var]==ref_group), ]),na.rm = T)
      s1 <- sd(data.matrix(data[,j][which(data[group_var]==i), ]),na.rm = T)
      s2 <- sd(data.matrix(data[,j][which(data[group_var]==ref_group), ]),na.rm = T)
      n1 <- length(as.matrix(data[,j][which(data[group_var]==i), ]))
      n2 <- length(as.matrix(data[,j][which(data[group_var]==ref_group), ]))
      lx <- n1- 1
      ly <- n2- 1
      md  <- m1-m2        ## mean difference (numerator)
      csd <- lx * s1^2 + ly * s2^2
      csd <- csd/(lx + ly)
      csd <- sqrt(csd)                     ## common sd computation
      cd  <- md/csd; # Cohens D
      rownames(output) <- groups
      colnames(output) <- scores
      output[i,j] <- cd
      # output <- matrix(output, nrow = length(groups),ncol = length(scores),byrow = T)
      
    }}
  return(output)
}

# races <- c("Black")
sex <- c("F")
scores <- c('ScorVar1', 'ScorVar2', 'ScorVar3', 'ScorVar4')

cohensD(data,'SEXA',sex,scores,"M")