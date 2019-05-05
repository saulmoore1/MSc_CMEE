#!/usr/bin/env R
rm(list=ls())
graphics.off()

# Snake Puzzle (protein folding) contact matrices

# Wrangling the contact matrix data
Data001 <- read.csv("../Data/Contact_Matrices/Contact-Matrix_ToyFolding001.dat", sep=" ", header=FALSE)
Data002 <- read.csv("../Data/Contact_Matrices/Contact-Matrix_ToyFolding002.dat", sep=" ", header=FALSE)
Data003 <- read.csv("../Data/Contact_Matrices/Contact-Matrix_ToyFolding003.dat", sep=" ", header=FALSE)
Data004 <- read.csv("../Data/Contact_Matrices/Contact-Matrix_ToyFolding004.dat", sep=" ", header=FALSE)
Data005 <- read.csv("../Data/Contact_Matrices/Contact-Matrix_ToyFolding005.dat", sep=" ", header=FALSE)
Data006 <- read.csv("../Data/Contact_Matrices/Contact-Matrix_ToyFolding006.dat", sep=" ", header=FALSE)
Data007 <- read.csv("../Data/Contact_Matrices/Contact-Matrix_ToyFolding007.dat", sep=" ", header=FALSE)
Data008 <- read.csv("../Data/Contact_Matrices/Contact-Matrix_ToyFolding008.dat", sep=" ", header=FALSE)

Transform_Matrix <- function(ContactMatrix)
{
  Monomer1 <- c()
  Monomer2 <- c()
  Contact <- c()
  for(i in 1:64) {
    for(j in 1:i) {
      Monomer1 <- c(Monomer1, i)
      Monomer2 <- c(Monomer2, j)
      Contact <- c(Contact, ContactMatrix[j,i])
    }
  }
  Network <- data.frame(Monomer1, Monomer2, Contact)
  Network <- Network[Network[,1] != Network[,2],]
  Network <- Network[Network[,3] == 1,]
  return(Network)
}

TransformedData001 <- Transform_Matrix(Data001)
TransformedData002 <- Transform_Matrix(Data002)
TransformedData003 <- Transform_Matrix(Data003)
TransformedData004 <- Transform_Matrix(Data004)
TransformedData005 <- Transform_Matrix(Data005)
TransformedData006 <- Transform_Matrix(Data006)
TransformedData007 <- Transform_Matrix(Data007)
TransformedData008 <- Transform_Matrix(Data008)

write.csv(TransformedData001, "../Data/Network001.csv", row.names = FALSE)
write.csv(TransformedData002, "../Data/Network002.csv", row.names = FALSE)
write.csv(TransformedData003, "../Data/Network003.csv", row.names = FALSE)
write.csv(TransformedData004, "../Data/Network004.csv", row.names = FALSE)
write.csv(TransformedData005, "../Data/Network005.csv", row.names = FALSE)
write.csv(TransformedData006, "../Data/Network006.csv", row.names = FALSE)
write.csv(TransformedData007, "../Data/Network007.csv", row.names = FALSE)
write.csv(TransformedData008, "../Data/Network008.csv", row.names = FALSE)

# Absolute contact order
# ACO represents the mean of the difference in residue position along the protein, ie. average degree of separation of residues in the amino-acid chain
rm(list=ls())
graphics.off()

Data001 <- read.csv("../Data/Contact_Matrices/Contact-Matrix_ToyFolding001.dat", sep=" ", header=FALSE)
Data002 <- read.csv("../Data/Contact_Matrices/Contact-Matrix_ToyFolding002.dat", sep=" ", header=FALSE)
Data003 <- read.csv("../Data/Contact_Matrices/Contact-Matrix_ToyFolding003.dat", sep=" ", header=FALSE)
Data004 <- read.csv("../Data/Contact_Matrices/Contact-Matrix_ToyFolding004.dat", sep=" ", header=FALSE)
Data005 <- read.csv("../Data/Contact_Matrices/Contact-Matrix_ToyFolding005.dat", sep=" ", header=FALSE)
Data006 <- read.csv("../Data/Contact_Matrices/Contact-Matrix_ToyFolding006.dat", sep=" ", header=FALSE)
Data007 <- read.csv("../Data/Contact_Matrices/Contact-Matrix_ToyFolding007.dat", sep=" ", header=FALSE)
Data008 <- read.csv("../Data/Contact_Matrices/Contact-Matrix_ToyFolding008.dat", sep=" ", header=FALSE)

Transform_Matrix <- function(ContactMatrix)
{
  Monomer1 <- c()
  Monomer2 <- c()
  Contact <- c()
  for(i in 1:64) {
    for(j in 1:i) {
      Monomer1 <- c(Monomer1, i)
      Monomer2 <- c(Monomer2, j)
      Contact <- c(Contact, ContactMatrix[j,i])
    }
  }
  Network <- data.frame(Monomer1, Monomer2, Contact)
  Network <- Network[Network[,1] != Network[,2],]
  Network <- Network[Network[,1] != (Network[,2]+1),]
  Network <- Network[Network[,1] != (Network[,2]-1),]
  return(Network)
}

TransformedData001 <- Transform_Matrix(Data001)
TransformedData002 <- Transform_Matrix(Data002)
TransformedData003 <- Transform_Matrix(Data003)
TransformedData004 <- Transform_Matrix(Data004)
TransformedData005 <- Transform_Matrix(Data005)
TransformedData006 <- Transform_Matrix(Data006)
TransformedData007 <- Transform_Matrix(Data007)
TransformedData008 <- Transform_Matrix(Data008)

ACO <- function(TransformedData)
{
  N.contacts <- 0
  for(i in 1:nrow(TransformedData)){
    if(TransformedData[i,3] == 1){
    N.contacts <- N.contacts + 1
    }
  }
  Sum.Pos.Diff <- 0
  for(i in 1:nrow(TransformedData)){
    if(TransformedData[i,3] == 1){
    Sum.Pos.Diff <- Sum.Pos.Diff + (abs(TransformedData[i,1] - TransformedData[i,2]))
    }
  }
  ACOValue <- Sum.Pos.Diff/N.contacts
  return(ACOValue)
}
ACOData001 <- ACO(TransformedData001) # ACOData004 <- ACO(TransformedData004) 
ACOData002 <- ACO(TransformedData002) # ACOData005 <- ACO(TransformedData005) & ACOData007 <- ACO(TransformedData007) & ACOData008 <- ACO(TransformedData008)
ACOData003 <- ACO(TransformedData003) # ACOData006 <- ACO(TransformedData006)

# Contact Overlap
ContactOverlap <- function(Transformed1, Transformed2)
{
  N.Similar.Monomers <- 0
  for(i in 1:nrow(Transformed1)){
    N.Similar.Monomers <- N.Similar.Monomers + (Transformed1[i,3] * Transformed1[i,3])
  }
  # Sum contacts for each matrix (RMSD)
  Sum.Cont.Monomer1 <- sum(Transformed1[,3] == 1)
  Sum.Cont.Monomer2 <- sum(Transformed2[,3] == 1)
  ContactOverlapValue <- N.Similar.Monomers/(sqrt(Sum.Cont.Monomer1*Sum.Cont.Monomer2))
  return(ContactOverlapValue)
}

TransformedMatrices <- list(TransformedData001, TransformedData002, TransformedData003, TransformedData004, TransformedData005, TransformedData006, TransformedData007, TransformedData008)
Names <- c("ContactMatrix_1", "ContactMatrix_2", "ContactMatrix_3", "ContactMatrix_4", "ContactMatrix_5", "ContactMatrix_6", "ContactMatrix_7", "ContactMatrix_8")
OverlapMatrix <- matrix(NA, nrow = 8, ncol = 8)
rownames(OverlapMatrix) <- Names
colnames(OverlapMatrix) <- Names
for (i in 1:8){
  for (j in 1:8){
    OverlapMatrix[i,j] <- ContactOverlap(TransformedMatrices[[i]], TransformedMatrices[[j]])
  }
}
