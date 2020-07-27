########################################################################################################
# Author: Eduarda Chagas
# Date : Jun 18 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

# Packages and sources ---------------------------------------------------------------------------------
source("Test-point.R")
source("Bandt-Pompe.R")

filenames <- list.files(path = "../../random.org/")

## Get names without ".DAT" and store in "names"
names <- substr(filenames, 1, 10)

index <- c(1:length(names))
j <- 0
dados <- vector()

##Loading data from files
for(i in names){
  j = j + 1
  filepath = file.path(paste("../../random.org/", i,".bin",sep=""))
  assign("Data", readBin(filepath, n = 1e8, size = "4", what ='integer'))
  dados = c(dados, Data)
}

N = 500000
D = c(3,4,5,6)
tau = c(1,10,30,50)
dados = abs(dados/max(dados))
ts = matrix(dados[1:(round(52166656/N)*N)], nrow = N)
HC = data.frame(H = numeric(16*round(52166656/N)),
                C = numeric(16*round(52166656/N)),
                D = numeric(16*round(52166656/N)),
                Tau = numeric(16*round(52166656/N)))
j = 0
d = 3
for(t in tau){
  for(i in 1:round(52166656/N)){
    j = j + 1
    probs = bandt.pompe(ts[,i], d, t)
    HC$H[j] = shannon.entropy.normalized(probs)
    HC$C[j] = Ccomplexity(probs)
    HC$D[j] = d
    HC$Tau[j] = t
    cat("j: ", j, " H: ", HC$H[j], " C: ", HC$C[j], "\n")
  }
}
write.csv(HC, file = "../Data/HC_500k.csv")

d = 4
for(t in tau){
  for(i in 1:round(52166656/N)){
    j = j + 1
    probs = bandt.pompe(ts[,i], d, t)
    HC$H[j] = shannon.entropy.normalized(probs)
    HC$C[j] = Ccomplexity(probs)
    HC$D[j] = d
    HC$Tau[j] = t
    cat("j: ", j, " H: ", HC$H[j], " C: ", HC$C[j], "\n")
  }
}
write.csv(HC, file = "../Data/HC_500k.csv")

d = 5
for(t in tau){
  for(i in 1:round(52166656/N)){
    j = j + 1
    probs = bandt.pompe(ts[,i], d, t)
    HC$H[j] = shannon.entropy.normalized(probs)
    HC$C[j] = Ccomplexity(probs)
    HC$D[j] = d
    HC$Tau[j] = t
    cat("j: ", j, " H: ", HC$H[j], " C: ", HC$C[j], "\n")
  }
}
write.csv(HC, file = "../Data/HC_500k.csv")

d = 6
for(t in tau){
  for(i in 1:round(52166656/N)){
    j = j + 1
    probs = bandt.pompe(ts[,i], d, t)
    HC$H[j] = shannon.entropy.normalized(probs)
    HC$C[j] = Ccomplexity(probs)
    HC$D[j] = d
    HC$Tau[j] = t
    cat("j: ", j, " H: ", HC$H[j], " C: ", HC$C[j], "\n")
  }
}
write.csv(HC, file = "../Data/HC_500k.csv")
 
 
