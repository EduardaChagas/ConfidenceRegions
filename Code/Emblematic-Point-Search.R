########################################################################################################
# Author: Eduarda Chagas
# Date : Jun 19 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

# Packages and sources ---------------------------------------------------------------------------------
source("Test-point.R")

filenames <- list.files(path = "../Data/random.org/")

## Get names without ".DAT" and store in "names"
names <- substr(filenames, 1, 10)

index <- c(1:length(names))
j <- 0
dados <- vector()

##Loading data from files
for(i in names){
  j <- j + 1
  filepath <- file.path(paste("../Data/random.org/", i,".bin",sep=""))
  assign("Data", readBin(filepath, n = 1e8, size = "4", what ='integer'))
  dados <- c(dados, Data)
}

dados <- readBin('../Data/random.org/random_50k.bin',n=1e8,size="4",what ='integer')
dados <- abs(dados/max(dados))
write.csv(dados[1:50000], file = "../Data/random_sample.csv")

source("bandt_pompe/bandt_pompe.R")
source("bandt_pompe/measures.R")
source("bandt_pompe/features.R")
source("bandt_pompe/visibility.R")
source("bandt_pompe/helpers.R")

random_50k <- readBin('../Data/random.org/random_50k.bin',n=1e8,size="4",what ='integer')
seq_50k <- abs(random_50k/max(random_50k))
split_seq <- as.matrix(split(seq_50k, ceiling(seq_along(seq_50k)/50000)))
ts = matrix(nrow = 50000, ncol = 104)

for(i in 1:104){
  ts[,i] = split_seq[[i]]
}

for(i in 1:104){
  probs = bandt.pompe(ts[,i], 3, 1)
  h = shannon.entropy.normalized(probs)
  c = Ccomplexity(probs)
  cat("i: ", i, " H: ", round(h, 9), " C: ", round(c, 9), "\n")
  
  if(round(h, 6) == round(0.999985292278035, 6) && round(c, 6) == round(0.0000145144384709536, 6))
    print(i)
}

#H = 0.999985292278035
#C = 1.45144384709536e-05

write.csv(ts[,10], "../Data/Emblematic-serie-D3-N50000.csv")
