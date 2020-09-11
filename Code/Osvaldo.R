source("Bandt-Pompe.R")

series.generator.map <- function(r, x_0, n, n_0){
  series = vector(mode = "numeric", length = n)
  series[1] = x_0
  j = 0
  for(i in 2:(n+n_0)){
    if(i > n_0 + 1){
      j = j + 1
      series[j + 1] <- r*series[j]*(1 - series[j])
    }
  }
  return(series)
}

## Logistic Map --------------------------------------------------------------------------------------------
x.0 = 0.1
n.0 = 10^5
n.series = 15
r = c(3.73, 3.70, 3.68, 3.67, 3.83, 4.00)
names.series = rep(r, n.series)

j = 1
series.map.1 = matrix(nrow = 10^5, ncol = length(r) * n.series)
for(i in 1:n.series){
  for(rr in r){
    series.map.1[,j] = series.generator.map(rr, x.0, 10^5, n.0)
    j = j + 1
    print(j)
  } 
}

j = 1
series.map.2 = matrix(nrow = 10^6, ncol = length(r) * n.series)
for(i in 1:n.series){
  for(rr in r){
    series.map.2[,j] = series.generator.map(rr, x.0, 10^6, n.0)
    j = j + 1
    print(j)
  } 
}

HC.values = data.frame(H = numeric(n.series * length(r) * 2), 
                       C = numeric(n.series * length(r) * 2),  
                       A = rep(names.series, 2),
                       N = c(rep(10^5, n.series * length(r)), rep(10^6, n.series * length(r))))

j = 1
for(i in 1:(n.series * length(r))){
  probs = bandt.pompe(series.map.1[,i], 4, 1)
  HC.values$H[j] = shannon.entropy.normalized(probs)
  HC.values$C[j] = Ccomplexity(probs)
  j = j + 1
  cat("Series: ", j, "\n")
}

for(i in 1:(n.series * length(r))){
    probs = bandt.pompe(series.map.2[,i], 4, 1)
    HC.values$H[j] = shannon.entropy.normalized(probs)
    HC.values$C[j] = Ccomplexity(probs)
    j = j + 1
    cat("Series: ", j, "\n")
}

write.csv(HC.values, "HC-Logistic-Map-2.csv")