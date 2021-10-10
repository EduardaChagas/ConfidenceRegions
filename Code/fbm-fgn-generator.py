from fbm import fbm, fgn
import numpy as np
import csv

n_series = 100
n_size = 50000

fbm_matrix = []
fgn_matrix = []

for i in range(n_series):
	fbm_sample = fbm(n = n_size, hurst = 0.1, length=1, method = 'daviesharte')
	fgn_sample = fgn(n = n_size, hurst = 0.5, length=1, method = 'daviesharte')
	fbm_matrix.append(fbm_sample) 
	fgn_matrix.append(fgn_sample) 
	print(i)

with open("../Data/fBm.csv","w+") as my_csv:
    csvWriter = csv.writer(my_csv, delimiter=',')
    csvWriter.writerows(fbm_matrix)

with open("../Data/fGn.csv","w+") as my_csv:
    csvWriter = csv.writer(my_csv, delimiter=',')
    csvWriter.writerows(fgn_matrix)
