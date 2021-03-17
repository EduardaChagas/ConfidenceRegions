
regions.HC.D3 = read.csv("../Data/Regions-HC/regions-hc-D3-N1000.csv")
regions.HC.D4 = read.csv("../Data/Regions-HC/regions-hc-D4-N1000.csv")
regions.HC.D5 = read.csv("../Data/Regions-HC/regions-hc-D5-N1000.csv")
regions.HC.D6 = read.csv("../Data/Regions-HC/regions-hc-D6-N1000.csv")

regions       = data.frame(H.90 = c(regions.HC.D3$H[1], regions.HC.D3$H[2], regions.HC.D3$H[3], regions.HC.D3$H[4],
                                    regions.HC.D4$H[1], regions.HC.D4$H[2], regions.HC.D4$H[3], regions.HC.D4$H[4],
                                    regions.HC.D5$H[1], regions.HC.D5$H[2], regions.HC.D5$H[3], regions.HC.D5$H[4],
                                    regions.HC.D6$H[1], regions.HC.D6$H[2], regions.HC.D6$H[3], regions.HC.D6$H[4]),
                           C.90 = c(regions.HC.D3$C[1], regions.HC.D3$C[2], regions.HC.D3$C[3], regions.HC.D3$C[4],
                                    regions.HC.D4$C[1], regions.HC.D4$C[2], regions.HC.D4$C[3], regions.HC.D4$C[4],
                                    regions.HC.D5$C[1], regions.HC.D5$C[2], regions.HC.D5$C[3], regions.HC.D5$C[4],
                                    regions.HC.D6$C[1], regions.HC.D6$C[2], regions.HC.D6$C[3], regions.HC.D6$C[4]),
                           H.95 = c(regions.HC.D3$H[5], regions.HC.D3$H[6], regions.HC.D3$H[7], regions.HC.D3$H[8],
                                    regions.HC.D4$H[5], regions.HC.D4$H[6], regions.HC.D4$H[7], regions.HC.D4$H[8],
                                    regions.HC.D5$H[5], regions.HC.D5$H[6], regions.HC.D5$H[7], regions.HC.D5$H[8],
                                    regions.HC.D6$H[5], regions.HC.D6$H[6], regions.HC.D6$H[7], regions.HC.D6$H[8]),
                           C.95 = c(regions.HC.D3$C[5], regions.HC.D3$C[6], regions.HC.D3$C[7], regions.HC.D3$C[8],
                                    regions.HC.D4$C[5], regions.HC.D4$C[6], regions.HC.D4$C[7], regions.HC.D4$C[8],
                                    regions.HC.D5$C[5], regions.HC.D5$C[6], regions.HC.D5$C[7], regions.HC.D5$C[8],
                                    regions.HC.D6$C[5], regions.HC.D6$C[6], regions.HC.D6$C[7], regions.HC.D6$C[8]),
                           H.99 = c(regions.HC.D3$H[9], regions.HC.D3$H[10], regions.HC.D3$H[11], regions.HC.D3$H[12],
                                    regions.HC.D4$H[9], regions.HC.D4$H[10], regions.HC.D4$H[11], regions.HC.D4$H[12],
                                    regions.HC.D5$H[9], regions.HC.D5$H[10], regions.HC.D5$H[11], regions.HC.D5$H[12],
                                    regions.HC.D6$H[9], regions.HC.D6$H[10], regions.HC.D6$H[11], regions.HC.D6$H[12]),
                           C.99 = c(regions.HC.D3$C[9], regions.HC.D3$C[10], regions.HC.D3$C[11], regions.HC.D3$C[12],
                                    regions.HC.D4$C[9], regions.HC.D4$C[10], regions.HC.D4$C[11], regions.HC.D4$C[12],
                                    regions.HC.D5$C[9], regions.HC.D5$C[10], regions.HC.D5$C[11], regions.HC.D5$C[12],
                                    regions.HC.D6$C[9], regions.HC.D6$C[10], regions.HC.D6$C[11], regions.HC.D6$C[12]),
                           H.999 = c(regions.HC.D3$H[13], regions.HC.D3$H[14], regions.HC.D3$H[15], regions.HC.D3$H[16],
                                    regions.HC.D4$H[13], regions.HC.D4$H[14], regions.HC.D4$H[15], regions.HC.D4$H[16],
                                    regions.HC.D5$H[13], regions.HC.D5$H[14], regions.HC.D5$H[15], regions.HC.D5$H[16],
                                    regions.HC.D6$H[13], regions.HC.D6$H[14], regions.HC.D6$H[15], regions.HC.D6$H[16]),
                           C.999 = c(regions.HC.D3$C[13], regions.HC.D3$C[14], regions.HC.D3$C[15], regions.HC.D3$C[16],
                                    regions.HC.D4$C[13], regions.HC.D4$C[14], regions.HC.D4$C[15], regions.HC.D4$C[16],
                                    regions.HC.D5$C[13], regions.HC.D5$C[14], regions.HC.D5$C[15], regions.HC.D5$C[16],
                                    regions.HC.D6$C[13], regions.HC.D6$C[14], regions.HC.D6$C[15], regions.HC.D6$C[16]))
write.csv(regions, paste0("../Data/Regions-HC/regions-hc-N", 1000, ".csv"))

regions.HC.D3 = read.csv("../Data/Regions-HC/regions-hc-D3-N50000.csv")
regions.HC.D4 = read.csv("../Data/Regions-HC/regions-hc-D4-N50000.csv")
regions.HC.D5 = read.csv("../Data/Regions-HC/regions-hc-D5-N50000.csv")
regions.HC.D6 = read.csv("../Data/Regions-HC/regions-hc-D6-N50000.csv")

regions       = data.frame(H.90 = c(regions.HC.D3$H[1], regions.HC.D3$H[2], regions.HC.D3$H[3], regions.HC.D3$H[4],
                                    regions.HC.D4$H[1], regions.HC.D4$H[2], regions.HC.D4$H[3], regions.HC.D4$H[4],
                                    regions.HC.D5$H[1], regions.HC.D5$H[2], regions.HC.D5$H[3], regions.HC.D5$H[4],
                                    regions.HC.D6$H[1], regions.HC.D6$H[2], regions.HC.D6$H[3], regions.HC.D6$H[4]),
                           C.90 = c(regions.HC.D3$C[1], regions.HC.D3$C[2], regions.HC.D3$C[3], regions.HC.D3$C[4],
                                    regions.HC.D4$C[1], regions.HC.D4$C[2], regions.HC.D4$C[3], regions.HC.D4$C[4],
                                    regions.HC.D5$C[1], regions.HC.D5$C[2], regions.HC.D5$C[3], regions.HC.D5$C[4],
                                    regions.HC.D6$C[1], regions.HC.D6$C[2], regions.HC.D6$C[3], regions.HC.D6$C[4]),
                           H.95 = c(regions.HC.D3$H[5], regions.HC.D3$H[6], regions.HC.D3$H[7], regions.HC.D3$H[8],
                                    regions.HC.D4$H[5], regions.HC.D4$H[6], regions.HC.D4$H[7], regions.HC.D4$H[8],
                                    regions.HC.D5$H[5], regions.HC.D5$H[6], regions.HC.D5$H[7], regions.HC.D5$H[8],
                                    regions.HC.D6$H[5], regions.HC.D6$H[6], regions.HC.D6$H[7], regions.HC.D6$H[8]),
                           C.95 = c(regions.HC.D3$C[5], regions.HC.D3$C[6], regions.HC.D3$C[7], regions.HC.D3$C[8],
                                    regions.HC.D4$C[5], regions.HC.D4$C[6], regions.HC.D4$C[7], regions.HC.D4$C[8],
                                    regions.HC.D5$C[5], regions.HC.D5$C[6], regions.HC.D5$C[7], regions.HC.D5$C[8],
                                    regions.HC.D6$C[5], regions.HC.D6$C[6], regions.HC.D6$C[7], regions.HC.D6$C[8]),
                           H.99 = c(regions.HC.D3$H[9], regions.HC.D3$H[10], regions.HC.D3$H[11], regions.HC.D3$H[12],
                                    regions.HC.D4$H[9], regions.HC.D4$H[10], regions.HC.D4$H[11], regions.HC.D4$H[12],
                                    regions.HC.D5$H[9], regions.HC.D5$H[10], regions.HC.D5$H[11], regions.HC.D5$H[12],
                                    regions.HC.D6$H[9], regions.HC.D6$H[10], regions.HC.D6$H[11], regions.HC.D6$H[12]),
                           C.99 = c(regions.HC.D3$C[9], regions.HC.D3$C[10], regions.HC.D3$C[11], regions.HC.D3$C[12],
                                    regions.HC.D4$C[9], regions.HC.D4$C[10], regions.HC.D4$C[11], regions.HC.D4$C[12],
                                    regions.HC.D5$C[9], regions.HC.D5$C[10], regions.HC.D5$C[11], regions.HC.D5$C[12],
                                    regions.HC.D6$C[9], regions.HC.D6$C[10], regions.HC.D6$C[11], regions.HC.D6$C[12]),
                           H.999 = c(regions.HC.D3$H[13], regions.HC.D3$H[14], regions.HC.D3$H[15], regions.HC.D3$H[16],
                                     regions.HC.D4$H[13], regions.HC.D4$H[14], regions.HC.D4$H[15], regions.HC.D4$H[16],
                                     regions.HC.D5$H[13], regions.HC.D5$H[14], regions.HC.D5$H[15], regions.HC.D5$H[16],
                                     regions.HC.D6$H[13], regions.HC.D6$H[14], regions.HC.D6$H[15], regions.HC.D6$H[16]),
                           C.999 = c(regions.HC.D3$C[13], regions.HC.D3$C[14], regions.HC.D3$C[15], regions.HC.D3$C[16],
                                     regions.HC.D4$C[13], regions.HC.D4$C[14], regions.HC.D4$C[15], regions.HC.D4$C[16],
                                     regions.HC.D5$C[13], regions.HC.D5$C[14], regions.HC.D5$C[15], regions.HC.D5$C[16],
                                     regions.HC.D6$C[13], regions.HC.D6$C[14], regions.HC.D6$C[15], regions.HC.D6$C[16]))
write.csv(regions, paste0("../Data/Regions-HC/regions-hc-N", 50000, ".csv"))