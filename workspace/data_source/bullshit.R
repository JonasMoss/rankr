# https://osf.io/3jbzk/
# The data was originally obtained from the journal of Judgment Decision Making
# website http://www.sjdm.org/journal/16/16305/data.csv
# I also uploaded, just the key columns,  the OSF: https://osf.io/qwzyb/

bullshit = read.csv("data_source/bullshit.csv")
x = bullshit$FreeMarketIdeology
y = bullshit$BullshitReceptivity
y = y[!is.na(x)]
x = x[!is.na(x)]
bullshit = cbind(x, y)
colnames(bullshit) = c("free_market_ideology", "bullshit_receptivity")
saveRDS(as.data.frame(bullshit), file = "data/bullshit.Rds")
