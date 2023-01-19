
# Paired t-test using a built-in dataset
#data() #look at all pre-loaded R datasets
rm(list = ls())
data(sleep) #Loads the sleep dataset in your environment
sleep

# Compute t-test
res <- t.test(extra ~ group, data = sleep, paired = TRUE)
res

#How is p-value obtained?
pvalue <- 2*pt(res$statistic,9)

pvalue
#write.csv(sleep, "~/Documents/Courses/Stat453_558/sleep")

#Clean data (it keeps the loaded packages)
rm(list = ls())
