#Name:Daniyal Arshad



download.file("http://www.openintro.org/stat/data/atheism.RData", destfile = "atheism.RData")
load("atheism.RData")

#1
#Data is from the entire population 


#2
atheist1 = subset(atheism, nationality =="United States")
atheism.us = table(atheist1$year, atheist1$response)
#atheist non-atheist
#2005      10         992
#2012      50         952


#3
prop.table(atheism.us, margin = 1)["2012", "atheist"]
#0.0499002
#There were only 50 people from the United States who claimed to be atheists
#952 claimed to not be atheist


#4
responses = prop.table(colSums(atheism.us))
loop = rowSums(atheism.us)
loop_out = outer(loop, responses)

#5
new_athiests = table(atheism)
prop.test(as.table(atheism.us))

#2-sample test for equality of proportions with continuity
#correction

#data:  as.table(atheism.us)
#X-squared = 26.132, df = 1, p-value = 3.188e-07
#alternative hypothesis: two.sided
#95 percent confidence interval:
#  -0.05573843 -0.02410189
#sample estimates:
#  prop 1     prop 2 
#0.00998004 0.04990020 


#6
atheism_2012 = prop.table(atheism.us, margin = 1)["2012", "atheist"]
atheism_2005 = prop.table(atheism.us, margin = 1)["2005", "atheist"]
#Atheists in 2012 is higher. The sample of 2005 is smaller than 2012.
#0.0499002 = 2012
#0.00998004 = 2005

#7
n <- 1000
p <- seq(0, 1, 0.01)
sderr = sqrt(p *(1-p)/(n))


#8
plot(sderr)


#9
athiests.in.spain = subset(atheism, nationality == "Spain")
athiests.spain = table(atheism$year, atheism$response)
athiests.spain
prop.test(as.table(athiests.spain))

#      atheist  non-atheist
# 2005    2022       34083
# 2012    3476       48451
# There has been a growth in the number of athiests and non atheists over the years.


#2-sample test for equality of proportions with continuity
#correction

#data:  as.table(athiests.spain)
#X-squared = 43.319, df = 1, p-value = 4.651e-11
#alternative hypothesis: two.sided
#95 percent confidence interval:
#  -0.01416114 -0.00771247
#sample estimates:
#  prop 1     prop 2 
#0.05600332 0.06694013 


#10
year_2012 = subset(atheism, year == '2012')
year_2012_table = table(year_2012$nationality, year_2012$response)
colombia_brazil = year_2012_table[c("Colombia", "Brazil"),]
prop.test(as.table((colombia_brazil)))

#           atheist non-atheist
#Colombia      18         588
#Brazil        20        1982


#2-sample test for equality of proportions with continuity correction

#data:  as.table((colombia_brazil))
#X-squared = 11.255, df = 1, p-value = 0.0007942
#alternative hypothesis: two.sided
#95 percent confidence interval:
#  0.004436963 0.034988957
#sample estimates:
#  prop 1     prop 2 
# 0.02970297 0.00999001 


