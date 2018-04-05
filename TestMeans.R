# test for differences in means of explanatory variables
# for categories of y_pred

SP500_Predictions <- read.csv("~/Desktop/Research/StockReturnsNeuralNetworks/Paper/Version4_2018April/SP500_Predictions.CSV")

names(SP500_Predictions)
options(scipen=999)
options(digits=6)
dep_vars = c('Y_pred', 'Corr')
rhs_vars = c("RC","AVC","SDC","RT","AVT","SDT","SPY_RT")
for (dep in dep_vars) {
  for (rhs in rhs_vars) 
    {
      print(c(dep, rhs))
      fm <- aov(get(rhs) ~ factor(get(dep)) , data = SP500_Predictions)
      print (summary(fm))
      print (model.tables(fm,"means",digits=3))
      print (TukeyHSD(fm))
      print (kruskal.test(get(rhs) ~ factor(get(dep)) , data = SP500_Predictions))
    }
  }