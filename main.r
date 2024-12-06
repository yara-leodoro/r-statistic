df_csv=read.csv('salaries.csv', sep = ',')

var(df_csv$salary_in_usd)

dp<-sd(df_csv$salary_in_usd)
print(dp)

m<-mean(df_csv$salary_in_usd)
print(m)

coef<-dp/m
cat(coef * 100,"%\n", sep = "")

s<-summary(df_csv$salary_in_usd)
print(s)

p<-quantile(df_csv$salary_in_usd, probs = 0.01)
print(p)