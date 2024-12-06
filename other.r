df_csv=read.csv('salaries.csv', sep = ',')
# print(df_csv)
# colnames(df_csv)
# dim(df_csv)

m<-mean(df_csv$salary_in_usd)
print(m)

me<-median(df_csv$salary_in_usd)
print(me)

freq_sal<-table(df_csv$salary_in_usd)
freq_max<-max(freq_sal)

moda<-as.numeric(names(freq_sal[freq_sal == freq_max]))
print(moda)