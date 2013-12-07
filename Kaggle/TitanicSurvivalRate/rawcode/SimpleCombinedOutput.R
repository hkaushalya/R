# just taking aggregate of the 3 models (ctree/RF/NB)
# predictions to produce a final result.
#This yeilds a kscore of 0.72727 . same as NB score.!


rf <- read.csv('submissions/rf_v1_kscore_0.77512.csv')
ct <- read.csv('submissions/ct_trainAgeNAtreated_kscore_0.78469.csv')
nb <- read.csv('submissions/nb_v1_kscore_0.72727.csv')

df.temp <- merge(rf, ct , by='PassengerId')
df <- merge(df.temp, nb , by='PassengerId')
names(df) <- c('PassengerId', 'RF', 'CT', 'NB') 

head(df)
df$Combined <- ifelse( (df$RF+df$CT+df$NB)>=2, 1, 0)
str(df)
df
# Write to a file

df$RF <- NULL
df$CT <- NULL
df$NB <- NULL

outfile <- 'SimpleCombOutput_v1.csv'
write.csv(final.df, file=outfile, row.names = F, quote =FALSE)
cat('\n Output written to', outfile,'\n')

# Summary of Rate of Survived predicted 
tot.survived <- sum( ifelse(survived ==1, 1, 0))
tot          <- nrow(df)
surv.pct     <- tot.survived * 100/ tot
cat ('Predicts a survival rate of = ', 
     surv.pct, ' (out of total ', tot , ' passengers)')
