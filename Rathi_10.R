# Q4
library(dplyr)

EPL201415 = read.csv("http://www.football-data.co.uk/mmz4281/1415/E0.csv")

n<-dim(EPL201415)[1]
EPL201415<-EPL201415[1:(n-1),]

EPL201415$Total_Goals <- EPL201415$FTHG + EPL201415$FTAG

hg_df <- EPL201415 %>% group_by(EPL201415$FTHG) %>% summarise(n_teams = n())
ag_df <- EPL201415 %>% group_by(EPL201415$FTAG) %>% summarise(n_teams = n())
tg_df <- EPL201415 %>% group_by(EPL201415$Total_Goals) %>% summarise(n_teams = n()) #Reference: Raj Thakkar


# Home Goals
games = sum(hg_df$n_teams)
goals = sum(hg_df$'EPL201415$FTHG'*hg_df$n_teams)
avg = goals/games
expected = games * dpois(0:20, avg)
data.frame(goals=0:20, expected=round(expected, 1))

# Grouping by 5 teams each
observed = c(hg_df$n_teams[c(1:5)],sum(hg_df$n_teams[c(6:8)]))
expected = rep(NA, length(observed))
expected[1:5] = games * dpois(0:4, avg)
expected[6] = games * (1 - ppois(4, avg))
sum(expected)

G2=2*sum(observed*log(observed/expected))
df=length(observed)-2
pvalue = 1-pchisq(G2,df=4)
message("Likelihood Ratio chi squared test p-value = ",pvalue)

X2=sum((observed-expected)^2/expected)
pval=1-pchisq(X2,df=4)
message("Pearson's chi sqaured test p-value = ",pval)

message("Thus the p-value for both the tests is greater than significance level of 0.05.
        Hence, we cannot reject the null hypothesis and can say that Poisson model is a 
        good fit for home team goals.")

# Away Goals
games = sum(ag_df$n_teams)
goals = sum(ag_df$`EPL201415$FTAG`*ag_df$n_teams)
avg = goals/games
expected = games * dpois(0:20, avg)
data.frame(goals=0:20, expected=round(expected, 1))

# Grouping by 5 teams each
observed = c(ag_df$n_teams[c(1:4)],sum(ag_df$n_teams[c(5:6)]))
expected = rep(NA, length(observed))
expected[1:4] = games * dpois(0:3, avg)
expected[5] = games * (1 - ppois(3, avg))
sum(expected)

G2=2*sum(observed*log(observed/expected))
df=length(observed)-2
pvalue=1-pchisq(G2,df=df)
message("Likelihood Ratio chi squared test p-value = ",pvalue)

X2=sum((observed - expected)^2 / expected)
pval=1-pchisq(X2,df=df)
message("Pearson's chi squared test p-value = ",pval)

message("The p-value for both the tests are greater than the significance level of 0.05 thus we cannot reject the null
hypothesis and can say that Poisson model is a good fit for away team goals.")

# Total Goals

games = sum(tg_df$n_teams)
goals = sum(tg_df$`EPL201415$Total_Goals`*tg_df$n_teams)
avg = goals/games
expected = games * dpois(0:20, avg)
data.frame(goals=0:20, expected=round(expected, 1))

# Grouping by 5 teams each
observed = c(tg_df$n_teams[c(1:7)],sum(tg_df$n_teams[c(7:9)]))
expected = rep(NA, length(observed))
expected[1:7] = games * dpois(0:6, avg)
expected[8] = games * (1 - ppois(6, avg))
sum(expected)

G2=2*sum(observed*log(observed/expected))
df=length(observed)-2
pvalue=1-pchisq(G2,df=df)
message("Likelihood Ratio chi squared test p-value = ",pvalue)

X2=sum((observed - expected)^2 / expected)
pval=1-pchisq(X2,df=df)
message("Pearson's chi squared test p-value = ",pval)

message("The p-value for both the tests are lesser than the significance level of 0.05 thus we reject the null
        hypothesis and can say that Poisson model is not a good fit for total goals.")
# Q5

observed = c(53,3110-53,110,4731-110,27,633-27)
N = sum(observed)
low_anger = (3110)/N
moderate_anger = (4731)/N
high_anger = (633)/N
heart_disease = (53+110+27)/N
no_heart_disease = (3057+4621+606)/N
expected = c(low_anger*heart_disease*N, low_anger*no_heart_disease*N,
             moderate_anger*heart_disease*N, moderate_anger*no_heart_disease*N,
             high_anger*heart_disease*N,high_anger*no_heart_disease*N)

G2 = 2 * sum(observed * log(observed/expected))

message("Therefore, the test statistic by Likelihood Ratio is ", G2)

X2 = sum((observed - expected)^2 / expected)

message("Therefore, the test statistic by Pearson's is ", X2)

df=(3-1)*(2-1)
pvalue_G = 1-pchisq(G2, df=df)
message("Likelihood Ratio chi squared test p-value = ",pvalue_G)
pvalue_X = 1-pchisq(X2, df=df)
message("Pearson's chi squared test p-value = ",pvalue_X)

message("Thus, the p-value for both chi-squared tests < significance level of 0.05, so we can reject the null
hypothesis of independence and can say that anger is associated with heart diesease.")

message("This analysis is not sufficient to comment that anger affects the chance of getting heart disease.
        Since, there are many people in the population who developed heart disease in spite of low or moderate 
        anger. To be precise, around 2% with low anger and 4% with moderate anger developed heart diseases.")
