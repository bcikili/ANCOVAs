library('rcompanion')
install.packages('effects')
library('effects')
install.packages('car')
library('car')
library('multcomp')

#Many folks with international relatives often find themselves calling at odd hours to fit 
#typical schedules in other time zones. How does the presence or absence of an 
#international phone plan (International.Plan) influence the use of nighttime minutes (Night.Mins), 
#holding whether or not the client has a voicemail plan (vMail.Plan) constant?


cell <- read.csv('cellPhone.csv')

plotNormalHistogram(cell$Night.Mins)
#checked dependent var.and it looks normal.

str(cell)
leveneTest(Night.Mins ~ International.Plan, data = cell)
# it does not violate homogeniety of variance test. 

RegSlop <- lm(Night.Mins ~ vMail.Plan, data = cell)
anova(RegSlop)
# this assumption met too, and we have more than enough cases to run the analysis. 

ANCOVA = lm(Night.Mins ~ vMail.Plan + International.Plan*vMail.Plan, data = cell )
anova(ANCOVA)
# International plan does not influence the night mins. And voice mail plan also has no influence on it. 