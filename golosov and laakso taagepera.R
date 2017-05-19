library(SciencesPo)
#First of all, I must run database for CMP
library(manifestoR)
mp_setapikey("manifesto_apikey.txt")
mpds <- mp_maindataset()

#Then I make my database for Italy, General Elections 13.04.2008
f <- data.frame(mpds$countryname, mpds$partyname, mpds$edate, mpds$absseat, mpds$totseats, mpds$pervote)
x.sub4 <- f[f$mpds.countryname == "Italy", ]
x.sub5 <- x.sub4[x.sub4$mpds.edate == "2008-04-13", ]

a <- data.frame(x.sub5$mpds.partyname, x.sub5$mpds.absseat)
a$x.sub5.mpds.absseat/630
z <-a$x.sub5.mpds.absseat/630
n <- x.sub5$mpds.pervote
# By "n" we find effective number of electoral parties
PoliticalDiversity(n, index= "golosov")
PoliticalDiversity(n, index= "laakso/taagepera")
# By "z" we find effective number of parliamentary parties
PoliticalDiversity(z, index= "golosov")
PoliticalDiversity(z, index= "laakso/taagepera")


