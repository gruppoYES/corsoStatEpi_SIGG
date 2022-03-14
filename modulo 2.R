rm(list = ls()) #puliamo il nostro environment

#carichiamo i pacchetti necessari
#se questi non fossero presenti, possiamo installarli, ad esempio:
install.packages("tidyverse") #il nome del pacchetto, con install.packages, va tra virgolette
library(tidyverse)
library(epiR)
library(epitools)
library(pROC)

#####################MISURE DI ASSOCIAZIONE##########################
#carichiamo il dataset OSWEGO
data("oswego")
?oswego #capiamo di più su questi dati

#1) una malattia gastrointestinale acuta ha colpito diverse persone nella contea di Oswego
#2) tutte le persone malate hanno mangiato ad una cena
#3) il nostro obiettivo è capire QUALE CIBO HA CAUSATO LA MALATTIA?

#guardiamo la struttura dei nostri dati
str(oswego)

#iniziamo a capire la proporzione di malati
table(oswego$ill) %>% prop.table #61.3%

#che cosa mi aspetto dal cibo che è causa di malattia?
table(oswego$baked.ham,oswego$ill) %>% prop.table(.,1) 

# qual ? il rischio di essere malati per chi ha mangiato il prosciutto?
# qual ? il rischio di essere malati per chi non ha mangiato il prosciutto?
# calcoliamo RR...
# e gli odds/odds ratios...?


#ATTIVITA 1: scegliete un alimento e calcolate RR e OR
#
#
#
#
#
#
alimenti <- colnames(oswego)[8:21]
rr.alimenti <- c()
or.alimenti <- c()
for (alimento.x in alimenti) {
  rr.x <- readline(prompt = paste0("RR per ", alimento.x, " "))
  or.x <- readline(prompt = paste0("OR per ", alimento.x, " "))
  rr.alimenti <- c(rr.alimenti, rr.x)
  or.alimenti <- c(or.alimenti, or.x)
}
data.frame(alimento = alimenti,
           RR = rr.alimenti,
           OR = or.alimenti)

#qual ? l'alimento che pi? probabilmente ha dato malattia?



#####possibili soluzioni pi? rapide

oswego %>% 
  select(id,ill,baked.ham:fruit.salad) %>% 
  gather(key=alimento, value=assunto, -id, -ill) %>% 
  group_by(alimento,assunto) %>% 
  count(ill) %>% 
  mutate(prop=n/sum(n)) %>% 
  filter(ill=="Y") %>% 
  group_by(alimento) %>% 
  mutate(RR=prop/first(prop)) %>%
  filter(assunto=="Y")


apply(oswego[,8:21],2, function(cibo){
  risk.tab = table(cibo,oswego$ill) %>% prop.table(.,1)
  return(round(risk.tab[2,2] / risk.tab[1,2],2))
})


####CAMBIAMO DATASET
df <- lung
?lung
str(lung)

#ci concentriamo su egoc performance score (ph.ecog)
table(df$ph.ecog)
#dividiamo in due classi... 3 pu? essere accorpata a 2 
df$ecog.class <- ifelse(df$ph.ecog >= 2, "2+", "0-1")
table(df$ph.ecog, df$ecog.class) #"misura due volte, taglia una volta"
df$sex <- factor(df$sex, levels = c(1,2), labels=c("M","F"))
df$status <- factor(df$status, levels = c(1,2), labels = c("vivo","morto"))


table(df$ecog.class,df$status)
# utilizziamo RR o OR per indagare questa associazione? 
# qual ? il rischio se ecog >= 2
# calcola OR e RR per ecog >= 2 vs ecog < 2
# la ragione della differenza tra rischio e OR?

riskratio(df$ecog.class, df$status)
#quale interpretazione date dei 95%CI?
#quale interpretazione date delle p?
oddsratio(df$ecog.class,df$status)
oddsratio.wald(df$ecog.class,df$status)

#COSA CONCLUDIAMO, alla fine?
#
#
#
#
#
#pensa al DAG della relazione ecog --> morte: quale(i) potenziali fattori confondenti ti vengono in mente?
#
#
#
#
#
##AGE

df$age.2cat <- ntile(df$age, 2)



table(df$ecog.class, df$status) #OR senza et? 3.6
table(df$ecog.class, df$status, df$age.2cat)
#OR per i giovani? 
#
#
#
#-Haldane-Anscombe correction
#OR per i vecchi? 
mantelhaen.test(table(df$ecog.class, df$status, df$age.2cat)) #age.adjOR

df$age.5cat <- ntile(df$age,5)
mantelhaen.test(table(df$ecog.class, df$status, df$age.5cat)) #age.adjOR

#estrema cautela!!!

mantelhaen.RR(table(df$ecog.class,df$status,df$age.2cat))
#### oppure
?epi.2by2 #ATTENZIONE ALL' ORDINE DELLA TABELLA RICHIESTA
cont.table <- table(df$ecog.class,df$status,df$age.2cat)
cont.table <- cont.table[c(2,1),c(2,1),c(1,2)]
epi.2by2(cont.table, method = "cohort.count")




#################ACT.2 = provate a fare lo stesso ragionamento usando M e F come strati
#
#
#
#
#
#
table(df$ecog.class, df$status, df$sex)
#OR maschi = 8.8
#OR femmine = 2.8
mantelhaen.test(table(df$ecog.class, df$status, df$sex)) #4.137
mantelhaen.RR(table(df$ecog.class, df$status, df$sex))
#effect modification?

cont.table <- table(df$ecog.class,df$status,df$sex)
cont.table <- cont.table[c(2,1),c(2,1),c(1,2)]
epi.2by2(cont.table, method = "cohort.count")


############incidence rate
#concentriamoci sul tempo
#perch? scegliere un rate ratio invece di un risk ratio?
df$ID <- seq_len(nrow(df))


ggplot(df) +
  geom_segment(aes(y = ID, x = 0, xend = time, yend = ID))+
  geom_point(aes(x = time, y = ID, shape = status, colour = status), alpha = .7)+
  xlab("time, days")+
  ylab("participant ID")+
  theme_bw()



df %>% 
  filter(!is.na(ecog.class)) %>%
  group_by(ecog.class) %>% 
  summarise(event=sum(status=="morto"),
            futime=sum(time)) %>% 
  column_to_rownames("ecog.class") %>% 
  as.matrix -> rate.tab

rate.tab #calcoliamo IR per ecog basso e alto
45/11822
(45/11822)*365.25 #1.39

(119/57700)*365.25 #.75


rateratio(rate.tab)
rate.tab.inv <- rate.tab[c(2,1),c(1,2)]
epi.2by2(rate.tab.inv, method = "cohort.time")




##################sensitivity & specificity
#cambio di prospettiva
#definizione
#qual ? la sensitivy di ecog 2+?
#qual ? la specificity di ecog 2+?
table(df$ecog.class, df$status)

#se calcolo spec e sens per tutti i cut-off possibili?
# 0+?
# 1+?
# 2+?
# 3+?


sens.spec <- c()
for (i in 0:3) {
  test.cutoff <- ifelse(df$ph.ecog >= i, 1,0)
  sens.spec <- c(sens.spec,
                 i, sens_spec_calc(test.cutoff,df$status))
}
results <- sens.spec %>% matrix(.,ncol=3, byrow = T)
results <- cbind(results, 1-results[,3])
plot(results[,4], results[,2], type = "b", xlab="1-spec", ylab="sens")
lines(x = c(0,1), y = c(0,1), col = "red")
text(results[,4], results[,2]+.1, labels = results[,1])


roc(status ~ ph.ecog, df) %>% plot
roc(status ~ ph.ecog, df) %>% coords

roc.ph <- roc(status ~ ph.karno, df)
roc.pat <- roc(status ~ pat.karno, df)
roc.test(roc.ph, roc.pat)
#PPV e NPV?


##########Linear Reg
#sono interessato alla relazione meal cal --> wt.loss
plot(df$meal.cal, df$wt.loss)
boxplot(df$meal.cal)
df.clean <- df[df$meal.cal < 2000,]

plot(df.clean$meal.cal, df.clean$wt.loss)
mod.base <- lm(wt.loss ~ meal.cal, df.clean)
df.clean$pred.modbase <- predict(mod.base) #errore (missing)
mod.base <- lm(wt.loss ~ meal.cal, df.clean, na.action = na.exclude)
df.clean$pred.modbase <- predict(mod.base)
df.clean$ID <- seq_len(nrow(df.clean))

ggplot(df.clean, aes(x = meal.cal, y = wt.loss))+
  geom_point()+
  geom_smooth(method = "lm", se = F)+
  geom_segment(aes(x = meal.cal, y = pred.modbase, yend = wt.loss, xend = meal.cal, group = ID), colour = "red")+
  geom_hline(yintercept = mean(df.clean$wt.loss, na.rm = T), linetype = "dashed")+
  geom_segment(aes(x = meal.cal, y= mean(df.clean$wt.loss, na.rm  = T), yend = pred.modbase, xend = meal.cal, group = ID), colour= "blue")

ggplot(df.clean, aes(x = pred.modbase, y = wt.loss)) +
  geom_point()+
  geom_abline(intercept =  0, slope = 1, colour = "red")+
  xlim(-25,50)

SS.wtloss <- (df.clean$wt.loss - mean(df.clean$wt.loss, na.rm = T))**2 %>% sum(na.rm = T)
SS.mod <- (df.clean$pred.modbase - mean(df.clean$pred.modbase, na.rm = T))**2 %>% sum(na.rm = T)
SS.res <- SS.wtloss - SS.mod
R2 = SS.mod / SS.wtloss

summary(mod.base)
#e se aggiungessi anche l'et? al modello?
#
#
#
#
#
mod.complex <- lm(wt.loss ~ meal.cal + sex + age, df.clean)
summary(mod.complex)
anova(mod.complex)
anova(mod.sex, mod.complex)


par(mfrow=c(2,2)) 
plot(mod.base)
par(mfrow=c(1,1)) 


par(mfrow=c(2,2)) 
plot(lm(wt.loss ~ meal.cal, df))
par(mfrow=c(1,1))
#linearit?
#normalit?
#omoschedasticit?




