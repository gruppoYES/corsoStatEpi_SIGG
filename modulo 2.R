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
#in generale, potrei dire che la proporzione di malati tra coloro i quali hanno mangiato il cibo "colpevole"
#sia MAGGIORE rispetto a quella che ritrovo tra gli altri (= quelli che non hanno mangiato il cibo colpevole)
#la proporzione di malati tra coloro esposti o non esposti ad un alimento prende il nome di RISCHIO.
#il rapporto tra due rischi, si chiama RISK RATIO ed è una misura di associazione

#prendiamo un cibo qualsiasi
table(oswego$baked.ham,oswego$ill)
#calcoliamo la proporzione di malati tra coloro che hanno mangiato il cibo e coloro i quali non lo hanno assunto
table(oswego$baked.ham, oswego$ill) %>% prop.table(1) #prima le righe, poi le colonne. prop.table(1) calcola % di riga

#il 63% di coloro i quali hanno mangiato baked.ham si sono ammalati (rischio = .63)
#il 58.6% di coloro i quali non hanno mangiato baked.ham si sono ammalati (rischio = .586)
#il risk ratio è 
.63/.586 #= 1.075

#un'altra misura di associazione spesso utilizzata è l' ODDS RATIO
#l'odds ratio si basa sul rapporto tra ODDS. L'odds negli esposti è (N. esposti malati)/(N. esposti non-malati)
#nel nostro esempio
table(oswego$baked.ham, oswego$ill)
29/17 #1.71 odds negli esposti
17/12 #1.41 odds nei non esposti
#odds ratio =
1.71/1.41 #1.21

#L' odds è una misura surrogata del rischio. L'odds approssima il rischio quando la prevalenza della malattia è bassa

bassa.prev <- c(95,5,90,10) %>% matrix(ncol = 2, byrow = T)
bassa.prev
# rischio non esposti
5/100 #= .05
# rischio esposti
10/100 #= .10
#odds non esposti
5/95 #=.053
#odds esposti
10/90 #=.11
#RR
.10/.05 #= 2
#OR
.11/.053 #= 2.07

alta.prev <- c(50,50,30,70) %>% matrix(ncol = 2, byrow = T)
alta.prev
#rischio non esposti
50/100 #= .5
#rischio esposti
70/100 #=.7
#odds non esposti
50/50 # = 1
#odds esposti
70/30 #= 2.33
#RR
.7/.5 #=1.4
#OR
2.33/1 #2.33

#l'OR offre il vantaggio di poter essere calcolato come rapporto crociato (a*c) / (b*d)
alta.prev
(50*70)/(50*30) #2.33
#questo permette di calcolare l'OR anche negli studi caso-controllo, quando le percentuali di riga perdono di senso
#negli studi caso-controllo, infatti, è il ricercatore che stabilisce il rapporto tra malati e non malati (casi e controlli)


#creaiamo una formula per calcolare RR e OR
calcolo_RR_OR <- function(exposure, outcome, dat) { #la mia funzione vorrà tre argomenti. Exposure, outcome e dat sono "segnaposto" nella funzione
  
  #creo la mia tabella di contigenza 2x2
  my.table <- table(dat[,exposure],dat[,outcome])
  #calcolo il rischio e faccio il rapporto
  #utilizzo prop.table
  RR <- prop.table(my.table,1)[2,2] / prop.table(my.table,1)[1,2]
  #per gli odds devo calcolare "a mano"
  #nei non esposti
  odds.non.exp <- my.table[1,2] / my.table[1,1]
  #negli esposti
  odds.exp <- my.table[2,2] / my.table[2,1] 
  OR <- odds.exp/odds.non.exp
  
  #riporto i risultati
  return(data.frame(exp = exposure,
           RR = round(RR,3),
           OR = round(OR,3))) 
}
#adesso faccio girare il codice della funzione, la nuova funzione comparirà nell'environment

calcolo_RR_OR("baked.ham","ill",oswego)

#possiamo applicare la funzione a tutti i cibi
alimenti <- colnames(oswego)[8:21] #creo un vettore contenente tutti i cibi

#uso una funzione della famiglia "apply".

lapply(alimenti, function(x) {              #applica la funzione a tutti gli elementi di "alimenti"
        calcolo_RR_OR(x,"ill",oswego)       #ogni alimento, a turno, prederà il posto di "x" nella funzione
      }) %>% bind_rows() -> associazioni    #bind_rows permette di tramutare una lista (il risultato di lapply) in un dataframe, 
                                            #lo salvo in un elemento

associazioni

#"plottiamo" i risultati
ggplot(associazioni, aes(x = exp, y = RR))+ #da dove deve prendere i dati ggplot?
  geom_bar(stat = "identity")+              #cosa deve disegnare? 
                                            #stat = "identity" serve per far utilizzare in modo "diretto" i miei dati
  theme(axis.text.x = element_text(angle = 90)) #ruoto il testo in modo che sia comprensibile

#appare evidente che "vanilla.ice.cream" sia il colpevole
#"chocolate.ice.cream" ha l' RR più basso (chi mangia il gelato alla vaniglia non mangia quello al cioccolato e viceversa)

calcolo_RR_OR("chocolate.ice.cream","vanilla.ice.cream",oswego) #chi mangia il gelato al cioccolato ha più del 35% di probabilità 
                                                                #in meno di mangiare quello alla vaniglia

#il rapporto tra RR e OR è tutt'altro che lineare...
plot(associazioni$RR, associazioni$OR, xlim = c(0,25), ylim = c(0,25))
lines(x = c(0,25), y = c(0,25), col = "red")


#########################################CAMBIAMO DATASET
df <- lung

?lung
#questi dati vengono dal North Central Caner Treatment group e riportano la sopravvivenza di pazienti
#affetti da K. polmone avanzato

str(lung)

#il nostro OBIETTIVO è capire se c'è un'associazione tra l'ecog performance score
#e la morte


#ci concentriamo su egoc performance score (ph.ecog)
table(df$ph.ecog)

#per semplificare creaiamo 2 classi. Inoltre, il livello più alto contiene pochi elementi
#potremmo anche scegliere di eliminarlo... ma in questo caso lo accorpiamo alla classe precedente
df$ecog.class <- ifelse(df$ph.ecog >= 2, "2+", "0-1")

#mi assicuro che le principali variabili siano codificate in modo corretto (factor)
df$sex <- factor(df$sex, levels = c(1,2), labels=c("M","F"))
df$status <- factor(df$status, levels = c(1,2), labels = c("vivo","morto"))


table(df$status) %>% prop.table() #mortalità = 72.4% 

#proviamo una funzione di R (epitools) per il calcolo di RR
riskratio(df$ecog.class, df$status)

#oltre alla stima puntuale di RR (1.305), ci vengono forniti anche gli intervalli di confidenza al 95%
#come si interpretano? se ripetessimo questo studio un numero elevatissimo di volte, 
#il nostro RR cadrebbe nell'intervallo 1.13-1.51 il 95% delle volte
#oppure, noi siamo fiduciosi al 95% che il valore di RR nella popolazione (questo valore non potremo mai conoscerlo!)
#stia all'interno di questi valori

#e per la p?
#quando si vede un p, si pensa sempre ad un test di ipotesi
#il test sarà basato su un'ipotesi nulla ed un'ipotesi alternativa
#la p indica la probabilità di ottenere valori come quelli osservati (1.305) o più estremi
#posto che nella popolazione sia vera l'ipotesi alternativa (RR = 1).
#quindi....
#la probabilità di ottenere RR >= 1.30 nel mio campione, posto che nella popolazione RR = 1 
#è inferiore allo 0.5% (a prescindere dal test che utilizzo)

#####CONFOUNDERS
#pensiamo al "DAG" della relazione tra performance score e morte
#esistono fattori che potrebbero "confondere" l'associazione
#questi sono fattori teoricamente associati all'exposure
#teoricamente associati all'outcome
#ma che non sono sul pathway causale diretto tra l'exposure e l'outcome
#NB: i confounder si scelgono in primo luogo sulla base della relazione
#fisiologica/biologica/patologica/clinica e non sulle "p < 0.05" della tabella 1...

#età potrebbe confondere la relazione tra ecog e morte
#divido l'età in base alla mediana, per permettermi di stratificare le analisi
df$age.2cat <- ntile(df$age, 2)

#R offre la possibilità di calcolare l' OR secondo Cochran-Mantel-Haenszel
#di fatto si tratta di un OR della relazione exp-->out pesato per i k strati del confounder

#OR senza prendere in considerazione età
calcolo_RR_OR("ecog.class","status",df) #OR = 3.59
#creo una tabella 2 x 2 x k, dove k sono i livelli del mio confounder
tab.conf <- table(df$ecog.class, df$status, df$age.2cat)
tab.conf #diamo un'occhiata alla nostra tabella

#calcoliamo l' OR di Mantel-Haenszel
mantelhaen.test(tab.conf)  #"common odds ratio" = 3.53, l' OR è piuttosto vicino a quello
                           # non aggiustao (3.59): l'età non sembra essere un confounder
                           # di questa relazione.
                           # p = 0.008, vuol dire che la probabilità di trovare un ORadj >= 3.53
                           # nel nostro campione, se nella popolazione l' ORadj == 1 è 0.8%

#proviamo a calcolare OR nei singoli strati
calcolo_RR_OR("ecog.class","status",df[df$age.2cat == 1, ]) #l' OR è infinito!!! perchè?

#guardiamo la tabella 2x2 nei giovani:
table(df$ecog.class[df$age.2cat == 1], df$status[df$age.2cat == 1]) #ho uno zero...il che spiega il risultato

#in questi casi si può utilizzare la correzione di "Haldane-Anscombe".
#si tratta semplicemente di aggiungere 0.5 a tutte le celle prima di fare i calcoli.
#aggiorniamo la nostra funzione...

calcolo_RR_OR <- function(exposure, outcome, dat) { #la mia funzione vorrà tre argomenti. 
                                                    #Exposure, outcome e dat sono "segnaposto" nella funzione
  
  #creo la mia tabella di contigenza 2x2
  my.table <- table(dat[,exposure],dat[,outcome])
  ################vediamo se c'è uno zero da qualche parte...
  if (any(my.table == 0)) {
   #################se è vero aggiungiamo .5 a tutte le celle
   my.table <- my.table +.5
  }
  
  #calcolo il rischio e faccio il rapporto
  #utilizzo prop.table
  RR <- prop.table(my.table,1)[2,2] / prop.table(my.table,1)[1,2]
  #per gli odds devo calcolare "a mano"
  #nei non esposti
  odds.non.exp <- my.table[1,2] / my.table[1,1]
  #negli esposti
  odds.exp <- my.table[2,2] / my.table[2,1] 
  OR <- odds.exp/odds.non.exp
  
  #riporto i risultati
  return(data.frame(exp = exposure,
                    RR = round(RR,3),
                    OR = round(OR,3))) 
}
#facciamo girare la funzione aggiornata


calcolo_RR_OR("ecog.class","status",df[df$age.2cat == 1, ]) #l' OR è altissimo, ma non è infinito
                                                            #si vede nuovamente quanto l' OR possa dare una stima molto diversa
                                                            #rispetto al rischio (50% aumento del rischio vs  22 volte tanto in scala odds)
calcolo_RR_OR("ecog.class","status",df[df$age.2cat == 2, ]) #tra gli "anziani", l' OR è 1.69

#c'è una grossa differenza in OR tra "giovani" e "anziani". Questo potrebbe significare che l' età è un
#EFFECT MODIFIER della relazione ecog --> morte. In altre parole, l'associazione tra ecog e morte è modificata
#a seconda del livello di una terza variabile (l'età)
#ATTENZIONE: questa "effect modification" è palese usando gli OR (dove un OR è stato corretto...). 
#E' molto meno evidente sulla scala dei rischi: RRgiovani = 1.5 vs RRanziani = 1.12

#proviamo a vedere 
riskratio(df$ecog.class[df$age.2cat == 1], df$status[df$age.2cat == 1]) #1.57 (1.34-1.82)
riskratio(df$ecog.class[df$age.2cat == 2], df$status[df$age.2cat == 2]) #1.13 (0.91-1.40)

#se tra i giovani possiamo essere abbastanza fiduciosi del fatto che l'associazione ecog-->morte 
#sia presente anche nella popolazione e sia positiva
#le nostre "certezze crollano" tra gli anziani...se ripetessimo un numero altissimo di volte l'esperimento,
#potremmo anche ottenere RR = 1 (nessuna associazione) o RR < 1 (ecog è protettivo nei confronti della morte tra gli anziani...)




table(df$ecog.class, df$status) 
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




