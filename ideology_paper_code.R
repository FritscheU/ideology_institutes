# Authors: Sami Diaf, Ulrich Fritsche, Jörg Döpke, Ida Rockenbach
# replication code for paper "Sharks and minnows in a shoal of words..."
# V_2 for Submission IJPE (last edit: November 17 2021)
# ---------------------------------------------------------------------------

# tested with R version 4.1.2 Patched (2021-11-01 r81115) -- "Bird Hippie"

# install libraries if necessary
# install.packages("devtools")
# devtools::install_github("kbenoit/wordshoal") 

# Remove objects from memory

rm(list=ls())

# activate libraries

library(tm)
library(topicmodels)
library(quanteda)
library(tidyverse)
library(RColorBrewer)
library(wordcloud)
library(xtable)
library(robustbase)
library(stargazer)
library(fastDummies)
library(slam)
library(stm)
library(igraph)
library(ggplot2)
library(ggrepel)
library(SnowballC)
library(wordshoal)
library(tm)
library(xtable)
library(lubridate)
library(tidytext)
library(quanteda.textmodels)
library(udpipe)
library(corpustools)


library(xlsx)
library(dplyr)
library(tidyr)
library(reshape)
library(reshape2)

library(broom)
library(glmnet)
library(openxlsx)
library(readxl)
library(MASS)
library(EnvStats)
library(VGAM)
library(quantreg)
library(data.table)

library(Cairo)
library(ggpubr)


# set option for text analysis

options(stringsAsFactors = F)

load(paste(wd,".../mon_fisc_corpus_new.Rdata",sep=""))

# own stopWords vector 

stopWords <- c("januar","februar","märz","april","mai","juni","juli","august","september","oktober","november","dezember",
               "jahr","jahre","jahren","jahres","deutschland","quartal","quelle","quellen",
               "imk","gd","diw","ifo","rwi","hwwi","hwwa", "ifw","sachverständigenrat","fb01","fb02","wirtschaftsdienst","bundesbank","monatsbericht","iwh","svr","mrd","bundesamt",
               ",", "i","ii","iii","iv", "v", "vh", "jg", "abb", "dm", "abbildung", "prozent", "u", 
               "wochenbericht" ,"schnelldienst", "inprozent", "berlin","institut","instituts","institute", 
               "skala", "tabelle", "milliarde", "milliarden", "mill", "jahrgang", "schaubild", "sonderausgabe",
               "•","-","-",">","<","„","€","euro","mark","”","â","Ã","¢","â","€","“","Â",
               "kapitel","frühjahr","aktuell","deutsche","deutsch","deutsches","deutscher",
               "jahresgutachten","herbst","sonderausgabe","quartal","vorjahr",
               "alfred","boss","deutschen","wirtschaft","köln",
               "clie", "clic", "cler", "clcr","clas", "uncl", "flir", "fiir", "vmjahr", "diesemjahr", "vergangenenjahr",
               "fachserie", "benner", "boysenhogrefe", "grau", "hinterlegt", "kasten", "ndern", "krisenl", "nnen", "liquidit","nnte", "nnten", "europ","–","—", "",
               "—","¨","“","”","„","¦","£","¤","€","<U+2158>","<U+FFFD>","«","»","©","°","·","…","•","¼","½","¾","¹",
               "a<U+0457>tsahalia","aa","aaa", "aao", "on", "of", "the", "and", "vol", "z.b", "erh", "ber", "ische", "rfte", "____________________", "roye", "van", "fiedler", "gen", "zent", "lität",
               "tens", "chen", "che", "än", "be", "ti", "sic", "dei", "cn", "vl", "kuro", "luro", "kzb", "no", "ois", "al", "__blob", "publicationfile",
               "ren", "sen", "ver", "te", "an", "pro", "ge", "ent", "ken", "da", "kri", "tltro", "glrg", "cppb3", "ba",
               "1999a", "zit", "ni", "cht", "staa", "wa", "ie", "l1", "erung", "gesetzli", "stefan","bach", "deuverden", "no", "re", "hren", "ck", "hrung", "hrt", "rde", "nd",
               "glichkeiten", "fr", "u.a", "____________________", "ßen", "lich", "z.b", "det", "se", "kr", "erl", "lur", "1h", "yz", "ber", "st", "hren", "ck", "ffentlichen", "sse", "ffentliche", "rker", "ftig", "begr", "be", "we", "mals", "tigt", "hen",
               "en", "1e", "ei", "em", "ch", "ahr", "eren", "tes", "ta", "na", "icht", "l3", "ckf", "einsch", "tzung", "beitr", "ftigung", "nder", "betr",
               "rzungen", "angek", "tz", "tze", "ge", "de", "bud", "wltp","krholung","abs","kon","stabi","tab","ca","a.a.o","sieh","ver-","an-","kri","lung","gaben")

### Prepare both main corpora
## Monetary policy
# Preparing the monetary policy corpus

mon_corpus <- corpus(df_mon)

mon_tokens <- tokens(mon_corpus,
                     remove_numbers = TRUE,
                     remove_punct = TRUE,
                     remove_symbols = TRUE,
                     remove_separators = TRUE,
                     remove_url = TRUE,
                     split_hyphens=TRUE) %>% 
  tokens_tolower() %>%
  tokens_remove(c(stopwords("german"),stopWords)) %>%
  tokens_select(min_nchar = 2) 

vdfm <- dfm(mon_tokens) %>%
  dfm_trim(min_termfreq = 3, min_docfreq = 1)

## Fiscal policy
# preparing the fiscal policy corpus

fisc_corpus <- corpus(df_fisc)

fisc_tokens <- tokens(fisc_corpus,
                      remove_numbers = TRUE,
                      remove_punct = TRUE,
                      remove_symbols = TRUE,
                      remove_separators = TRUE,
                      remove_url = TRUE,
                      split_hyphens=TRUE) %>%  
  tokens_tolower() %>%
  tokens_remove(c(stopwords("german"),stopWords)) %>%
  tokens_select(min_nchar = 2) 

vdfm2 <- dfm(fisc_tokens) %>%
  dfm_trim(min_termfreq = 3, min_docfreq = 1)

## Producing Wordfish results as a benchmark
## Monetary policy
# Estimate wordfish

wordfish_mon <- textmodel_wordfish(vdfm, dir = c(1,151))

# reproducing the quanteda plot with ggplot2 for the paper section on Wordfish
# "wordfish_m" is used in ggplot2 below for date identification

wordfish_m <- data.frame(theta=wordfish_mon$theta,
                          year=wordfish_mon$x@docvars$year,se=wordfish_mon$se.theta,
                          month=sprintf("%02d",df_mon$month)) %>%
   mutate(dat=ymd(paste0(year,month,"01")))


# plotting wordfish scores per year (quanteda style) + edits
# labelling data

doclabels <- docnames(wordfish_mon$x)

# collect data for figure

results <- data.frame(doclabels = doclabels, theta = wordfish_mon$theta,
                       lower = wordfish_mon$theta - 1.96 * wordfish_mon$se.theta, upper = wordfish_mon$theta +
                         1.96 * wordfish_mon$se.theta)

# create figure for paper 

ggplot2::ggplot(data = results, aes(x = doclabels, y = theta,color=(wordfish_m$dat>"2007-12-31"))) +
   geom_point(aes(x = reorder(doclabels, theta), y = theta)) +
   geom_pointrange(aes(ymin = lower, ymax = upper)) +
   facet_grid(wordfish_m$year~.,
              scales = "free_y", space="free") +
   coord_flip() +
   ylab("") +
   xlab("") +
   theme_minimal() +
   # theme(text = element_text(size=20))+
   theme(legend.position = "none",axis.ticks.y = element_blank(), axis.text.y=element_blank(),
         axis.title.y = element_blank(), strip.text.y.right = element_text(angle = 0)) +
   geom_hline(data = results, aes(yintercept = 0,linetype="longdash"))

## Fiscal policy
# Estimate wordfish

wordfish_fisc <- textmodel_wordfish(vdfm2, dir = c(1,163))

# reproducing the quanteda plot with ggplot2
# wordfish_f is later used for ggplot2 command (date identification)

wordfish_f <- data.frame(theta=wordfish_fisc$theta,
                         year=wordfish_fisc$x@docvars$year,se=wordfish_fisc$se.theta,
                         month=sprintf("%02d",df_fisc$month)) %>%
  mutate(dat=ymd(paste0(year,month,"01")))

# plotting wordfish scores per year (quanteda style) + edits

doclabels_f <- docnames(wordfish_fisc$x)

# collect data for figure

results_f <- data.frame(doclabels = doclabels_f, theta = wordfish_fisc$theta,
                        lower = wordfish_fisc$theta - 1.96 * wordfish_fisc$se.theta, upper = wordfish_fisc$theta +
                          1.96 * wordfish_fisc$se.theta)

# plot figure

ggplot2::ggplot(data = results_f, aes(x = doclabels, y = theta,color=(wordfish_f$dat>"2007-12-31"))) +
  geom_point(aes(x = reorder(doclabels, theta), y = theta)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  facet_grid(wordfish_f$year~.,
             scales = "free_y", space="free") +
  coord_flip() +
  ylab("") +
  xlab("") +
  theme_minimal() +
  theme(legend.position = "none",axis.ticks.y = element_blank(), axis.text.y=element_blank(),
        axis.title.y = element_blank(), strip.text.y.right = element_text(angle = 0)) +
  geom_hline(data = results_f, aes(yintercept = 0,linetype="longdash"))

## Descriptive analysis (using library corpustools)
# Create corpustools' tCorpus objects
# Monetary policy corpus

# loading german language model, downloaded with udpipe
udmodel_ger <- udpipe::udpipe_load_model(paste(wd,"Ideology_text/code/german-gsd-ud-2.4-190531.udpipe", sep = ""))

# preprocessing on dataframe level (prepared as input for corpustools' create_tcorpus command)
df_mon$text <- df_mon$text %>%
  tolower() %>%
  tm::removeNumbers() %>%
  tm::removePunctuation() %>%
  tm::removeWords(c(stopWords,stopwords("german"))) %>%
  tm::stripWhitespace()

# keep just the words appearing in the edited monetary dfm (vdfm@Dimnames$features)
for(i in seq(1,nrow(df_mon))){
  df_mon$text[i] <- paste(intersect(strsplit(df_mon$text[i], "\\s")[[1]], c(vdfm@Dimnames$features)), collapse=" ")
}

tc_mon <- create_tcorpus(df_mon,
                         text_columns = "text",
                         doc_column = "id",
                         meta="institution",
                         meta= "year",
                         remove_stopwords = TRUE,
                         language="german",
                         udpipe_model_path =  udmodel_ger$model,
                         use_parser = TRUE)

tc_mon$preprocess('token', 'feature', remove_stopwords = TRUE, use_stemming = TRUE)

# create sub-corpora...

tc_mon_pre2008 <- create_tcorpus(df_mon %>% dplyr::filter(year<2008),
                                 text_columns = "text",
                                 doc_column = "id",
                                 meta="institution",
                                 meta="year",
                                 remove_stopwords = TRUE,
                                 language="german",
                                 udpipe_model_path =  udmodel_ger$model,
                                 use_parser = TRUE)

tc_mon_pre2008$preprocess('token', 'feature', remove_stopwords = TRUE, use_stemming = TRUE)

tc_mon_post2008 <- create_tcorpus(df_mon %>% dplyr::filter(year>=2008),
                                  text_columns = "text",
                                  doc_column = "id",
                                  meta="institution",
                                  remove_stopwords = TRUE,
                                  language="german",
                                  udpipe_model_path =  udmodel_ger$model,
                                  use_parser = TRUE)

tc_mon_post2008$preprocess('token', 'feature', remove_stopwords = TRUE, use_stemming = TRUE)

# same procedure for fiscal policy

# preprocessing on dataframe level (prepared as input for corpustools' create_tcorpus command)
df_fisc$text <- df_fisc$text %>%
  tolower() %>%
  tm::removeNumbers() %>%
  tm::removePunctuation() %>%
  tm::removeWords(c(stopWords,stopwords("german"))) %>%
  tm::stripWhitespace()

# keep just the words appearing in the edited fiscal dfm (vdfm2@Dimnames$features)
for(i in seq(1,nrow(df_fisc))){
  df_fisc$text[i] <- paste(intersect(strsplit(df_fisc$text[i], "\\s")[[1]], c(vdfm2@Dimnames$features)), collapse=" ")
}

tc_fisc <- create_tcorpus(df_fisc,
                          text_columns = "text",
                          doc_column = "id",
                          meta="institution",
                          meta= "year",
                          remove_stopwords = TRUE,
                          language="german",
                          udpipe_model_path =  udmodel_ger$model,
                          use_parser = TRUE)

tc_fisc$preprocess('token', 'feature', remove_stopwords = TRUE, use_stemming = TRUE)

tc_fisc_pre2008 <- create_tcorpus(df_fisc %>% dplyr::filter(year<2008),
                                  text_columns = "text",
                                  doc_column = "id",
                                  meta="institution",
                                  meta="year",
                                  remove_stopwords = TRUE,
                                  language="german",
                                  udpipe_model_path =  udmodel_ger$model,
                                  use_parser = TRUE)

tc_fisc_pre2008$preprocess('token', 'feature', remove_stopwords = TRUE, use_stemming = TRUE)

tc_fisc_post2008 <- create_tcorpus(df_fisc %>% dplyr::filter(year>=2008),
                                   text_columns = "text",
                                   doc_column = "id",
                                   meta="institution",
                                   remove_stopwords = TRUE,
                                   language="german",
                                   udpipe_model_path =  udmodel_ger$model,
                                   use_parser = TRUE)

tc_fisc_post2008$preprocess('token', 'feature', remove_stopwords = TRUE, use_stemming = TRUE)

# produce corpora comparison results and graphs

comp_prepost2008 <- compare_corpus(tc_y=tc_mon_pre2008,tc_mon_post2008,'token')
comp_prepost2008 <- comp_prepost2008[order(-comp_prepost2008$chi),]

plot(comp_prepost2008)

comp_prepost2008 <- compare_corpus(tc_y=tc_fisc_pre2008,tc_fisc_post2008,'token')
comp_prepost2008 <- comp_prepost2008[order(-comp_prepost2008$chi),]

plot(comp_prepost2008)


## Wordshoal section
# load a modified Wordshoal code (for calculation of word loadings as in Lauderdale/ Herzog, p.5)
# tuning Bayesian aggregation priors

source(paste(wd,".../wordshoal_modif.R", sep = ""))

# Monetary policy
# Estimating Wordshoal parameters

wordshoalfit <- textmodel_wordshoal_modif(vdfm, dir = c(1,151),
                                          groups = docvars(vdfm, "year"), 
                                          authors = docvars(vdfm, "institution"),
                                          prioralpha = 0.5,
                                          priorbeta = 0.5,
                                          priortheta = 1,
                                          priortau = 1)
# collect information

author_positions <- summary(wordshoalfit)$estimated.author.positions
author_positions$row_names <- rownames(author_positions)
fitdf <- merge(author_positions,
               docvars(vdfm), 
               by.x = "row_names", by.y = "institution")
fitdf <- subset(fitdf, !duplicated(id))

# graph thetas with s.e.

summary(wordshoalfit)$estimated.author.positions %>%
  ggplot(aes(x=reorder(rownames(.),-theta),y=theta,col="darkred")) +
  geom_point(size=2) +
  scale_x_discrete(labels = c("DIW" = "Berlin institute (DIW)", "RWI" =  "Essen institute (RWI)", "IFO" = "Munich institute (ifo)", "IWH" = "Halle institute (IWH)",  "GED" = "Joint diagnosis",  "IFW" = "Kiel institute (IfW)" ) ) +
  geom_errorbar(aes(ymin=theta-1.96*se, ymax=theta+1.96*se), width=.2,
                position=position_dodge(.9),size=1) +
  coord_flip() +
  theme(axis.text=element_text(size=16)) +
  theme_minimal() +
  theme(legend.title = element_blank(),legend.position = "none") +
  theme(text = element_text(size=28))+
  xlab("") + ylab("")

# creating a dataframe to export the betas for LaTeX table

mon_beta <- tibble(year=seq(1999,2020,1),beta=round(wordshoalfit$beta,2)) %>% arrange(-abs(beta))
mon_alpha <- tibble(year=seq(1999,2020,1),alpha=round(wordshoalfit$alpha,2)) %>% arrange(-abs(alpha))

# transfering results to LaTeX

print(mon_beta, row.names=FALSE) %>% xtable(digits=c(0,0,2))

# plot figure 

mon_beta %>% ggplot(aes(x=year,y=abs(beta),col="darkred")) + 
  geom_line(size = 1) +
  theme_minimal() +
  theme(legend.title = element_blank(),legend.position = "none")+
  theme(text = element_text(size=28)) +
  scale_x_continuous(breaks=c(2000, 2005, 2010, 2015, 2020)) +
  #theme_light()+
  ylab("") + xlab("") + 
  ylim(0,0.6)

# plot figure

mon_alpha %>% ggplot(aes(x=year,y=(alpha),col="darkred")) + 
  geom_line(size = 1) +
  theme_minimal() +
  theme(legend.title = element_blank(),legend.position = "none")+
  theme(text = element_text(size=28)) +
  scale_x_continuous(breaks=c(2000, 2005, 2010, 2015, 2020)) +
  #theme(text = element_text(size=24))+
  #theme_light()+
  ylab("") + xlab("") + 
  ylim(-0.2,0.2)

# Compute debate loading (Lauderdale & Herzog, 2016, p. 5)

deb_load_mon <- data.frame(num=df_mon %>%
                           #filter(institution=="IFW") %>% 
                           group_by(year) %>% 
                           count(year), 
                           b=wordshoalfit[["beta"]])

# print debate loading to console

deb_load_mon %>% summarise(q=sqrt(sum(num.n*b*b)/sum(num.n)))

deb_load_mon %>% filter(num.year>=2008) %>%
  summarise(q=sqrt(sum(num.n*b*b)/sum(num.n)))

deb_load_mon %>% filter(num.year<2008) %>%
  summarise(q=sqrt(sum(num.n*b*b)/sum(num.n)))

# Compute word loading (Lauderdale & Herzog, 2016)
# requires wordfish coefficients (but estimated for each debate separately)
# wordshoal_modif is therefore used
# construct the kappa_ij for word loading

kappa <- wordshoalfit[["kappa"]] %>% lapply(as.data.frame) # extract the kappa values from wordshoal estimation results
kappa_ij <- kappa %>% reduce(full_join, by = "features") # dplyr::join all rows and columns
names(kappa_ij) <- c("word",seq(1999,2020,1)) # add names
k_m <- kappa_ij %>% gather(value=kappa,key=year,-word) %>% mutate(year=as.numeric(year))


# top 10 words contributing to positive positions for each year
k_m %>% 
  group_by(year) %>% 
  top_n(10) %>% 
  View()

k_m %>% 
  #filter(year %in% c(2014,2017,2019)) %>%
  group_by(year) %>% 
  top_n(8) %>%
  ggplot(aes(x=year,y=kappa)) +
  geom_point(color = "black") +
  # geom_text(aes(label=word),hjust=1, vjust=0, size = 3, color = "blue") +
  geom_text_repel(aes(label=word),hjust=2, vjust=0, size = 2, color = "black", max.overlaps = 30) +
  labs(y="Kappa", x="Year") +
  theme_light()
  #+
  #ggtitle("Estimated most important positive Kappa parameters for debate vocabulary", subtitle = "Monetary policy corpus")


# top 10 words contributing to negative positions for each year
k_m %>% 
  group_by(year) %>% 
  top_n(-10) %>% 
  View()

k_m %>% 
  #filter(year %in% c(2014,2017,2019)) %>%
  group_by(year) %>% 
  top_n(-8) %>%
  ggplot(aes(x=year,y=kappa)) +
  geom_point(color = "black") +
  # geom_text(aes(label=word),hjust=1, vjust=0, size = 3, color = "red") +
  geom_text_repel(aes(label=word),hjust=2, vjust=0, size = 2, color = "black", max.overlaps = 30) +
  labs(y="Kappa", x="Year") +
  theme_light() #+
  #ggtitle("Estimated most important negative Kappa parameters for debate vocabulary", subtitle = "Monetary policy corpus")
 

# data aggregation
wrd_mon <- df_mon %>% group_by(year) %>% unnest_tokens(word,text) %>% count(word) %>% as.data.frame()
a_mon <- k_m %>% left_join(wrd_mon, by = c("word" = "word", "year"="year"))
wordshoal_beta <- data.frame(year=seq(1999,2020,1),beta=wordshoalfit["beta"])

# calculate word loads over groups of words

wl_query1 = "stützung|stabilisierung|liquidität|aufkäufe"
wl_query2 = "geld|zins|potential"

a_mon %>% left_join(wordshoal_beta, by=c("year"="year")) %>% 
  dplyr::filter(grepl(wl_query1,word)) %>% group_by(year) %>% replace_na(list(n=0,kappa=0)) %>%
  summarize(wrd_load=sum(n*kappa*beta)/sum(n)) %>%  
  ggplot(aes(x=year,y=wrd_load)) + geom_line(col="black") +
  ggtitle(wl_query1) + labs(y="Word loading", x= "Year") +
  theme_light()

a_mon %>% left_join(wordshoal_beta, by=c("year"="year")) %>% 
  dplyr::filter(grepl(wl_query2,word)) %>% group_by(year) %>% replace_na(list(n=0,kappa=0)) %>%
  summarize(wrd_load=sum(n*kappa*beta)/sum(n)) %>%  
  ggplot(aes(x=year,y=wrd_load)) + geom_line(col="black") +
  ggtitle(wl_query2) + labs(y="Word loading", x= "Year") +
  theme_light()

q <- c("stützung|stabilisierung|liquidität|aufkäufe","geld|zins|potential")

q_1<- a_mon %>% left_join(wordshoal_beta, by=c("year"="year")) %>% 
  dplyr::filter(grepl(q[1],word)) %>% group_by(year) %>% replace_na(list(n=0,kappa=0)) %>% 
  summarise(wrd_load=sum(n*kappa*beta)/sum(n)) 

q_2<- a_mon %>% left_join(wordshoal_beta, by=c("year"="year")) %>% 
  dplyr::filter(grepl(q[2],word)) %>% group_by(year) %>% replace_na(list(n=0,kappa=0)) %>% 
  summarise(wrd_load=sum(n*kappa*beta)/sum(n)) 

q_1 %>% left_join(q_2, by=c("year"="year"),suffix = c("_m", "_f")) %>%
  ggplot(aes(year)) + 
  geom_line(aes(y = wrd_load_m, colour = q[1]), linetype = "dashed", size = 1) +
  geom_line(aes(y = wrd_load_f, colour = q[2]), size = 1) +
  #scale_x_continuous(labels=seq(1999,2017,1),breaks=seq(1999,2017,1)) +
  scale_x_continuous(breaks=c(2000, 2005, 2010, 2015, 2020)) +
  theme_minimal() +
  theme(legend.title=element_blank()) + theme(legend.position = "none")+
  theme(text = element_text(size=24))+
  ylab("") +xlab("")

# Collect data for document position plotting

cal_mon <- data.frame(year=as.numeric(as.character(wordshoalfit[["groups"]])),
                      instit=as.character(wordshoalfit[["authors"]]),
                      psi=as.numeric(wordshoalfit[["psi"]])) %>%
  left_join(data.frame(year=seq(1999,2020,1),beta=wordshoalfit[["beta"]]),by=c("year"="year")) %>% 
  left_join(data.frame(instit=levels(wordshoalfit$authors),theta=wordshoalfit[["theta"]]),by=c("instit"="instit"))

# compute year-specific position for institutions

cal_mon <- cal_mon %>% mutate(doc_pos=psi*beta)

# year-specific positions

ggplot(cal_mon,aes(x=year,y=doc_pos,col=instit)) +
  geom_point(size=1) +
  geom_text(aes(label=instit),hjust=1, vjust=0) +
  geom_smooth(se = FALSE)+
  theme_minimal() +
  theme(legend.title = element_blank(),legend.position = "none")+
  theme(text = element_text(size=18))+
  scale_x_continuous(breaks=c(2000, 2005, 2010, 2015, 2020)) +
  #scale_x_continuous(breaks=cal_mon$year)+
  xlab("") + ylab("")

# graph without labels, but with legend
ggplot(cal_mon,aes(x=year,y=doc_pos,col=as.factor(instit))) +
  geom_point(size=1.5, aes(shape=as.factor(instit))) +
  # geom_text(aes(colour = instit)) +
  # geom_text(aes(label=instit),hjust=1, vjust=0) +
  geom_smooth(se = FALSE)+
  theme_minimal() + 
  theme(legend.title = element_blank(),legend.position = "bottom")+
  theme(text = element_text(size=18))+
  scale_x_continuous(breaks=c(2000, 2005, 2010, 2015, 2020)) +
  #scale_x_continuous(breaks=cal_mon$year)+
  xlab("") + ylab("")

# Prepare for higher time resolution

cal_mon2 <- data.frame(cal_mon,month=sprintf("%02d",df_mon$month),
                       yr_mo=ymd(paste0(df_mon$year,"-",df_mon$month),truncated = 1)) %>%
  mutate(id=paste0(instit,year,"-",month))

# continuous scale for positions

cal_mon2 %>% 
  ggplot(aes(x=yr_mo,y=doc_pos,col=instit)) + 
  geom_point(size=1) +
  #geom_line(aes(y=beta),color="black") +
  geom_text(aes(label=instit),hjust=1, vjust=0) +
  #geom_smooth(se = F)
  theme_minimal() +
  theme(legend.title = element_blank(),legend.position = "none") +
  #scale_x_continuous(breaks=c(2000, 2005, 2010, 2015, 2017)) +
  scale_x_date(date_breaks="24 month", date_labels="%Y %m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") + ylab("")

## Wordshoal section
# Fiscal policy

wordshoalfit2 <- textmodel_wordshoal_modif(vdfm2, dir = c(1,163),
                                     groups = docvars(vdfm2, "year"), 
                                     authors = docvars(vdfm2, "institution"),
                                     prioralpha = 0.5,
                                     priorbeta = 0.5,
                                     priortheta = 1,
                                     priortau = 1)

author_positions2 <- summary(wordshoalfit2)$estimated.author.positions
author_positions2$row_names <- rownames(author_positions2)

fitdf2 <- merge(author_positions2,
                docvars(vdfm2), 
                by.x = "row_names", by.y = "institution")
fitdf2 <- subset(fitdf2, !duplicated(id))

aggregate(theta ~ year, data = fitdf2, mean)

# transfering to LaTeX
a2 <- summary(wordshoalfit2) 

print(xtable(a2$estimated.author.positions, type = "latex"))

# Fiscal corpus: Theta plot for paper

summary(wordshoalfit2)$estimated.author.positions %>%
  ggplot(aes(x=reorder(rownames(.),-theta),y=theta,col="darkred")) + 
  scale_x_discrete(labels = c("DIW" = "Berlin institute (DIW)", "RWI" =  "Essen institute (RWI)", "IFO" = "Munich institute (ifo)", "IWH" = "Halle institute (IWH)",  "GED" = "Joint diagnosis",  "IFW" = "Kiel institute (IfW)" ) ) +
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=theta-1.96*se, ymax=theta+1.96*se), width=.2,
                position=position_dodge(.9),size=1) +
  coord_flip() +
  theme(axis.text=element_text(size=16)) +
  theme_minimal() +
  theme(legend.title = element_blank(),legend.position = "none") +
  theme(text = element_text(size=28))+
  xlab("") + ylab("")

# Collect the beta values

fisc_beta <- data.frame(year=seq(1999,2020,1),beta=wordshoalfit2$beta) %>% arrange(-abs(beta))
fisc_alpha <- data.frame(year=seq(1999,2020,1),alpha=wordshoalfit2$alpha) %>% arrange(-abs(alpha))

# Transfer beta results to LaTeX

print(fisc_beta, row.names=FALSE) %>% xtable(digits=c(0,0,2))

# plot figure 

fisc_beta %>% ggplot(aes(x=year,y=abs(beta),col="darkred")) + 
  geom_line(size=1) +
  theme_minimal() +
  theme(legend.title = element_blank(),legend.position = "none")+
  theme(text = element_text(size=28)) +
  scale_x_continuous(breaks=c(2000, 2005, 2010, 2015, 2020)) +
  #theme(text = element_text(size=20))+
  #theme_light()+
  ylab("") + xlab("") +
  ylim(0,0.6)


# plot figure 

fisc_alpha %>% ggplot(aes(x=year,y=(alpha),col="darkred")) + 
  geom_line(size=1) +
  theme_minimal() +
  theme(legend.title = element_blank(),legend.position = "none")+
  theme(text = element_text(size=28)) +
  scale_x_continuous(breaks=c(2000, 2005, 2010, 2015, 2020)) +
  #theme(text = element_text(size=20))+
  #theme_light()+
  ylab("") + xlab("") + 
  ylim(-0.2,0.2)


# Compute Debate loading (Lauderdale & Herzog, 2016)

deb_load_fisc <- data.frame(num=df_fisc %>%
                              #dplyr::filter(year > "2008") %>% 
                              group_by(year) %>% 
                              count(year), 
                            b=wordshoalfit2[["beta"]]) 

deb_load_fisc %>% summarise(q=sqrt(sum(num.n*b*b)/sum(num.n)))

deb_load_fisc %>% filter(num.year>=2008) %>%
  summarise(q=sqrt(sum(num.n*b*b)/sum(num.n)))

deb_load_fisc %>% filter(num.year<2008) %>%
  summarise(q=sqrt(sum(num.n*b*b)/sum(num.n)))

# Compute Word Loading (Lauderdale & Herzog, 2016)

kappa2 <- wordshoalfit2[["kappa"]] %>% lapply(as.data.frame) # extract the kappa values from wordshoal estimation results
kappa2_ij <- kappa2 %>% reduce(full_join, by = "features") # dplyr::join all rows and columns
names(kappa2_ij) <- c("word",seq(1999,2020,1)) # add names
k_m2 <- kappa2_ij %>% gather(value=kappa,key=year,-word) %>% mutate(year=as.numeric(year))

# top 10 words contributing to positive positions for each year
k_m2 %>% 
  group_by(year) %>% 
  top_n(10) %>% 
  View()

k_m2 %>% 
  group_by(year) %>% 
  top_n(8) %>%
  ggplot(aes(x=year,y=kappa)) +
  geom_point(color = "black") +
  # geom_text(aes(label=word),hjust=1, vjust=0, size = 3, color = "blue") +
  geom_text_repel(aes(label=word),hjust=2, vjust=0, size = 2, color = "black", max.overlaps = 30) + 
  labs(y="Kappa", x="Year") +
  theme_light() #+
  #ggtitle("Estimated most important negative Kappa parameters for debate vocabulary", subtitle = "Fiscal policy corpus")


# top 10 words contributing to negative positions for each year
k_m2 %>% 
  group_by(year) %>% 
  top_n(-10) %>% 
  View()

k_m2 %>% 
  group_by(year) %>% 
  top_n(-8) %>%
  ggplot(aes(x=year,y=kappa)) +
  geom_point(color = "black") +
  # geom_text(aes(label=word),hjust=1, vjust=0, size = 3, color = "red") +
  geom_text_repel(aes(label=word),hjust=2, vjust=0, size = 2, color = "black", max.overlaps = 30) +
  labs(y="Kappa", x="Year") +
  theme_light() #+
  #ggtitle("Estimated most important positive Kappa parameters for debate vocabulary", subtitle = "Fiscal policy corpus")

# data aggregation

wrd_fisc <- df_fisc %>% group_by(year) %>% unnest_tokens(word,text) %>% count(word) %>% as.data.frame()
a_fisc <- k_m2 %>% left_join(wrd_fisc, by = c("word" = "word", "year"="year"))
wordshoal_beta2 <- data.frame(year=seq(1999,2020,1),beta=wordshoalfit2["beta"])

# calculate word loading over groups of other words 

#wl_query = "konsolidierung|schuld|reform"
wl_query1 = "reform|konsolidierung"
wl_query2 = "nachfrage|ausgabe"

# calculate over groups of words

a_fisc %>% left_join(wordshoal_beta2, by=c("year"="year")) %>% 
  dplyr::filter(grepl(wl_query1,word)) %>% group_by(year) %>% replace_na(list(n=0,kappa=0)) %>%
  summarize(wrd_load=sum(n*kappa*beta)/sum(n)) %>%  
  ggplot(aes(x=year,y=wrd_load)) + geom_line(col="black") +
  ggtitle(wl_query1) + labs(y="Word loading", x= "Year") + 
  theme_light()

# calculate over groups of words 

a_fisc %>% left_join(wordshoal_beta2, by=c("year"="year")) %>% 
  dplyr::filter(grepl(wl_query2,word)) %>% group_by(year) %>% replace_na(list(n=0,kappa=0)) %>%
  summarize(wrd_load=sum(n*kappa*beta)/sum(n)) %>%  
  ggplot(aes(x=year,y=wrd_load)) + geom_line(col="black") +
  ggtitle(wl_query2) + labs(y="Word loading", x= "Year") +
  theme_light()

q <- c("reform|konsolidierung","nachfrage|ausgabe")

q_1<- a_fisc %>% left_join(wordshoal_beta2, by=c("year"="year")) %>% 
  dplyr::filter(grepl(q[1],word)) %>% group_by(year) %>% replace_na(list(n=0,kappa=0)) %>% 
  summarise(wrd_load=sum(n*kappa*beta)/sum(n)) 

q_2<- a_fisc %>% left_join(wordshoal_beta2, by=c("year"="year")) %>% 
  dplyr::filter(grepl(q[2],word)) %>% group_by(year) %>% replace_na(list(n=0,kappa=0)) %>% 
  summarise(wrd_load=sum(n*kappa*beta)/sum(n)) 

q_1 %>% left_join(q_2, by=c("year"="year"),suffix = c("_m", "_f")) %>%
  ggplot(aes(year)) + 
  geom_line(aes(y = wrd_load_m, colour = q[1]), linetype = "dashed", size = 1) +
  geom_line(aes(y = wrd_load_f, colour = q[2]), size = 1) +
  #scale_x_continuous(labels=seq(1999,2017,1),breaks=seq(1999,2017,1)) +
  scale_x_continuous(breaks=c(2000, 2005, 2010, 2015, 2020)) +
  theme_minimal() +
  theme(legend.title=element_blank()) + theme(legend.position = "none")+
  theme(text = element_text(size=24))+
  ylab("") +xlab("")

# Wordshoal calibration fiscal policy
# necessary to plot different document positions over time.

cal_fisc <- data.frame(year=as.numeric(as.character(wordshoalfit2[["groups"]])),instit=as.character(wordshoalfit2[["authors"]]),psi=as.numeric(wordshoalfit2[["psi"]])) %>%
  left_join(data.frame(year=seq(1999,2020,1),beta=wordshoalfit2[["beta"]]),by=c("year"="year")) %>% 
  left_join(data.frame(instit=levels(wordshoalfit2$authors),theta=wordshoalfit2[["theta"]]),by=c("instit"="instit"))

# compute year-specific position for institutions

cal_fisc <- cal_fisc %>% mutate(doc_pos=psi*beta)

# year-specific positions (debate-specific position) 

ggplot(cal_fisc,aes(x=year,y=doc_pos,col=instit)) +
  geom_point(size=1) +
  geom_text(aes(label=instit),hjust=1, vjust=0) +
  geom_smooth(se = FALSE)+
  theme_minimal() +
  theme(legend.title = element_blank(),legend.position = "none")+
  theme(text = element_text(size=18))+
  scale_x_continuous(breaks=c(2000, 2005, 2010, 2015, 2020)) +
  #scale_x_continuous(breaks=cal_mon$year)+
  xlab("") + ylab("")

# graph without labels, but with legend
ggplot(cal_fisc,aes(x=year,y=doc_pos, col=as.factor(instit))) +
  geom_point(size=1.5, aes(shape=as.factor(instit))) +
  # geom_text(aes(colour = instit)) +
  # geom_text(aes(label=instit),hjust=1, vjust=0) +
  geom_smooth(se = FALSE)+
  theme_minimal()  + ylim(c(-1,1)) +
  theme(legend.title = element_blank(),legend.position = "right")+
  theme(text = element_text(size=18))+
  scale_x_continuous(breaks=c(2000, 2005, 2010, 2015, 2020)) +
  #scale_x_continuous(breaks=cal_mon$year)+
  xlab("") + ylab("")  + theme(legend.position = "bottom")

# reorganize data
cal_fisc2 <- data.frame(cal_fisc,month=sprintf("%02d",df_fisc$month),
                        yr_mo=ymd(paste0(df_fisc$year,"-",df_fisc$month),truncated = 1)) %>%
  mutate(id=paste0(instit,year,"-",month))

# continuous scale for year-specific positions without beta

cal_fisc2 %>% 
  ggplot(aes(x=yr_mo,y=doc_pos,col=instit)) + 
  geom_point(size=1.5, aes(shape=as.factor(instit))) +
  #geom_line(aes(y=beta),color="black") +
  geom_text(aes(label=instit),hjust=1, vjust=0) +
  theme_minimal() +
  theme(legend.title = element_blank(),legend.position = "none") +
  scale_x_date(date_breaks="24 month", date_labels="%Y %m") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("") + ylab("")  + theme(legend.position = "bottom") 

## SAVE RESULTS

# save wordshoal results of both corpora in a RData file
wordshoal_results <- cal_mon2 %>% full_join(cal_fisc2 %>% dplyr::select(id,psi,beta,theta,doc_pos), by=c("id"="id"), suffix=c(".mon",".fisc"))

