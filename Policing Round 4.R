#####1.Setting Up
library (dplyr)
library(rio)
library(ggplot2)
library(DescTools)
library(tidyr)
library(tibble)
library(haven)
library(gridExtra)
library(reshape2)
library(quanteda)
CES<-read_sav("CESDATA/CCES22_USC_OUTPUT.sav")

### Grabbing the questions and information we "care about" in the context of this study
ICareCES<-select(CES, CC22_307, birthyr, gender4, educ, 
                 race, hispanic,
                 multrace_1,multrace_2,multrace_3,multrace_4,
                 multrace_5,multrace_8,multrace_97,multrace_98,
                 UCC437,UCC438,UCC439,UCC310,UCC315,
                 pid3,pid7,CC22_300_1,CC22_300_2,CC22_300_3, 
                 CC22_300_4,CC22_300_5,CC22_300a,CC22_300c,
                 CC22_300b_1,CC22_300b_2,CC22_300b_3,CC22_300b_4,
                 CC22_300b_5,CC22_300b_6,CC22_300b_7,CC22_300b_8,
                 CC22_300d_1,CC22_300d_2,CC22_300d_3,
                 CC22_300d_4,CC22_300d_5,CC22_300d_6,
                 UCC434_N,UCC435,UCC436)

### Gonna make a dataset of all the variables just for really easy univariate work if I ever want to do that
datasets <- list()
for(col_name in names(ICareCES)) {
  datasets[[col_name]] <- data.frame(ICareCES[, col_name])}

FinalUni<-datasets
rm(datasets)
FinalUni<-lapply(FinalUni,na.omit)
lapply(FinalUni,nrow)

### Now we're going to clean up the ICare Data, But in this case we're making an experimental condition a variable declaring how the individuals were prompted which we can use later with the Police Thermostat, but other than that we're going back to data cleaning Yippee :/
ICareCES<-ICareCES%>%mutate(experimentalcondition=ifelse(UCC434_N!="__NA__"&UCC434_N!="-1", "Positive", ifelse(UCC435!="__NA__"&UCC435!="-1","Negative",ifelse(UCC436!="__NA__"&UCC436!="-1", "Neutral", NA))))
ICareCES<-ICareCES%>%mutate(experimentalconditionNums=recode(experimentalcondition,
"Positive"="1",
"Negative"="2",
"Neutral"="3"))

ICareCES = ICareCES %>%
  mutate(
gender_labeled = factor(gender4, 
                        levels = c(1,2,3,4), 
                        labels= c("male","female","non-binary","other")),
# Labeling the "race" variable. Here you might also clean the "hispanic" variable but don't need to clean each of the multiracial variables at this point in time. 
race_labeled = factor(race, 
                      levels = c(1,2,3,4,5,6,7,8),
                      labels=c("White","Black","Hispanic","Asian",
                               "Native American","Multiracial","Other",
                               "Middle Eastern")),
police_feelsafe = factor(CC22_307, levels=1:4, 
                         labels=c("Mostly Safe",
                                  "Somewhat Safe",
                                  "Somewhat Unsafe",
                                  "Mostly Unsafe")),

###Now I'm just going to grab all the other important variables and create a text based version of them, and yeah, that's it
Pid7WithLean=factor(pid7,levels=1:9,labels=c("StrongDem","WeakDem","LeanDem","Independent","LeanRep","WeakRep","StrongRep","NotSure","DontKnow")),
Pid7NoLean=factor(pid7,levels=1:9,labels=c("Dem","Dem","Dem","Independent","Rep","Rep","Rep","NotSure","DontKnow")),
Education=factor(educ,levels=1:6,labels=c("NoHS","HS","SomeCollege","2yr","4yr","Post-Grad")),
ICareCES$CC22_307<-factor(ICareCES$CC22_307, levels=c(1,2,3,4), labels=c("MostSafe","SomeSafe","SomeUnsafe","MostUnsafe")),
ICareCES$UCC438<-factor(ICareCES$UCC438,levels=c(-1,1,2,3,4,5),labels=c(NA,"StrongAgree","Agree",
"Neutral","Disagree","StrongDisagree")),
ICareCES$UCC439<-factor(ICareCES$UCC439,levels=c(-1,1,2,3,4,5),labels=c(NA,"StrongAgree","Agree", "Neutral","Disagree","StrongDisagree")),
hispanicW=factor(hispanic, levels = c(1,2), labels= c("Yes","No")),
SocialMedia=factor(CC22_300_1, levels = c(1,2), labels= c("Selected","Not Selected")),
 TVNews=factor(ICareCES$CC22_300_2, levels = c(1,2), labels= c("Selected","Not Selected")),
NewsPaper=factor(CC22_300_3, levels = c(1,2), labels= c("Selected","Not Selected")),
 RadioNews=factor(CC22_300_4, levels = c(1,2), labels= c("Selected","Not Selected")),
NoNews=factor(CC22_300_5, levels = c(1,2), labels= c("Selected","Not Selected")),
ABC=factor(CC22_300b_1, levels = c(1,2,3), labels= c("Selected","Not Selected", "Not Asked")),
CBS=factor(CC22_300b_2, levels = c(1,2,3), labels= c("Selected","Not Selected", "Not Asked")),
NBC=factor(CC22_300b_3, levels = c(1,2,3), labels= c("Selected","Not Selected", "Not Asked")),
CNN=factor(CC22_300b_4, levels = c(1,2,3), labels= c("Selected","Not Selected", "Not Asked")),
Fox=factor(CC22_300b_5, levels = c(1,2,3), labels= c("Selected","Not Selected", "Not Asked")),
 MSNBC=factor(CC22_300b_6, levels = c(1,2,3), labels= c("Selected","Not Selected", "Not Asked")),
PBS=factor(CC22_300b_7, levels = c(1,2,3), labels= c("Selected","Not Selected", "Not Asked")),
 Other=factor(CC22_300b_8, levels = c(1,2,3), labels= c("Selected","Not Selected", "Not Asked")),
PostPoliticsMain=factor(CC22_300d_1, levels = c(1,2,3), labels= c("Selected","Not Selected", "Not Asked")),
 CommentPolitics=factor(CC22_300d_2, levels = c(1,2,3), labels= c("Selected","Not Selected", "Not Asked")),
ReadStoryPolitics=factor(CC22_300d_3, levels = c(1,2,3), labels= c("Selected","Not Selected", "Not Asked")),
 FollowedPoliticEvent=factor(CC22_300d_4, levels = c(1,2,3), labels= c("Selected","Not Selected", "Not Asked")),
ForwardsPoliticsFriends=factor(CC22_300d_5, levels = c(1,2,3), labels= c("Selected","Not Selected", "Not Asked")),
MediaPoliticsNoneAbove=factor(CC22_300d_6, levels = c(1,2,3), labels= c("Selected","Not Selected", "Not Asked")),
TVNewsTypes=factor(CC22_300a, levels=c("1","2","3"), labels= c("Local","National", "Both")),
NewsPaperTypes=factor(ICareCES$CC22_300c, levels = c("1","2","3"), labels= c("Print","Online", "Both")), 
FriendsFamilyNews=factor(UCC310, levels=c("1","2","3","4","5"), labels=c("VFrequently","Frequently","Occasionally","Rarely","Never")),
PoliceAcademyPart=factor(UCC439, levels=c("-1",1,2,3,4,5),labels=c("Nodata","StrongAgree","Agree","Neutral","Disagree","StrongDisagree")),
OtherNetwork=factor(CC22_300b_8, levels=c(1,2), labels=c("Selected","NotSelected")),
PoliceThermo=UCC437)
###Now we're going to incorporate Income into ICareCES
ICareCES<-bind_cols(ICareCES, CES$faminc_new)
ICareCES<-ICareCES %>% rename(famincome=...81)
ICareCES=ICareCES%>%
  mutate(famincome=factor(famincome,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","97","998"),labels=c("0-10000","10000-19999","20000-29999","30000-39999","40000-49999","50000-59999","60000-69999","70000-79999","80000-99999","100000-119999","120000-149999","150000-199999","200000-249999","250000-349999","350000-499999","500000+","NS",NA)))
### Now has been incorporated! And we make Regression Tables Yippee
###Retesting our 1st Hypothesis, now that we've recleaned the data in a more "socially acceptable" manner (Not a jab, realizing it sounds like a jab), This is Online vs Print, but maybe I should also sort by local and national to make it a multi tri column

HypothesisOne<-data.frame(ICareCES$PoliceThermo,ICareCES$NewsPaperTypes)
HypothesisOne<-na.omit(HypothesisOne)
HypothesisOneGraph<-ggplot(HypothesisOne, aes(x=ICareCES.PoliceThermo, y=ICareCES.NewsPaperTypes))+geom_boxplot()

### Retesting some correlational stuff with the new cleaned datas
ProvenRelationship<-data.frame(ICareCES$race_labeled,ICareCES$PoliceThermo)
ProvenRelationship<-(na.omit(ProvenRelationship))
ggplot(ProvenRelationship, aes(x=ICareCES.race_labeled,y=ICareCES.PoliceThermo))+geom_boxplot()

#### Hypothesis 2 intitial testing, might want to return to this later, When we sort by those who also don't really watch the news, but will do later
HypothesisTwo<-data.frame(ICareCES$FriendsFamilyNews,ICareCES$Pid7NoLean,ICareCES$PoliceThermo)
HypothesisTwo<-filter(HypothesisTwo,ICareCES.FriendsFamilyNews!="Never")
HypothesisTwo<-filter(HypothesisTwo,ICareCES.FriendsFamilyNews!="Rarely")
HypothesisTwo<-na.omit(HypothesisTwo)
HypothesisTwoGraph<-ggplot(HypothesisTwo, aes(x=ICareCES.Pid7NoLean,y=ICareCES.PoliceThermo))+geom_boxplot()+ggtitle("Americans who occassionally+ learn about police from friends and Family: opinions on police based on Party Affiliation")
HypothesisTwoPart2<-data.frame(ICareCES$Pid7NoLean,ICareCES$PoliceThermo)
HypothesisTwoTooGraph<-ggplot(HypothesisTwo, aes(x=ICareCES.Pid7NoLean,y=ICareCES.PoliceThermo))+geom_boxplot()+ggtitle("Americans Opinion of Police, sorted by political Party")
grid.arrange(HypothesisTwoGraph,HypothesisTwoTooGraph, ncol=2)

####Text-based equation priming To PoliceThermometer Correlation 🔥🔥🔥🔥🔥🔥🔥🔥
PrimingThermometer<-data.frame(ICareCES$experimentalcondition,ICareCES$PoliceThermo)
PrimingThermometer<-na.omit(PrimingThermometer)
PrimingThermometerGraph<-ggplot(PrimingThermometer, aes(x=ICareCES.experimentalcondition,ICareCES.PoliceThermo))+geom_boxplot()+ggtitle("Police Opinions organized by priming questions")
pdf("PrimingThermCalc.PDF",7.5,5)
PrimingThermometerGraph
dev.off()
pdf("Hypothesis2Calc.PDF",7.5,5)
HypothesisTwoGraph
dev.off()
pdf("Hypothesis2Calcalso.PDF",7.5,5)
HypothesisTwoTooGraph
dev.off()
pdf("Hypothesis1.PDF",7.5,5)
HypothesisOneGraph
dev.off()

HypothesisThree<-data.frame(ICareCES$PoliceThermo,ICareCES$PoliceAcademyPart)
na.omit(HypothesisThree)
HypothesisThreeGraph<-ggplot(HypothesisThree, aes(x=ICareCES.PoliceAcademyPart,y=ICareCES.PoliceThermo))+geom_boxplot()+ggtitle("Police Perceptions split by willingness to participate in a Citizen Police Academy")
#### Hypothesis 4, we're gonna sort by networks and calculate that with Police Perception, shaboinger
HypothesisFour<-data.frame(CES$caseid,ICareCES$ABC,ICareCES$CBS,ICareCES$NBC,ICareCES$CNN,ICareCES$Fox,ICareCES$MSNBC,ICareCES$PBS,ICareCES$OtherNetwork,ICareCES$PoliceThermo)
HypothesisFour<-na.omit(HypothesisFour)
###But because people watch multiple networks, we're going to have to change the data into a long format
HypothesisFourLong<-HypothesisFour%>%pivot_longer(cols=ICareCES.ABC:ICareCES.OtherNetwork,names_to="Network",values_to = "Value")

HypothesisFourLong=HypothesisFourLong%>%
  mutate(
Network=factor(Network, levels=c("ICareCES.ABC","ICareCES.NBC","ICareCES.CBS","ICareCES.CNN","ICareCES.Fox","ICareCES.MSNBC","ICareCES.PBS","ICareCES.OtherNetwork"), labels=c("ABC","NBC","CBS","CNN","Fox","MSNBC","PBS","Other")))
HypothesisFourLong<-filter(HypothesisFourLong, Value=="Selected")

NewsStationPerception<-ggplot(HypothesisFourLong,aes(x=Network,y=ICareCES.PoliceThermo))+geom_boxplot()+ggtitle("Police Perceptions Sorted by Network Choice of Respondents")
pdf("NetworkPolicePerceptionCorrelation.PDF",7.5,5)
NewsStationPerception
dev.off()
#### Chisquare, DV, Race Gender are all gonna matter? slap it all in the same model Run Regression.... DV=Numeric, run a regression, difference of means. ANOVA, unless it's a count then it's a different ype of regression, OLS,  lm(), 
### lm( Dependent  ~Independent, data=data.frame), Regression works for multiple things 
###Facet Wrap, by Political Party and News
###Use TechReg/StarGazer for making sexy Regression Tables
###Quanteda and also Tidy text for text analysis
###Write a paragraph about regression
###Use Github
###What is R markdown, R markdown work with it in R Studio, Instead of jumping docs and code, integrate it all together
###HuggingFace, Absolutely Terrifying name, use that as an LLM
###Gpt For All, LLM space, OpenAI, pay to do things you want to do
###Baseline Free Version, API to automatically connect to it and give it information

###Resorting Hypothesis Four to be by political Party as opposed to just network
HypothesisFourPolitical<-data.frame(CES$caseid,ICareCES$ABC,ICareCES$CBS,ICareCES$NBC,ICareCES$CNN,ICareCES$Fox,ICareCES$MSNBC,ICareCES$PBS,ICareCES$OtherNetwork,ICareCES$PoliceThermo,ICareCES$Pid7WithLean,ICareCES$Pid7NoLean)
HypothesisFourPolitical<-na.omit(HypothesisFourPolitical)
HypothesisFourPoliticalLong<-HypothesisFourPolitical%>%pivot_longer(cols=ICareCES.ABC:ICareCES.OtherNetwork,names_to="Network",values_to = "Value")
HypothesisFourPoliticalLong=HypothesisFourPoliticalLong%>%
  mutate(
    Network=factor(Network, levels=c("ICareCES.ABC","ICareCES.NBC","ICareCES.CBS","ICareCES.CNN","ICareCES.Fox","ICareCES.MSNBC","ICareCES.PBS","ICareCES.OtherNetwork"), labels=c("ABC","NBC","CBS","CNN","Fox","MSNBC","PBS","Other")))

HypothesisFourPoliticalLong=HypothesisFourPoliticalLong%>%
  mutate(ICareCES.Pid7NoLean=factor(
ICareCES.Pid7NoLean,levels=c("Dem","Rep","Independent","NotSure"), labels=c("Dem","Rep","Ind","NS")))


HypothesisFourPoliticalLong<-filter(HypothesisFourPoliticalLong, Value=="Selected")
NewsStationPerceptionPolitical<-ggplot(HypothesisFourPoliticalLong, aes(x=ICareCES.Pid7NoLean,y=ICareCES.PoliceThermo, fill=ICareCES.Pid7NoLean))+geom_boxplot(position = position_dodge())+facet_wrap(~Network) + scale_fill_manual(values = c("Rep" = "red", "Dem" = "blue","Ind"="White","NS"="Purple"))+ggtitle("Survey Results, sorted by political party")
pdf("NetworkPolicePerceptionPolitical.PDF",7.5,5)
NewsStationPerceptionPolitical
dev.off()
###install.packages("stargazer")
library(stargazer)
### lm( Dependent  ~Independent, data=data.frame),
### That's Kooky I didn't realize that the Latex File Didn't give me P, and I had to use the *** to read that, that's wild, Stargazer latex files look weird
####Let's test the graph that we just made
### New Week New Grind Session: Fit the following 6 regressions each of which predict the the police thermometer variable and then put them in the same table: (1) race; (2) party id; (3) news consumption; (4) experimental condition; (5) all the above + socioeconomic status. Then repeat this for our other two outcomes of interest: UCC438 and UCC439. 

RaceRegression<-lm(PoliceThermo ~race_labeled, data=ICareCES)
stargazer(RaceRegression, type="latex")
PartyRegression<-lm(PoliceThermo ~Pid7NoLean, data=ICareCES)
stargazer(PartyRegression, type="latex")
NewsconsumptionRegression<-lm(ICareCES.PoliceThermo ~Network, data=HypothesisFourPoliticalLong)
stargazer(NewsconsumptionRegression, type="latex")
PrimedThoughtRegression<-lm(PoliceThermo ~experimentalcondition, data=ICareCES)
stargazer(PrimedThoughtRegression, type="latex")
IncomeRegression<-lm(PoliceThermo ~famincome, data=ICareCES)

stargazer(PartyRegression, type="latex")
stargazer(IncomeRegression, type="latex")
stargazer(NewsconsumptionRegression, type="latex")
stargazer(RaceRegression, type="latex")
stargazer(PrimedThoughtRegression,type="latex")

####Before we can do the multiRegression, we have to Pivot ALL THE DATA longer, Womp Womp!!!
MultiRegressionDataFrame<-data.frame(ICareCES$race_labeled,ICareCES$Pid7NoLean,ICareCES$experimentalcondition,ICareCES$famincome,ICareCES$ABC,ICareCES$CBS,ICareCES$NBC,ICareCES$CNN,ICareCES$Fox,ICareCES$MSNBC,ICareCES$PBS,ICareCES$OtherNetwork,ICareCES$PoliceThermo,ICareCES$UCC438,ICareCES$PoliceAcademyPart)
MultiRegressionDataFrame=MultiRegressionDataFrame%>%mutate(ICareCES.UCC438=factor(ICareCES.UCC438, levels=c("-1","1","2","3","4","5"), labels=c(NA,"SAgree","Agree","Neutral","Disagree","SDisagree")))
MultiRegressionDataFrame<-MultiRegressionDataFrame%>%pivot_longer(cols=ICareCES.ABC:ICareCES.OtherNetwork,names_to="Network",values_to = "Value")
MultiRegressionDataFrame<-filter(MultiRegressionDataFrame, Value=="Selected")
NAOmittedMultiRegressionDataFrame<-na.omit(MultiRegressionDataFrame)
MultiRegressionDataFrame=MultiRegressionDataFrame%>%
  mutate(
    Network=factor(Network, levels=c("ICareCES.ABC","ICareCES.NBC","ICareCES.CBS","ICareCES.CNN","ICareCES.Fox","ICareCES.MSNBC","ICareCES.PBS","ICareCES.OtherNetwork"), labels=c("ABC","NBC","CBS","CNN","Fox","MSNBC","PBS","Other")))
### Okie Dokey, We Pivoted Nice and Long! Time to make MultiRegression TABLE Yipee!!!
### New Week New Grind Session: Fit the following 6 regressions each of which predict the the police thermometer variable and then put them in the same table: (1) race; (2) party id; (3) news consumption; (4) experimental condition; (5) all the above + socioeconomic status. Then repeat this for our other two outcomes of interest: UCC438 and UCC439. 

RaceRegressionPoliceThermo<-lm(PoliceThermo ~race_labeled, data=ICareCES)

PartyRegressionPoliceThermo<-lm(PoliceThermo ~Pid7NoLean, data=ICareCES)

NewsconsumptionRegressionPoliceThermo<-lm(PoliceThermo ~CC22_300b_1+CC22_300b_2+CC22_300b_3+CC22_300b_4+CC22_300b_5+CC22_300b_6+CC22_300b_7+CC22_300b_8, data=ICareCES)

PrimedThoughtRegressionPoliceThermo<-lm(PoliceThermo ~experimentalcondition, data=ICareCES)

IncomeRegressionPoliceThermo<-lm(PoliceThermo ~famincome, data=ICareCES)

PoliceBulkThermoRegression<-lm(PoliceThermo ~race_labeled+Pid7NoLean+experimentalcondition+famincome+CC22_300b_1+CC22_300b_2+CC22_300b_3+CC22_300b_4+CC22_300b_5+CC22_300b_6+CC22_300b_7+CC22_300b_8, data=ICareCES)

stargazer(RaceRegressionPoliceThermo, type="text")
stargazer(PartyRegressionPoliceThermo, type="latex")
stargazer(NewsconsumptionRegressionPoliceThermo, type="latex")
stargazer(PrimedThoughtRegressionPoliceThermo, type="latex")
stargazer(IncomeRegressionPoliceThermo, type="latex")
stargazer(PoliceBulkThermoRegression, type="latex")


### Same Regressions but for UCC 438
RaceRegressionPoliceCall<-lm(UCC438 ~race_labeled, data=ICareCES)

PartyRegressionPoliceCall<-lm(UCC438 ~Pid7NoLean, data=ICareCES)

NewsconsumptionRegressionPoliceCall<-lm(UCC438 ~CC22_300b_1+CC22_300b_2+CC22_300b_3+CC22_300b_4+CC22_300b_5+CC22_300b_6+CC22_300b_7+CC22_300b_8, data=ICareCES)

PrimedThoughtRegressionPoliceCall<-lm(UCC438 ~experimentalcondition, data=ICareCES)

IncomeRegressionPoliceCall<-lm(UCC438 ~famincome, data=ICareCES)

BulkUCC438Regression<-lm(UCC438 ~race_labeled+Pid7NoLean+experimentalcondition+famincome+CC22_300b_1+CC22_300b_2+CC22_300b_3+CC22_300b_4+CC22_300b_5+CC22_300b_6+CC22_300b_7+CC22_300b_8, data=ICareCES)

stargazer(RaceRegressionPoliceCall, type="text")
stargazer(PartyRegressionPoliceCall, type="latex")
stargazer(NewsconsumptionRegressionPoliceCall, type="latex")
stargazer(PrimedThoughtRegressionPoliceCall, type="latex")
stargazer(IncomeRegressionPoliceCall, type="latex")
stargazer(BulkUCC438Regression, type="latex")

###Same Regression but for UCC439
RaceRegressionPoliceCad<-lm(UCC439 ~race_labeled, data=ICareCES)

PartyRegressionPoliceCad<-lm(UCC439 ~Pid7NoLean, data=ICareCES)

NewsconsumptionRegressionPoliceCad<-lm(UCC439 ~CC22_300b_1+CC22_300b_2+CC22_300b_3+CC22_300b_4+CC22_300b_5+CC22_300b_6+CC22_300b_7+CC22_300b_8, data=ICareCES)

PrimedThoughtRegressionPoliceCad<-lm(UCC439 ~experimentalcondition, data=ICareCES)

IncomeRegressionPoliceCad<-lm(UCC439 ~famincome, data=ICareCES)

BulkUCC439Regression<-lm(UCC439 ~race_labeled+Pid7NoLean+experimentalcondition+famincome+CC22_300b_1+CC22_300b_2+CC22_300b_3+CC22_300b_4+CC22_300b_5+CC22_300b_6+CC22_300b_7+CC22_300b_8, data=ICareCES)

stargazer(RaceRegressionPoliceCad, type="text")
stargazer(PartyRegressionPoliceCad, type="latex")
stargazer(NewsconsumptionRegressionPoliceCad, type="latex")
stargazer(PrimedThoughtRegressionPoliceCad, type="latex")
stargazer(IncomeRegressionPoliceCad, type="latex")
stargazer(BulkUCC439Regression, type="latex")

####DONE onto intro to text analysis
#install.packages("stopwords", dependencies = TRUE)/
#install.packages("cowplot", dependencies = TRUE)
#install.packages("quanteda", dependencies = TRUE)
##install.packages("quanteda.textplots", dependencies = TRUE)
#install.packages("quanteda.textstats", dependencies = TRUE)
#install.packages("quanteda.corpus", dependencies = TRUE)
#install.packages("devtools", dependencies = TRUE)

`devtools::install_github("quanteda/quanteda.corpora", force=TRUE)
library(quanteda)
library(stringr)
library(ggplot2)
library(cowplot)
library(quanteda)
library(quanteda.corpora)
library(quanteda.textplots)
library(quanteda.textstats)
library(stopwords)
##update.packages(ask = FALSE)
###Making a new Data Frame, without all that extra hubbub, and then filtering the NA and Coalescing into a singular column
SentimentAnalysis<-data.frame(ICareCES$UCC434_N,ICareCES$UCC435,ICareCES$UCC436)
SentimentAnalysis$ICareCES.UCC434_N[SentimentAnalysis$ICareCES.UCC434_N== "__NA__"] <- NA
SentimentAnalysis$ICareCES.UCC435[SentimentAnalysis$ICareCES.UCC435== "__NA__"] <- NA
SentimentAnalysis$ICareCES.UCC436[SentimentAnalysis$ICareCES.UCC436== "__NA__"] <- NA

SentimentAnalysis$ICareCES.UCC434_N[SentimentAnalysis$ICareCES.UCC434_N== "-1"] <- NA
SentimentAnalysis$ICareCES.UCC435[SentimentAnalysis$ICareCES.UCC435== "-1"] <- NA
SentimentAnalysis$ICareCES.UCC436[SentimentAnalysis$ICareCES.UCC436== "-1"] <- NA

SentimentAnalysis$Mono<- coalesce(SentimentAnalysis$ICareCES.UCC434_N, SentimentAnalysis$ICareCES.UCC435, SentimentAnalysis$ICareCES.UCC436)
SentimentAnalysisFinal<-data.frame(SentimentAnalysis$Mono,ICareCES$experimentalcondition)

###Doing some Text Analysis stuff, that I'm kind of following along with, a lot of really specific jargon that I haven't internalized yet though
Textual<-corpus(SentimentAnalysisFinal, text_field ="SentimentAnalysis.Mono")
summary(Textual)
my_tokens_defaults <- tokens(Textual)
###So Just testing some words I'd expect to see
kwic(my_tokens_defaults, pattern=phrase("George"), window=4)
kwic(my_tokens_defaults, pattern=phrase("black"), window=4)
kwic(my_tokens_defaults, pattern=phrase("Taylor"), window=4)
kwic(my_tokens_defaults, pattern=phrase("Elijah"), window=4)
kwic(my_tokens_defaults, pattern=phrase("love"), window=4)
kwic(my_tokens_defaults, pattern=phrase("White"), window=4)
kwic(my_tokens_defaults, pattern=phrase("Racism"), window=4)
kwic(my_tokens_defaults, pattern=phrase("Race"), window=4)
kwic(my_tokens_defaults, pattern=phrase("women"), window=4)
kwic(my_tokens_defaults, pattern=phrase("woman"), window=4)
### Pre Proccessing but now with Intentions
TokenizedOpi<-tokens(Textual,
                     what = "word",
                     remove_punct = TRUE, # removes punctuation
                     remove_symbols = TRUE, # removes unicode symbols
                     remove_numbers = TRUE, # removes numbers but not words starting with digits
                     remove_url = TRUE, # eliminates URLs beginning with http/https
                     remove_separators = TRUE, # removes separators as categorized in unicode
                     include_docvars = FALSE) # whether to pass on the docvars to the tokens
###I'm a child and named my variable such as one, I will never grow up
PostLSDNotThatKindTokenizedOpi<-tokens_lookup(TokenizedOpi, dictionary = data_dictionary_LSD2015)

dfm_lsd <- dfm(PostLSDNotThatKindTokenizedOpi)

###All of a sudden this piece of code doesn't want to work anymore, it seems as though it's using rio Convert as opposed to Quanteda, and quanteda isn't working anymore for whatever reason... IDK
OpinionSentimentCount<-convert(dfm_lsd, to="data.frame")

all_words <- dfm(TokenizedOpi)
###The Valence Calculations
OpinionSentimentCount$total_words <- rowSums(all_words)
OpinionSentimentCount$valence <- ((OpinionSentimentCount$positive/OpinionSentimentCount$total_words) - 
                                 (OpinionSentimentCount$negative/OpinionSentimentCount$total_words))*100
OpinionSentimentCountExp<-data.frame(OpinionSentimentCount,SquaentimentAnalysisFinal$ICareCES.experimentalcondition)

####This is a filter that finds "Conflicting Sentiment Analysiseseses"
OpinionSentimentCountExpFiltered<-filter(OpinionSentimentCountExp, OpinionSentimentCountExp$valence>0 &OpinionSentimentCountExp$SentimentAnalysisFinal.ICareCES.experimentalcondition=="Negative"| OpinionSentimentCountExp$valence<0 &OpinionSentimentCountExp$SentimentAnalysisFinal.ICareCES.experimentalcondition=="Positive"|OpinionSentimentCountExp$valence!=0 &OpinionSentimentCountExp$SentimentAnalysisFinal.ICareCES.experimentalcondition=="Neutral" )
###So we have a list of outliers, which is very interesting, but other than that, I'mma go back to Text Count, Make some Graphs, but we gotta filter out some stop words

dfm_defaults<-dfm(TokenizedOpi,tolower=FALSE)
dfm_nostopwords  <- dfm_remove(dfm_defaults,
                               stopwords("english"))
dfm_stemmed <- dfm_wordstem(dfm_nostopwords)
top_terms = data.frame("Rank" = 1:25,
                       "Defaults" = names(topfeatures(dfm_defaults,25)),
                       "No Stop Words" = names(topfeatures(dfm_nostopwords,25)),
                       "Stemmed" = names(topfeatures(dfm_stemmed,25)))

#### We're doing some Big Party Work
SentimentAnalysisFinalPartyStyle<-data.frame(SentimentAnalysis$Mono,ICareCES$experimentalcondition,ICareCES$Pid7NoLean)
Textual<-corpus(SentimentAnalysisFinalPartyStyle, text_field ="SentimentAnalysis.Mono")

my_tokens <- tokens(Textual,
                    what = "word",
                    remove_punct = TRUE, # removes punctuation
                    remove_symbols = TRUE, # removes unicode symbols
                    remove_numbers = TRUE, # removes numbers but not words starting with digits
                    remove_url = TRUE, # eliminates URLs beginning with http/https
                    remove_separators = TRUE, # removes separators as categorized in unicode
                    include_docvars = FALSE) # whether to pass on the docvars to the tokens object

dfm_defaults<-dfm(my_tokens,tolower=FALSE)

dfm_nostopwords  <- dfm_remove(dfm_defaults,
                               stopwords("english"))

dfm_stemmed <- dfm_wordstem(dfm_nostopwords)
top_terms = data.frame("Rank" = 1:25,
                       "Defaults" = names(topfeatures(dfm_defaults,25)),
                       "No Stop Words" = names(topfeatures(dfm_nostopwords,25)),
                       "Stemmed" = names(topfeatures(dfm_stemmed,25)))

group_term_matrix_step1<-dfm(dfm_nostopwords)

group_term_matrix<-dfm_group(group_term_matrix_step1,
                              groups = group_term_matrix_step1$ICareCES.Pid7NoLean)

textplot_wordcloud(group_term_matrix, 
                   comparison = TRUE, 
                   max_words = 100)
ndoc()
