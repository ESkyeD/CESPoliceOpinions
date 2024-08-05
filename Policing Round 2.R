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
MyFirstRegressionTableWhereIUsePData<-lm(PoliceThermo ~Pid7WithLean, data=ICareCES)
MyFirstRegressionReal<-stargazer(MyFirstRegressionTableWhereIUsePData, type = "latex")
### That's Kooky I didn't realize that the Latex File Didn't give me P, and I had to use the *** to read that, that's wild, Stargazer latex files look weird
####Let's test the graph that we just made
PartysandNewsSourceRegression<-lm(ICareCES.PoliceThermo ~ICareCES.Pid7NoLean+Network, data=HypothesisFourPoliticalLong)
PartysandNewsSourceRegressionLatex<-stargazer(PartysandNewsSourceRegression, type="latex")