library(readr)
library(dplyr)
library(knitr)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
c103<-read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_C.csv")
c104<-read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_C.csv")
c105<-read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_C.csv")
c106<-read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_C.csv")
s103<-read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_S.csv")
s104<-read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_S.csv")
s105<-read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_S.csv")
s106<-read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_S.csv")
ods_result<-read_csv("Student_RPT_07.csv")
world<-read_csv("https://ws.moe.edu.tw/Download.ashx?u=C099358C81D4876CC7586B178A6BD6D5062C39FB76BDE7EC7685C1A3C0846BCDD2B4F4C2FE907C3E7E96F97D24487065577A728C59D4D9A4ECDFF432EA5A114C8B01E4AFECC637696DE4DAECA03BB417&n=4E402A02CE6F0B6C1B3C7E89FDA1FAD0B5DDFA6F3DA74E2DA06AE927F09433CFBC07A1910C169A1845D8EB78BD7D60D7414F74617F2A6B71DC86D17C9DA3781394EF5794EEA7363C&icon=..csv")

####1.1
cleandata_c<-function(x){
  y<-
    x%>%
    mutate(總人數=rowSums(x[,3:11]))%>%
    select(國別,總人數)
  return(y)
}
total_c103<-cleandata_c(c103)
colnames(total_c103)=c("國別","總人數103")
total_c104<-cleandata_c(c104)
colnames(total_c104)=c("國別","總人數104")
total_c105<-cleandata_c(c105)
colnames(total_c105)=c("國別","總人數105")
total_c106<-cleandata_c(c106)
colnames(total_c106)=c("國別","總人數106")
total_c<-
  Reduce(function(x, y) merge(x, y, by="國別",all=T),
         list(total_c103, total_c104, total_c105,total_c106))
result_c<-
  total_c%>%
  mutate(總人數=rowSums(total_c[,2:5],na.rm=T))%>%
  select(國別,總人數)%>%
  arrange(desc(總人數))
kable(head(result_c,10))
###1.2
cleandata_s<-function(x){
  y<-
    x%>%
    mutate(總人數=rowSums(x[,c(4:9,11,12)]))%>%
    select(學校名稱,總人數)
  return(y)
}
total_s103<-cleandata_s(s103)
colnames(total_s103)=c("學校名稱","總人數103")
total_s104<-cleandata_s(s104)
colnames(total_s104)=c("學校名稱","總人數104")
total_s105<-cleandata_s(s105)
colnames(total_s105)=c("學校名稱","總人數105")
total_s106<-cleandata_s(s106)
colnames(total_s106)=c("學校名稱","總人數106")
total_s<-
  Reduce(function(x, y) merge(x, y, by="學校名稱",all=T),
         list(total_s103, total_s104, total_s105,total_s106))
result_s<-
  total_s%>%
  mutate(總人數=rowSums(total_s[,2:5],na.rm=T))%>%
  select(學校名稱,總人數)%>%
  arrange(desc(總人數))
kable(head(result_s,10))
###2
sum_other<-sum(result_c[11:nrow(result_c),2])
result2<-
  result_c%>%
  rbind(c("其他",sum_other))
result2$總人數<-as.numeric(result2$總人數)
result2<-result2[c(1:10,nrow(result2)),]
orderID<-as.integer(rownames(result2))
result2$國別<-factor(as.integer(rownames(result2))
                   ,labels=result2$國別)
chart1<-
  ggplot(data=result2,
         aes(x=國別,y=總人數))+
  geom_bar(stat = "identity",fill = "cornflowerblue")+
  geom_text(aes(label=result2$總人數),color = "blue", vjust=-1)+
  theme_light()
chart1
###3
countryname<-read_csv("CountriesComparisionTable.csv")
result3<-merge(result_c,countryname,by.x="國別",by.y="Taiwan")
colnames(result3)<-c("國別","value","ISO3","region")
###中國大陸=中國大陸(5)+香港(91)+澳門(159)
grep("中國大陸",result3$國別)
grep("香港",result3$國別)
grep("澳門",result3$國別)
result3[5,2]=result3[5,2]+result3[91,2]+result3[159,2]
###索馬利亞民主共和國=索馬利亞民主共和國+索馬利蘭共和國
grep("索馬利亞民主共和國",result3$國別)
grep("索馬利蘭共和國",result3$國別)
result3[107,2]=result3[107,2]+result3[108,2]
result3<-
  result3%>%
  subset(region!="Unmatch")%>%
  subset(`國別`!="索馬利蘭共和國")%>%
  select(region,value)
chart2<-
  country_choropleth(result3)+
  scale_fill_brewer(palette=9,na.value="grey")+
  labs(title="各個國家來台灣唸書的學生人數面量圖")+
  theme(plot.title = element_text(hjust = 0.5))
chart2
###4
result4_1<-
  ods_result%>%
  subset(`學年度`>=103)%>%
  group_by(`對方學校(機構)國別(地區)`)%>%
  summarise(count=sum(小計))%>%
  arrange(desc(count))
kable(head(result4_1,10))
result4_2<-
  ods_result%>%
  subset(`學年度`>=103)%>%
  group_by(學校名稱)%>%
  summarise(count=sum(小計))%>%
  arrange(desc(count))
kable(head(result4_2,10))
###5
sum_other2<-sum(result4_1[11:nrow(result4_1),2])
result5<-
  result4_1%>%
  rbind(c("其他",sum_other2))
result5$count<-as.numeric(result5$count)
result5<-result5[c(1:10,nrow(result5)),]
orderID<-as.integer(rownames(result5))
result5$`對方學校(機構)國別(地區)`<-
  factor(as.integer(rownames(result5))
         ,labels=result5$`對方學校(機構)國別(地區)`)
chart3<-
  ggplot(data=result5,
         aes(x=`對方學校(機構)國別(地區)`,y=count))+
  geom_bar(stat = "identity",fill = "cornflowerblue")+
  geom_text(aes(label=result5$count),color = "blue", vjust=-1)+
  theme_light()
chart3
###6
countryname<-read_csv("CountriesComparisionTable.csv")
result4<-result4_1
colnames(result4)<-c("國名","count")
result6<-merge(result4,countryname,
               by.x="國名",by.y="Taiwan")

colnames(result6)<-c("國名","value","ISO3","region")
result6<-
  result6%>%
  subset(region!="Unmatch")
chart4<-
  country_choropleth(result6,num_colors=9)
chart4
###7
result7<-
  world%>%
  select(洲別:總人數)%>%
  arrange(desc(總人數))
kable(head(result7,10))
###8
countryname<-read_csv("CountriesComparisionTable.csv")
result7[13,2]<-"南韓"
result8<-merge(result7,countryname,
               by.x="國別",by.y="Taiwan")
colnames(result8)<-c("國別","洲別","value","ISO3","region")
chart5<-
  country_choropleth(result8,num_colors=7)
chart5
###9.1
###來台人數前10名
kable(head(result_c,10))
result9_1<-result_c
###離台人數前10名
kable(head(result4_1,10))
###9.2
result9_1<-head(result_c,10)
a<-result9_1$國別[1:2]
b<-result9_1$總人數[1:2]
c<-result9_1$國別[3:4]
d<-result9_1$總人數[3:4]
e<-result9_1$國別[5:6]
f<-result9_1$總人數[5:6]
g<-result9_1$國別[7:8]
h<-result9_1$總人數[7:8]
i<-result9_1$國別[9:10]
j<-result9_1$總人數[9:10]
result9<-data.frame(a,b,c,d,e,f,g,h,i,j)
colnames(result9)<-c("國別","總人數","國別","總人數","國別","總人數","國別","總人數","國別","總人數")
kable(result9)
# grep("中國大陸",result4_1$`對方學校(機構)國別(地區)`)#1
# grep("馬來西亞",result4_1$`對方學校(機構)國別(地區)`)#15
# grep("香港",result4_1$`對方學校(機構)國別(地區)`)#11
# grep("日本",result4_1$`對方學校(機構)國別(地區)`)#2
# grep("越南",result4_1$`對方學校(機構)國別(地區)`)#29,66
# grep("澳門",result4_1$`對方學校(機構)國別(地區)`)#38
# grep("印尼",result4_1$`對方學校(機構)國別(地區)`)#49
# grep("南韓",result4_1$`對方學校(機構)國別(地區)`)#4,14
# grep("美國",result4_1$`對方學校(機構)國別(地區)`)#3
# grep("泰國",result4_1$`對方學校(機構)國別(地區)`)#16,25
result9_2<-result4_1[c(1,15,11,2,29,
                       38,49,4,3,16),]
colnames(result9_2)<-c("國別","母國台籍生")
kable(result9_2)
result9_2$國別<-factor(as.integer(rownames(result9_2))
                     ,labels=result9_2$國別)
chart6<-
  ggplot(data=result9_2,
         aes(x=國別,y=母國台籍生))+
  geom_bar(stat = "identity",fill = "cornflowerblue")+
  geom_text(aes(label=result9_2$母國台籍生),color = "blue", vjust=-1)+
  theme_light()
chart6
