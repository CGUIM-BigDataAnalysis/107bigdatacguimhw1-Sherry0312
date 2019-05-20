# 1.	比較103年度和106年度大學畢業者的薪資資料
library(readr)
library(dplyr)
#讀入資料
X103education <- read_csv("C:/Users/Jacky/Downloads/Data/103education.csv")
X106education <- read_csv("C:/Users/Jacky/Downloads/Data/106education.csv")
X106education$大職業別<-gsub("_","、",X106education$大職業別)
newtable<-select(inner_join(X103education,X106education,by="大職業別"),"大職業別","年度.x",
                 "大學-薪資.x","年度.y","大學-薪資.y")
newtable$`大學-薪資.x`<-gsub("—",NA,newtable$`大學-薪資.x`)
newtable$`大學-薪資.y`<-gsub("—",NA,newtable$`大學-薪資.y`)
newtable$`大學-薪資.x`<-as.numeric(newtable$`大學-薪資.x`)
newtable$`大學-薪資.y`<-as.numeric(newtable$`大學-薪資.y`)
newtable<-mutate(newtable,"比值"=(newtable$`大學-薪資.y`/newtable$`大學-薪資.x`))%>%
  arrange(desc(比值))
# 1.1 
morethan1<-filter(newtable,比值>1)
knitr::kable(morethan1)
# 1.2
knitr::kable(head(newtable,10))
# 1.3
morethan105<-filter(newtable,比值>1.05)
table(purrr::map_chr(strsplit(morethan105$大職業別,"-"),tail,1))
# 1.4
table(purrr::map_chr(strsplit(morethan105$大職業別,"-"),head,1))

# 2.	男女同工不同酬一直是性別平等中很重要的問題
#資料匯入和處理
library(readr)
library(dplyr)
#匯入資料
X103education <- read_csv("C:/Users/Jacky/Downloads/Data/103education.csv")
X104education <- read_csv("C:/Users/Jacky/Downloads/Data/104education.csv")
X105education <- read_csv("C:/Users/Jacky/Downloads/Data/105education.csv")
X106education <- read_csv("C:/Users/Jacky/Downloads/Data/106education.csv")
#資料處理
#103
salary103<-select(X103education,"大職業別","大學-女/男")
salary103$`大學-女/男`<-gsub("—",NA,salary103$`大學-女/男`)
salary103$`大學-女/男`<-gsub("…",NA,salary103$`大學-女/男`)
salary103$`大學-女/男`<-as.numeric(salary103$`大學-女/男`)
#104
salary104<-select(X104education,"大職業別","大學-女/男")
salary104$`大學-女/男`<-gsub("—",NA,salary104$`大學-女/男`)
salary104$`大學-女/男`<-gsub("…",NA,salary104$`大學-女/男`)
salary104$`大學-女/男`<-as.numeric(salary104$`大學-女/男`)
#105
salary105<-select(X105education,"大職業別","大學-女/男")
salary105$`大學-女/男`<-gsub("—",NA,salary105$`大學-女/男`)
salary105$`大學-女/男`<-gsub("…",NA,salary105$`大學-女/男`)
salary105$`大學-女/男`<-as.numeric(salary105$`大學-女/男`)
#106
salary106<-select(X106education,"大職業別","大學-女/男")
salary106$`大學-女/男`<-gsub("—",NA,salary106$`大學-女/男`)
salary106$`大學-女/男`<-gsub("…",NA,salary106$`大學-女/男`)
salary106$`大學-女/男`<-as.numeric(salary106$`大學-女/男`)

#2.1
#103
knitr::kable(head(arrange(salary103,`大學-女/男`),10))
#104
knitr::kable(head(arrange(salary104,`大學-女/男`),10))
#105
knitr::kable(head(arrange(salary105,`大學-女/男`),10))
#106
knitr::kable(head(arrange(salary106,`大學-女/男`),10))
#2.2
#103
knitr::kable(head(arrange(salary103,desc(`大學-女/男`)),10))
#104
knitr::kable(head(arrange(salary104,desc(`大學-女/男`)),10))
#105
knitr::kable(head(arrange(salary105,desc(`大學-女/男`)),10))
#106
knitr::kable(head(arrange(salary106,desc(`大學-女/男`)),10))

# 3.	以106年度的資料來看，哪個職業別念研究所最划算(研究所學歷薪資與大學學歷薪資增加比例最多)? 
#匯入資料
X106education <- read_csv("C:/Users/Jacky/Downloads/Data/106education.csv")
salary106U<-select(X106education,"大職業別","大學-薪資","研究所及以上-薪資")
salary106U$`大學-薪資`<-gsub("—",NA,salary106U$`大學-薪資`)
salary106U$`研究所及以上-薪資`<-gsub("—",NA,salary106U$`研究所及以上-薪資`)
salary106U$`大學-薪資`<-as.numeric(salary106U$`大學-薪資`)
salary106U$`研究所及以上-薪資`<-as.numeric(salary106U$`研究所及以上-薪資`)
salary106U<-mutate(salary106U,"比值"=(salary106U$`研究所及以上-薪資`/salary106U$`大學-薪資`))%>%
  arrange(desc(比值))
#3.1
knitr::kable(head(salary106U,10))

# 4.	請列出自己有興趣的職業別 (至少一個至多五個)(5分)
#匯入資料
X106education <- read_csv("C:/Users/Jacky/Downloads/Data/106education.csv")
#取出大學薪資欄位與研究所薪資欄位
salary106Like<-select(X106education,"大職業別","大學-薪資","研究所及以上-薪資")
salary106Like5<-filter(salary106Like,大職業別=="資訊及通訊傳播業-專業人員"|
                         大職業別=="批發及零售業-專業人員"|大職業別=="服務業部門-專業人員"|
                         大職業別=="住宿及餐飲業-專業人員"|大職業別=="專業_科學及技術服務業-專業人員")
salary106Like5$`大學-薪資`<-as.numeric(salary106Like5$`大學-薪資`)
salary106Like5$`研究所及以上-薪資`<-as.numeric(salary106Like5$`研究所及以上-薪資`)
#4.1
knitr::kable(salary106Like5)
#4.2
salary106Like5difference<-mutate(salary106Like5,
                                 "薪資差"=(salary106Like5$`研究所及以上-薪資`-salary106Like5$`大學-薪資`))%>%
  arrange(desc(薪資差))
knitr::kable(salary106Like5difference)
