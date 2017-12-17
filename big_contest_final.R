pkgs <- c("readxl","dplyr","tidyr","stringr", 
          "lubridate","caret","data.table","reshape2")
sapply(pkgs,require,character.only=TRUE)

dir <- c('D:/바탕_정리/기러기')
setwd(dir)
dir_files <- list.files(dir)
datas <- lapply(dir_files,read.csv,header=T, stringsAsFactors=F)

d1 <- rbindlist(datas)

# 합쳐진 파일이 몇주차 데이터인지 인덱스.
aa <- sapply(datas,nrow)
ordd <- rep(seq(1,131,1), aa)
d2 <- cbind(d1,ordd)

# 주말 외 공휴일의 유무 변수.
a <- c(1,7,8,9,18,21,23,33,39,40,41,52,53,58,61,67,71,72,75,85,90,92,93,104,105,109,113,122,123,123,127)
d2$rest <- ifelse(d2$ordd %in% a,1,0)

# 활용할 수 없는 데이터 제거.
# d2의 매출액 퍼센트가 점유율이 되는 행이 5741,21566,28110
table(d2$순위=="합계") # 이외에 각 데이터마다 합계행을 세줌. 131주차니까 131개.
d3<-d2[-c(5741,21566,28110) , ] # 먼저 매출액 퍼센트가 점유율이 되는 행 3개 제거(오입력으로 판단)
d4 <- d3[!(d3$순위 == "합계")] # 합계행을 빼줌.

# 변수들의 이름을 영문으로 변경
d5 <- rename(d4,rank=순위,movie_name=영화명,open_day=개봉일,week_salary=매출액,week_salary_percent=매출액...,
           sum_salary=누적매출액,week_people=관객수,sum_people=누적관객수,screen=스크린수,play_freq=상영횟수,
           top_country=대표국적,country=국적,maker=제작사,supplier=배급사,cover_age=등급,genre=장르,
           director=감독,actor=배우)

d5 <- data.frame(d5)
# 숫자형에 섞여있는 문자 정리
# 4,6,7열에서 제거해야할 특수문자가 동일함.

d5[,5] <- str_replace(d5[,5],"%","")
d5[,5] <- as.numeric(d5[,5])
table(is.na(d5[,5])) # False 30278

for(i in c(4,6,7,8,9,10)){
  d5[,i] <- str_replace_all(d5[,i],",","")
  d5[,i] <- str_replace_all(d5[,i],"[)]","")
  d5[,i] <- str_replace_all(d5[,i],"[(]","")
  d5[,i] <- as.numeric(d5[,i])  
  }
table(is.na(d5[,c(4,6,7,8,9,10)])) # False 181668

movie <- d5

# 날짜 정리 테이블
open_t<-data.frame(table(movie$open_day))

# movie의 open_day가 빈 행을 제거.
movie2<-movie[!(movie$open_day==open_t$Var1[1]),]
open_t<-data.frame(table(movie2$open_day))

# 텍스트였던 날짜를 Date화 해주고
# 구간 외의 개봉일을 갖는 영화를 filter
movie2$open_day<-as.Date(movie2$open_day)
first_day<-as.Date("2015-01-01")
laast_day<-as.Date("2017-07-05")
movie3<-movie2 %>%
  filter(movie2$open_day<=laast_day &
           movie2$open_day>=first_day)


# 날짜 범위 확인 
range(movie3$open_day) # "2015-01-01" "2017-07-05"

# 개채수도 확인
sum(open_t$Freq[1008:1591]) # 16497

# ord를 min이랑 min+1인 값만 남기면 되지 않나?
movie3<-movie3[order(movie3$movie_name),]

movie4<-movie3 %>% group_by(movie_name) %>%
  mutate(ord_rank=row_number())
ord_rank_t<-data.frame(table(movie4$ord_rank))
sum(ord_rank_t$Freq[c(1,2)]) # 5340

movie5<-movie4 %>% filter(ord_rank<3) 

# obs가 movie4의 ord_rank 1,2의 freq를 sum 한 5340과 동일
country_t<-data.frame(table(movie5$top_country))
country_t<-country_t[order(-country_t$Freq),]

# 가장 많은 빈도수가 한국,미국,일본
if_kor<-vector(length=5301)
countr <- c("한국","미국","일본")
if_kor <- ifelse(movie5$top_country %in% countr,1,0)

# count수는 3986으로 동일
table(if_kor)
sum(country_t$Freq[1:3])
movie5<-cbind(movie5,if_kor=if_kor)
movie5<-movie5[,-20]

# age 정리
age_t<-data.frame(table(movie5$cover_age))
movie5$cover_age<-gsub("15세이상관람가,전체관람가","15세이상관람가",movie5$cover_age)
movie5$cover_age<-factor(movie5$cover_age, 
                         levels=c("전체관람가","12세이상관람가","15세이상관람가",
                                  "청소년관람불가"))

# 장르
attach(movie5)
movie6<-separate(movie5,genre,c("gen1","gen2"),sep = ",")
gen_t<-data.frame(table(movie6$gen1))

# 장르가 비어 있는 천하제일권(movie6[4360,])을 기타로
movie6$gen1[4360]<-"기타"

# top_5 장르
# 순위는 다르지만 액션,sf,드라마,로코,범죄
# 로코는 따로 있는 장르가 아니라서 멜로/로멘스,코미디가 같이 있는것으로.
temp<-data.frame(table(movie5$genre))
roco1<-which(movie5$genre==temp$Var1[115])
roco2<-which(movie5$genre==temp$Var1[116])
roco3<-which(movie5$genre==temp$Var1[117])
roco<-c(roco1,roco2,roco3)

# 멜로/로멘스, 코미디
if_roco<-vector(length=5301)
if_roco[roco]<-1

# 코미디, 멜로/로멘스
coro<-c(which(movie5$genre==temp$Var1[335]),which(movie5$genre==temp$Var1[339]),
        which(movie5$genre==temp$Var1[340]),which(movie5$genre==temp$Var1[344]))
if_roco[coro]<-1

table(if_roco)
# 0:5224, 1:116

# roco:64, coro:52 더하면 116이므로 일치함.

# 로멘틱 코메디를 제외한 4개의 장르더미.
genre_top4<-vector(length=5301)
genre_top4[movie6$gen1=="액션"]<-1
genre_top4[movie6$gen1=="SF"]<-1
genre_top4[movie6$gen1=="드라마"]<-1
genre_top4[movie6$gen1=="범죄"]<-1

# 이제 합치기 총 1975개
genre_top5<-genre_top4+if_roco
movie6<-cbind(movie6,gen_top5=genre_top5)

# 확인용
ma_t_save<-data.frame(table(movie6$maker))

# 특수문자 ㈜
# maker, supplier
# 삭제해야할 특수문자
# maker - (사),(유),(재),(주),(합)
# supplier - (사),(유),(재),(주)
movie7<-movie6
movie7$maker<-gsub("[(]사[)]","",movie7$maker,perl=TRUE)
movie7$maker<-gsub("[(]유[)]","",movie7$maker,perl=TRUE)
movie7$maker<-gsub("[(]재[)]","",movie7$maker,perl=TRUE)
movie7$maker<-gsub("[(]주[)]","",movie7$maker,perl=TRUE)
movie7$maker<-gsub("[(]합[)]","",movie7$maker,perl=TRUE)
movie7$maker<-gsub("[㈜]","",movie7$maker,perl=TRUE)

movie7$supplier<-gsub("[(]사[)]","",movie7$supplier,perl=TRUE)
movie7$supplier<-gsub("[(]유[)]","",movie7$supplier,perl=TRUE)
movie7$supplier<-gsub("[(]재[)]","",movie7$supplier,perl=TRUE)
movie7$supplier<-gsub("[(]주[)]","",movie7$supplier,perl=TRUE)
movie7$supplier<-gsub("[㈜]","",movie7$supplier,perl=TRUE)

# 확인
ma_t<-data.frame(table(movie7$maker))
sp_t<-data.frame(table(movie7$supplier))

movie7<-separate(movie7,maker,c("ma1","ma2","ma3"),sep=",")
movie7<-separate(movie7,supplier,c("sup1","sup2","sup3"),sep=",")

# NA랑 빈 값에 empty 입력
movie7$ma1[movie7$ma1==""]<-"empty"
movie7$sup1[movie7$sup1==""]<-"empty"

# 첫번째 행의 빈 값들도 전부 empty로.
movie7$ma2[is.na(movie7$ma2)]<-"empty"
movie7$ma3[is.na(movie7$ma3)]<-"empty"
movie7$sup2[is.na(movie7$sup2)]<-"empty"
movie7$sup3[is.na(movie7$sup3)]<-"empty"

# 잘못 입력했던 변수명 변경
movie7<-rename(movie7,country_top3=if_kor)

# 휴일
# -1 여부
rest<-c(1,7,8,9,18,21,23,33,39,40,41,52,53,58,61,67,71,72,75,85,89,90,92,93,104,
        105,109,113,122,123,127)
rrest<-vector(length=5301)
for(i in 1:length(rest)){
  rrest[movie7$ordd==rest[i]]<-1
}

# 확인
table(rrest)
t<-data.frame(table(movie7$ordd))
sum(t$Freq[rest]) # 1164로 동일.

# 공휴일 수
rest2<-vector(length=5301) ####
r1<-c(1,7,9,18,21,23,33,40,41,52,53,61,67,72,75,85,89,92,93,104,105,113,122,127)
r2<-c(8,71,90,123)
r3<-c(39,58,109)

for(i in 1:length(r1)){
  rest2[movie7$ordd==r1[i]]<-1
}

for(i in 1:length(r2)){
  rest2[movie7$ordd==r2[i]]<-2
}

for(i in 1:length(r3)){
  rest2[movie7$ordd==r3[i]]<-4
}
table(rest2) # 1,2,4의 freq합이 1164
movie7<-cbind(movie7,rest_dummy=rrest,rest_count=rest2)

# director,actor
movie_dir_act<-movie7[,c(2,22,23)]
movie_dir_act<-separate(movie_dir_act,director,c("dir1","dir2","dir3","dir4","dir5"),sep=",")
movie_dir_act<-separate(movie_dir_act,actor,c("ac1","ac2","ac3","ac4","ac5"),sep=",")
movie_dir_act[movie_dir_act==""]<-"empty"
movie_dir_act[is.na(movie_dir_act)]<-"empty"
movie_dir_act<-movie_dir_act[,-c(3,4,5,6)] # 감독을 첫번째만 남기고 날림.

# movie8
movie8<-cbind(movie_dir_act,movie7[,c(2,3,7,8,9,10,11,13,14,15,16,17,18,19,20,24,25,26,27,28,29)])
table(movie8$movie_name==movie8$movie_name1) # true가 5340
movie8<-movie8[,-8]

# movie9
# 영화명 3645개
movie9<-data.frame(unique(movie8$movie_name))

# 관객수 - 1 sum_people의 최대값
sum_max<-data.frame(tapply(movie8$sum_people,movie8$movie_name,max))
sum_max<-cbind(sum_max,rownames(sum_max))
table(movie9$unique.movie8.movie_name.==sum_max$`rownames(sum_max)`) # 전부 true 3645
movie9<-cbind(movie9,V_sum=sum_max$tapply.movie8.sum_people..movie8.movie_name..max.)

# 스크린
scr<-data.frame(tapply(movie8$screen,movie8$movie_name,sum))
scr<-cbind(scr,rownames(scr))
table(movie9$unique.movie8.movie_name.==scr$`rownames(scr)`) # 전부 true 3645
movie9<-cbind(movie9,screen=scr$tapply.movie8.screen..movie8.movie_name..sum.)

# 상영횟수
pla<-data.frame(tapply(movie8$play_freq,movie8$movie_name,sum))
pla<-cbind(pla,rownames(pla))
table(movie9$unique.movie8.movie_name.==pla$tapply.movie8.play_freq..movie8.movie_name..sum.) # 전부 true 3645
movie9<-cbind(movie9,play_freq=pla$tapply.movie8.play_freq..movie8.movie_name..sum.)

# movie_name만 이름 정리
movie9<-rename(movie9,movie_name=unique.movie8.movie_name.)

# 1주차인가 2주차인가 ord_rank
ord_co<-data.frame(tapply(movie8$ord_rank,movie8$movie_name,sum))
ord_co<-cbind(ord_co,rownames(ord_co))
table(movie9$movie_name==ord_co$`rownames(ord_co)`) # true 3645
table(ord_co$tapply.movie8.ord_rank..movie8.movie_name..sum.)
ord_co$tapply.movie8.ord_rank..movie8.movie_name..sum.<-ifelse(ord_co$tapply.movie8.ord_rank..movie8.movie_name..sum.==3,2,1)
table(ord_co$tapply.movie8.ord_rank..movie8.movie_name..sum.)
movie9<-cbind(movie9,play_week=ord_co$tapply.movie8.ord_rank..movie8.movie_name..sum.)

# country_top3
cout_top3<-data.frame(tapply(movie8$country_top3,movie8$movie_name,max))
cout_top3<-cbind(cout_top3,rownames(cout_top3))
table(movie9$movie_name==cout_top3$`rownames(cout_top3)`) # true 3645
movie9<-cbind(movie9,country_top3=cout_top3$tapply.movie8.country_top3..movie8.movie_name..max.)

# gen_top5
gen_5<-data.frame(tapply(movie8$gen_top5,movie8$movie_name,max))
gen_5<-cbind(gen_5,rownames(gen_5))
table(movie9$movie_name==gen_5$`rownames(gen_5)`) # true 3645
movie9<-cbind(movie9,gen_top5=gen_5$tapply.movie8.gen_top5..movie8.movie_name..max.)

# rest_dummy
rest_dum<-data.frame(tapply(movie8$rest_dummy,movie8$movie_name,max))
rest_dum<-cbind(rest_dum,rownames(rest_dum))
table(movie9$movie_name==rest_dum$`rownames(rest_dum)`) # true 3645
movie9<-cbind(movie9,rest_dummy=rest_dum$tapply.movie8.rest_dummy..movie8.movie_name..max.)

# rest_count
rest_co<-data.frame(tapply(movie8$rest_count,movie8$movie_name,max))
rest_co<-cbind(rest_co,rownames(rest_co))
table(movie9$movie_name==rest_co$`rownames(rest_co)`) # true 3645
movie9<-cbind(movie9,rest_count=rest_co$tapply.movie8.rest_count..movie8.movie_name..max.)

# write.csv(movie9,'C:/Users/simon-pc/Desktop/big/movie9.csv')
movie9<-read.csv(file='C:/Users/simon-pc/Desktop/big/movie9.csv',header = T,stringsAsFactors = F)
movie9<-movie9[,-1]

# 마지막으로 텍스트
a<-read.csv(file='C:/Users/simon-pc/Desktop/big/text_index.csv',header = T)
movie_text<-movie8[a$x,c(1,2,3,4,5,6,7,12,13,14,15,16,17,18,19,20)]
table(movie9$movie_name==movie_text$movie_name) # 전부 true 3645

movie9<-data.frame(movie9)
movie_text<-data.frame(movie_text)
movie_final<-data.frame(cbind(movie9,movie_text))
table(movie_final$movie_name==movie_final$movie_name.1) # true 3645
movie_final<-movie_final[,-10]

movie_final_save<-movie_final
movie_final<-movie_final[,-8]
movie_final_test1<-movie_final[,c(2,3,4,5,6,7,8,15,16,19,22,23)]

# supplier_top10
sup10_list<-c(128,102,184,183,182,186,28,55,196,68,4)
sup1_t<-data.frame(table(movie_final$sup1))
bb<-vector(length=3645)
bb[movie_final$sup1==sup1_t$Var1[sup10_list]]<-1
movie_final_test1<-cbind(movie_final_test1,sup_top10=bb)
movie_final_test2<-movie_final_test1[,-c(8,9,10,12)]
movie_final_test3<-movie_final_test1[,c(1,2,3,4,5,6,7,12,13)]

# cover_age가 활용되지 않음.
# 펙터문제로 생각되어 숫자로 변환.
age_num<-as.numeric(movie_final_test3$cover_age)
movie_final_test3$cover_age<-age_num

# 비교를 위해서
age_num<-as.numeric(movie_final_test3$cover_age)
age_num1<-movie_final_test3$cover_age

table(age_num) 
# age_num
# 1    2    3    4 
# 321  470  811 2043 

table(age_num1)
# age_num1
# 전체관람가 12세이상관람가 15세이상관람가 청소년관람불가 
#   321            470            811           2043 

movie_final_test3<-rename(movie_final_test3,age_grade=cover_age)
movie_final_test3$age_grade<-as.factor(movie_final_test3$age_grade)
levels(movie_final_test3$age_grade)<-c("전체관람가","12세이상관람가",
                                       "15세이상관람가","청소년관람불가")

# 범주형이 아닌 수치형 데이터 screen, play_freq에 대한 다중공선성 검정
mv_cort<-movie_final_test3[,c(1,2,3)]
cor(mv_cort) # screen과 play_freq 사이의 상관관계가 상당히 높음.

movie_final_test3<-movie_final_test3[,-3]
str(movie_final_test3)

# 비교 시각화
V_screen <- ggplot(data=mv_cort,aes(screen,V_sum)) +
  geom_point(alpha=0.5) +
  geom_smooth(se = F)

V_pfreeq <- ggplot(data=mv_cort,aes(play_freq,V_sum)) +
  geom_point(alpha=0.5) +
  geom_smooth(se = F)

library(gridExtra)
grid.arrange(V_screen, V_pfreeq, ncol = 2)

# 살인자의 기억법 1주    
killer<-data.frame(
  screen = 6894,
  play_freq = 12856,
  play_week = 1,
  country_top3 = 1,
  gen_top5 = 1,
  rest_count = 0,
  age_grade = "15세이상관람가",
  sup_top10 = 1)

# 살인자의 기억법 2주
killer2<-data.frame(
  screen = 33922,
  play_freq = 61672,
  play_week = 2,
  country_top3 = 1,
  gen_top5 = 1,
  rest_count = 0,
  age_grade = "15세이상관람가",
  sup_top10 = 1)

set.seed(1234)
ind   <- sample(2, nrow(movie_final_test3), replace = T, prob = c(0.7, 0.3))
train <- movie_final_test3[ind == 1, ]
test  <- movie_final_test3[ind == 2, ]

library(h2o)
h2o.init(max_mem_size = "7G")
my.y <- "V_sum"
my.x <- c("screen", 
          "play_week",  "country_top3", "gen_top5", 
          "rest_count","sup_top10","age_grade")

## glm
glm <- h2o.glm(x = my.x,
               y = my.y,
               training_frame = as.h2o(train),
               nfolds = 10,
               seed = 1234)

## gbm
gbm <- h2o.gbm(x = my.x,
               y = my.y,
               training_frame = as.h2o(train),
               nfolds = 10,
               ntrees = 500,
               seed = 1234)

## drf
drf <- h2o.randomForest(x = my.x,
                        y = my.y,
                        training_frame = as.h2o(train),
                        nfolds = 10,
                        ntrees = 500,
                        seed = 1234)

## deep learning
hdl <- h2o.deeplearning(x = my.x,
                        y = my.y,
                        training_frame = as.h2o(train),
                        nfolds = 10,
                        seed = 1234,
                        hidden = c(50, 50, 50),
                        epochs = 10)

## performance evaluation
train_rmse_glm_f  <- h2o.rmse(glm, train = TRUE)
valid_rmse_glm_f  <- h2o.rmse(glm, xval = TRUE)
test_perf_glm_f   <- h2o.performance(glm, as.h2o(test))
test_rmse_glm_f   <- h2o.rmse(test_perf_glm_f)

train_rmse_gbm_f  <- h2o.rmse(gbm, train = TRUE)
valid_rmse_gbm_f  <- h2o.rmse(gbm, xval = TRUE)
test_perf_gbm_f   <- h2o.performance(gbm, as.h2o(test))
test_rmse_gbm_f   <- h2o.rmse(test_perf_gbm_f)

train_rmse_drf_f  <- h2o.rmse(drf, train = TRUE)
valid_rmse_drf_f  <- h2o.rmse(drf, xval = TRUE)
test_perf_drf_f   <- h2o.performance(drf, as.h2o(test))
test_rmse_drf_f   <- h2o.rmse(test_perf_drf_f) 

train_rmse_hdl_f  <- h2o.rmse(hdl, train = TRUE)
valid_rmse_hdl_f  <- h2o.rmse(hdl, xval = TRUE)
test_perf_hdl_f   <- h2o.performance(hdl, as.h2o(test))
test_rmse_hdl_f   <- h2o.rmse(test_perf_hdl_f) 

df_f <- data.frame(
  model = c("glm", "gbm", "drf", "hdl"),
  train_rmse =  c(train_rmse_glm_f,
                  train_rmse_gbm_f,
                  train_rmse_drf_f,
                  train_rmse_hdl_f),
  valid_rmse =  c(valid_rmse_glm_f,
                  valid_rmse_gbm_f,
                  valid_rmse_drf_f,
                  valid_rmse_hdl_f),
  test_rmse  =  c(test_rmse_glm_f,
                  test_rmse_gbm_f,
                  test_rmse_drf_f,
                  test_rmse_hdl_f) ) 

df_f

# 살인자의 기억법 1주 - killer
pred  <- h2o.predict(drf, as.h2o(killer))
pred

pred2 <- h2o.predict(hdl, as.h2o(killer))
pred2

pred3 <- h2o.predict(gbm, as.h2o(killer))
pred3

pred4 <- h2o.predict(glm, as.h2o(killer))
pred4

# 살인자의 기억법 2주 - killer2
pred_1  <- h2o.predict(drf, as.h2o(killer2))
pred_1

pred2_1 <- h2o.predict(hdl, as.h2o(killer2))
pred2_1

pred3_1 <- h2o.predict(gbm, as.h2o(killer2))
pred3_1

pred4_1 <- h2o.predict(glm, as.h2o(killer2))
pred4_1

# 아이캔스피크
speak<-data.frame(
  screen = 7337,
  play_week = 1,
  country_top3 = 1,
  gen_top5 = 0,
  rest_count = 0,
  age_grade = "12세이상관람가",
  sup_top10 = 1)

sp_pred_gbm<-h2o.predict(gbm,as.h2o(speak))
sp_pred_gbm  

sp_pred_hdl<-h2o.predict(hdl,as.h2o(speak))
sp_pred_hdl

sp_pred_drf<-h2o.predict(drf,as.h2o(speak))
sp_pred_drf

# 예측영화
king<-data.frame(
  screen = 6202,
  play_week = 2,
  country_top3 = 1,
  gen_top5 = 1,
  rest_count = 6,
  age_grade = "청소년관람불가",
  sup_top10 = 1)

king_pred_hdl<-h2o.predict(hdl,as.h2o(king))
king_pred_hdl

nut<-data.frame(
  screen = 1040,
  play_week = 1,
  country_top3 = 1,
  gen_top5 = 0,
  rest_count = 4,
  age_grade = "전체관람가",
  sup_top10 = 0)

nut_pred_hdl<-h2o.predict(hdl,as.h2o(nut))
nut_pred_hdl

castle<-data.frame(
  screen = 11270,
  play_week = 1,
  country_top3 = 1,
  gen_top5 = 0,
  rest_count = 4,
  age_grade = '15세이상관람가',
  sup_top10 = 1
)

castle_pred_hdl<-h2o.predict(hdl,as.h2o(castle))
castle_pred_hdl

# 제출후 아쉬움.
# 시간이 있었다면 배우변수를 만들 수 있었을텐데 초반 전처리에서 헤메 시간이 부족했음.
# 1주차, 2주차 관객수다보니 첫날 관객수나 첫날 매출을 활용하면 어땠을까.
# 계절변수, 영화 상영기간에 사람들의 많은 관심을 받는 영화가 개봉하지는 않는가?
# 극장 점유율도 활용해 볼 수 있었을듯.

