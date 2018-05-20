pkgs <- c("readxl","dplyr","tidyr","stringr", 
          "lubridate","caret","data.table","reshape2")
sapply(pkgs,require,character.only=TRUE)

dir <- c('D:/승훈/Data/공모전/빅콘테스트/기러기')
setwd(dir)
dir_files <- list.files(dir)
datas <- lapply(dir_files,read.csv,header=T, stringsAsFactors=F)

movie <- rbindlist(datas)

glimpse(movie)
movie <- rename(movie,rank=순위,movie_name=영화명,open_day=개봉일,week_salary=매출액,week_salary_percent=매출액...,
             sum_salary=누적매출액,week_people=관객수,sum_people=누적관객수,screen=스크린수,play_freq=상영횟수,
             top_country=대표국적,country=국적,maker=제작사,supplier=배급사,cover_age=등급,genre=장르,
             director=감독,actor=배우)
 
# 예측에 쓸데없는 변수 cut
movie <- movie[,-c(4:6)]

# 주차 인덱스
date <- sapply(datas,nrow)
date <- rep(1:131,date)
movie$week <- date

# 날짜 구간 처리 - 재개봉인가?
movie$open_day <- as.Date(movie$open_day)
first_day<-as.Date("2015-01-01")
laast_day<-as.Date("2017-07-05")
movie$re_open <- ifelse(movie$open_day >= first_day &
                        movie$open_day <= laast_day,0,1)
remove(first_day,laast_day)

# 1주차, 2주차의 영화 예측이니까
# 1,2주차의 데이터만 남기면
movie <- movie %>% group_by(movie_name) %>%
  mutate(ord = row_number()) %>% filter(ord < 3)

# 계절은?
a <- month(movie$open_day)
movie$season <- ifelse(a %in% c(12,1,2),'winter',
                ifelse(a %in% c(3,4,5),'spring',
                ifelse(a %in% c(6,7,8),'summer',
                ifelse(a %in% c(9,10,11),'fall','no_date'))))


