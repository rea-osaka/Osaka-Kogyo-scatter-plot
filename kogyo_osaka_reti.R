##########  取引価格情報から散布図を作成
##########  ここでは  大阪府の工業地  宅地(土地)を使用

options(digits=10)
options(scipen=100)

  # setwd(" ")内は適宜修正を要す
setwd("C:\\Users\\fu300\\Desktop\\torihiki")

library(tidyverse)
library(reti) # インストールしていない場合は、devtools::install_github("rea-osaka/reti")

fnames <- dir(pattern="^\\d{2}_.*.csv") #ファイル名の取得
fnames
fnames <-fnames[27] #ここでは27番目が大阪府
 # 土地のみを読み込み
osaka <- get_LOdata(fnames)

head(osaka)
str(osaka)

#---------- 取引時点のみ修正
num1_4 <- function(x) {
  x <- sub("１","1",x, fixed=T)
  x <- sub("２","2",x, fixed=T)
  x <- sub("３","3",x, fixed=T)
  x <- sub("４","4",x, fixed=T)
  return(x)
} 
osaka$取引時点 <- as.factor(osaka$取引時点)
levels(osaka$取引時点) <- num1_4(levels(osaka$取引時点))  # 全角数字を半角に 
osaka$jiten <- as.integer(substr(osaka$取引時点,3,4))+1988-.125+as.numeric(substr(osaka$取引時点,7,7))*.25

# 大阪市・堺市をまとめた列を作成
osaka$city <- as.character(osaka$市名)
osaka$city[str_detect(osaka$市名,"^大阪市")] <- "大阪市"
osaka$city[str_detect(osaka$市名,"堺市")] <- "堺市"
osaka$city <- as.factor(osaka$city)

# =================  工業地・更地のみで単価・時点のグラフを作成
osaka_kogyo <- osaka[osaka$地域=="工業地", ]
osaka_kogyo$city %>% table() %>% sort() %>% tail()
 # サンプル数上位の大阪市、東大阪市、堺市のデータに絞り込み
osaka3_kogyo <- osaka_kogyo[str_detect(osaka_kogyo$city,"大阪市") | str_detect(osaka_kogyo$city,"堺市"),]
osaka3_kogyo <- osaka3_kogyo %>% droplevels()
 # ３市の出現順を変更
osaka3_kogyo$city <- fct_relevel(osaka3_kogyo$city, c("大阪市", "堺市", "東大阪市"))

# cut関数を使って、地積を区分(right=TRUEだと例えば100と200の間なら、100以上200未満、FALSEだと100超200ｍ以下となる)
# サンプル数に応じてcut関数のbreaks=c(  )内を適宜修正, 併せてlevelの数や名称も修正
osaka3_kogyo$kibo <- cut(osaka3_kogyo$land_size, breaks=c(0,100,250,500,1000,5000,100000), right=FALSE)
levels(osaka3_kogyo$kibo) <- c("100未満","100以上250未満","250以上500未満","500以上1000未満","1000以上5000未満","5000以上")
# サンプル数の表示
aggregate(data=osaka3_kogyo,kibo~city,table)

#---グラフ描画
# 四半期と単価の散布図
ggplot(data=osaka3_kogyo,aes(x=jiten, y=土地単価)) + geom_point(aes(alpha=0.5))+geom_smooth()
# 見にくいので、市・規模別に描画
osaka3_kogyo %>% ggplot(aes(jiten, 土地単価 )) + geom_hline(yintercept=c(100000,200000)) +
      geom_point(size=2, alpha=0.3) + geom_smooth(se=FALSE, size=1.2) + facet_grid(city~kibo)

 # テーマ変更
osaka3_kogyo %>% ggplot(aes(jiten, 土地単価 )) + geom_hline(yintercept=c(100000,200000)) + 
      geom_point(size=2, alpha=0.3) + geom_smooth() + facet_grid(city~kibo) + theme_bw()
