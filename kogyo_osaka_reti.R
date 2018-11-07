##########  ������i��񂩂�U�z�}���쐬
##########  �����ł�  ���{�̍H�ƒn  ��n(�y�n)���g�p

options(digits=10)
options(scipen=100)

  # setwd(" ")���͓K�X�C����v��
setwd("C:\\Users\\fu300\\Desktop\\torihiki")

library(tidyverse)
library(reti) # �C���X�g�[�����Ă��Ȃ��ꍇ�́Adevtools::install_github("rea-osaka/reti")

fnames <- dir(pattern="^\\d{2}_.*.csv") #�t�@�C�����̎擾
fnames
fnames <-fnames[27] #�����ł�27�Ԗڂ����{
 # �y�n�݂̂�ǂݍ���
osaka <- get_LOdata(fnames)

head(osaka)
str(osaka)

#---------- ������_�̂ݏC��
num1_4 <- function(x) {
  x <- sub("�P","1",x, fixed=T)
  x <- sub("�Q","2",x, fixed=T)
  x <- sub("�R","3",x, fixed=T)
  x <- sub("�S","4",x, fixed=T)
  return(x)
} 
osaka$������_ <- as.factor(osaka$������_)
levels(osaka$������_) <- num1_4(levels(osaka$������_))  # �S�p�����𔼊p�� 
osaka$jiten <- as.integer(substr(osaka$������_,3,4))+1988-.125+as.numeric(substr(osaka$������_,7,7))*.25

# ���s�E��s���܂Ƃ߂�����쐬
osaka$city <- as.character(osaka$�s��)
osaka$city[str_detect(osaka$�s��,"^���s")] <- "���s"
osaka$city[str_detect(osaka$�s��,"��s")] <- "��s"
osaka$city <- as.factor(osaka$city)

# =================  �H�ƒn�E�X�n�݂̂ŒP���E���_�̃O���t���쐬
osaka_kogyo <- osaka[osaka$�n��=="�H�ƒn", ]
osaka_kogyo$city %>% table() %>% sort() %>% tail()
 # �T���v������ʂ̑��s�A�����s�A��s�̃f�[�^�ɍi�荞��
osaka3_kogyo <- osaka_kogyo[str_detect(osaka_kogyo$city,"���s") | str_detect(osaka_kogyo$city,"��s"),]
osaka3_kogyo <- osaka3_kogyo %>% droplevels()
 # �R�s�̏o������ύX
osaka3_kogyo$city <- fct_relevel(osaka3_kogyo$city, c("���s", "��s", "�����s"))

# cut�֐����g���āA�n�ς��敪(right=TRUE���ƗႦ��100��200�̊ԂȂ�A100�ȏ�200�����AFALSE����100��200���ȉ��ƂȂ�)
# �T���v�����ɉ�����cut�֐���breaks=c(  )����K�X�C��, ������level�̐��▼�̂��C��
osaka3_kogyo$kibo <- cut(osaka3_kogyo$land_size, breaks=c(0,100,250,500,1000,5000,100000), right=FALSE)
levels(osaka3_kogyo$kibo) <- c("100����","100�ȏ�250����","250�ȏ�500����","500�ȏ�1000����","1000�ȏ�5000����","5000�ȏ�")
# �T���v�����̕\��
aggregate(data=osaka3_kogyo,kibo~city,table)

#---�O���t�`��
# �l�����ƒP���̎U�z�}
ggplot(data=osaka3_kogyo,aes(x=jiten, y=�y�n�P��)) + geom_point(aes(alpha=0.5))+geom_smooth()
# ���ɂ����̂ŁA�s�E�K�͕ʂɕ`��
osaka3_kogyo %>% ggplot(aes(jiten, �y�n�P�� )) + geom_hline(yintercept=c(100000,200000)) +
      geom_point(size=2, alpha=0.3) + geom_smooth(se=FALSE, size=1.2) + facet_grid(city~kibo)

 # �e�[�}�ύX
osaka3_kogyo %>% ggplot(aes(jiten, �y�n�P�� )) + geom_hline(yintercept=c(100000,200000)) + 
      geom_point(size=2, alpha=0.3) + geom_smooth() + facet_grid(city~kibo) + theme_bw()