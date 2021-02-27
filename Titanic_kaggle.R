# Data input, assesment : 데이터 불러들이기, 확인하는 과정 
library(readr)           # Data input with readr::read_csv()
library(descr)           # descr::CrossTable() - 범주별 빈도수, 비율 수치로 확인

# Visualization
library(VIM)             # Missing values assesment used by VIM::aggr()
library(ggplot2)         # Used in almost visualization 
library(RColorBrewer)    # plot의 color 설정 
library(scales)          # plot setting - x, y 축 설정

# Feature engineering, Data Pre-processing
library(tidyverse)     # dplyr, ggplot2, purrr, etc... 
library(dplyr)           # Feature Engineering & Data Pre-processing 
library(purrr)           # Check missing values 
library(tidyr)           # tidyr::gather() 

# Model generation  
library(randomForest)    # For Random Forest Modeling

# Model validation : 원래는 하는게 맞지만 이번 과정에서는 생략
# library(caret)           # caret::confusionMatrix() 
# library(ROCR)            # Plotting ROC Curve

#################################################################

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


## 2.2 Raw data import : 원본 데이터 불러오기
# bind_rows()를 사용하면 dimension이 달라도 병합 가능함(NULL로 처리)
train <- readr::read_csv('train_titanic.csv')
test <- readr::read_csv('test_titanic.csv')
full <- dplyr::bind_rows(train, test)


## 2.4 Change the variables type : 변수 속성 변환
full <- full %>% 
  dplyr::mutate(Survived = factor(Survived),
                Pclass   = factor(Pclass, ordered = TRUE),
                Name     = factor(Name),
                Sex      = factor(Sex),
                Ticket   = factor(Ticket),
                Cabin    = factor(Cabin),
                Embarked = factor(Embarked))

### 3. 탐색적 데이터 분석 (EDA : Exploratory data analysis)

## 3.1 수치값을 활용한 data 확인
str(full)
summary(full)

## 3.2 Missing values
## 3.2.1 VIM packages
VIM::aggr(full, 
          prop      = FALSE,
          combined  = TRUE,
          numbers   = TRUE,
          sortVars  = TRUE,
          sortCombs = TRUE)

## 3.2.2 tidyverse packages
full %>% 
  dplyr::summarize_all(funs(sum(is.na(.))/n()))

missing_values <- full %>% 
  dplyr::summarize_all(funs(sum(is.na(.))/n()))

missing_values <- tidyr::gather(missing_values,
                                key = "feature", value = "missing_pct")

missing_values %>% 
  ggplot(aes(x = reorder(feature, missing_pct),
             y = missing_pct)) +
  geom_bar(stat = "identity", fill = "red") +
  ggtitle("Rate of missing values in each features") +
  theme(plot.title = element_text(face = "bold",
                                  hjust = .5,
                                  size = 15,
                                  color = "darkblue")) +
  labs(x = "Feature names",
       y = "Rate") +
  coord_flip()

# Using purrr package, pick the variable which has NA variable.
miss_pct <- purrr::map_dbl(full, function(x){round((sum(is.na(x))/length(x))* 100, 1)})

miss_pct <- miss_pct[miss_pct > 0]

data.frame(miss = miss_pct,
           var = names(miss_pct),
           row.names = NULL) %>% 
  ggplot(aes(x = reorder(var, miss),
             y = miss)) +
  geom_bar(stat = "identity", fill = "red") +
  ggtitle("Rate of missing values") +
  labs(x = 'Feature names',
       y = 'Rate of missing values') +
  theme(plot.title = element_text(face = "bold",
                                  hjust = .5,
                                  size = 15,
                                  color = "darkblue")) +
  coord_flip()
