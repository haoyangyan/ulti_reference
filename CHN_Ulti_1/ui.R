Sys.setlocale(category = "LC_ALL", locale ='Chinese')
library(shiny)
library(ggplot2)
library(tidyverse)
his <- read_csv("his.csv")
int <- read_csv("int.csv")
his$年 <- as.integer(his$年)
his$月 <- as.integer(his$月)
his$队伍数 <- as.integer(his$队伍数)
his$Year <- as.integer(his$Year)
his$Month <- as.integer(his$Month)
his$NTeams <- as.integer(his$NTeams)
his <- his[order(his$年, his$月, na.last = TRUE, decreasing = TRUE),]

ncity <- his %>%
  unite('cityce', 城市, City) %>%
  group_by(cityce) %>%
  summarise(n = n())
ncity <- ncity[order(ncity$n, decreasing = TRUE),]$cityce
divi <- c('混合/Mixed','公开/Open',"女子/Women\'s",'大学 混合/College Mixed','高中 混合/High School Mixed','联赛 混合/League Mixed')

fluidPage(
  titlePanel('中国极限飞盘数据库  Chinese Ultimate Reference'),
  titlePanel('国内赛事 Domestic Tournament'),
  fluidRow(
    column(4,
           selectInput("lang", "语言/language", choices = c('中文','English'), selected = '中文', multiple = FALSE)),
    column(4,
           selectInput("a", "高水平赛事/high level tournament", choices = c('全部/all', '是/Yes'), selected = '全部', multiple = FALSE))
  ),
  fluidRow(
    column(4,
           selectInput("d", "组别/division", choices = c('全部/all', divi), selected = '全部', multiple = FALSE)),
    column(4,
           selectInput("b", "年份/year", choices = c('全部/all', unique(his$年)), selected = '全部', multiple = FALSE)),
    column(4,
           selectInput("c", "城市/city", choices = c('全部/all', ncity), selected = '全部', multiple = FALSE))
  ),
  DT::dataTableOutput("table"),
  textOutput("f2"),
  textOutput("f3"),
  textOutput("f4"),
  textOutput("f5"),
  textOutput("f6"),
  textOutput("f7"),
  titlePanel('国际赛事 International Tournament'),
  fluidRow(
    column(4,
           selectInput("e", "组别/Division", choices = c('Club','National'), selected = c('Club','National'), multiple = TRUE)),
    column(4,
           selectInput("f", "级别/Level", choices = unique(int$Level), selected = unique(int$Level), multiple = TRUE))
  ),
  DT::dataTableOutput("table2"),
  textOutput("f8")
)
