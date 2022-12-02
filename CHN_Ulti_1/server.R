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

function(input, output) {
  output$table <- DT::renderDataTable(DT::datatable({
    if (input$a == '全部/all'){
      s1 <- his
    }else{
      s1 <- his %>%
        filter(高水平 %in% strsplit(input$a, '/')[[1]][1])
    }
    if (input$b == '全部/all'){
      s2 <- s1
    }else{
      s2 <- s1 %>%
        filter(年 %in% input$b)
    }
    if (input$c == '全部/all'){
      s3 <- s2
    }else{
      s3 <- s2 %>%
        filter(城市 %in% strsplit(input$c, '_')[[1]][1])
    }
    if (input$d == '全部/all'){
      s4 <- s3
    }else{
      s4 <- s3 %>%
        filter(组别 %in% strsplit(input$d, '/')[[1]][1])
    }
    if (input$lang == '中文'){
      s4[,1:12]
    }else{
      s4[,13:24]
    }
  }))
  output$f2 <- renderText('（1）本数据库收录中国大陆的及有中国大陆队伍参加的七人制草地极限飞盘赛事信息  This Database collects all the information of 7 on 7 ultimate tournament in mainland  of China or with team from mainland of China')
  output$f3 <- renderText('（2）本数据库暂不收录分组赛信息  Hat or Jumble tournaments are not included now')
  output$f4 <- renderText('（3）高水平赛事的判定标准相当主观，目前将有两支及以上全国顶尖队伍参加的赛事定位为高水平赛事  The classification about Nationwide tournament is subjective. I consider tournaments with at least 2 top teams as High Level')
  output$f5 <- renderText('（4）有的队伍会改名或分为两支队伍，这里均采用统一名字  Sometimes a team changed its name or split into two teams. I use the consistent team name for them')
  output$f6 <- renderText('（5）大学队伍在大学组比赛采用学校名字，参加非大学组比赛采用队伍名字  For college team, I use the university name in college division while the team name in non-college division')
  output$f7 <- renderText('（6）如果您对本数据库有任何补充或建议，请在微信公众号 浩洋的随想录 留言，会在审查后上传更新，完善的数据库需要每一位盘友的帮助！  If you have any supplement or advice, please send message to wechat official account, 浩洋的随想录. I will check and update it. A good database need help from EVERYONE!')
  
  output$table2 <- DT::renderDataTable(DT::datatable({
    if (length(input$e) == 2){
      ss1 <- int
    }else{
    if (input$e == c('Club')){
      ss1 <- int %>%
        filter(Division %in% c('Open Club','Mixed Club','Women Club'))
    }
    if (input$e == c('National')){
      ss1 <- int %>%
        filter(!Division %in% c('Open Club','Mixed Club','Women Club'))
    }
    }
    ss2 <- ss1 %>%
      filter(Level %in% input$f)
    ss2
  }))
  output$f8 <- renderText('本数据库收录中国大陆所有队伍参加国际飞盘赛事的信息  This Database collects all the ultimate team in international tournament from mainland of China')
}