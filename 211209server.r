library(shiny)
library(tidyverse)
library(stringdist)
library(lubridate)

shinyServer(function(input, output) {
  
  #コミットログが選択されたら
  observeEvent(input$commitfile, {
    commit <-reactive(read_csv(input$commitfile$datapath))
    com_num <-count(commit())
    output$text2<-renderText(paste("コミット数:",com_num,"個"))
    output$com_table <- renderDataTable(commit())
  })
  
  #メンバーファイルが選択されたら
  observeEvent(input$memberfile, {
    member <-reactive(read_csv(input$memberfile$datapath))
    mem_num <-count(member())
    output$text3<-renderText(paste("メンバー数:",mem_num,"人"))
    output$mem_table <- renderDataTable(member())
  })
  
  #読み込みボタンを押されたら
  observeEvent(input$load, {
    #2つのデータが両方選択されてたら
    if(!is.null(input$commitfile) && !is.null(input$commitfile)){
      #csv読み込んで結合，データテーブル表示
      #updateに関しては編集距離を算出
      commit <-reactive(read.csv(input$commitfile$datapath)%>%mutate(strdist = if_else(operate == "_update",stringdist(origText, text),1)))
      member <-reactive(read.csv(input$memberfile$datapath))
      
      #userIdで上の2つを結合
      #full_data <- reactive(merge(commit(), member(), by.x="userId", by.y="id", sort = F))
      full_data <- reactive(merge(commit(), member(), by.x="userId", by.y="id", sort = F))
      #full_data <- reactive(left_join(commit2(), member(), by=c("userId"="id")))
      
      #まとめデータ的なのを作成
      view_data <- reactive(full_data() %>%
                              select(ID=commitId, 操作=operate, 名前=displayname, 距離=strdist))
      output$text1<-renderText(paste("データ数:",count(view_data()),"個"))
      output$view_table <- renderDataTable(view_data())
      
      #データにある人一覧データ作成
      mem <- reactive(full_data() %>%
                        distinct(userId, .keep_all = T) %>%
                        select(ID = userId, 名前=displayname))
      
      #選択肢の生成
      output$mem_choice <- renderUI(
        checkboxGroupInput("user", "メンバー選択",
                           choiceNames = mem()$名前,
                           choiceValues = mem()$ID,
                           selected = mem()$ID))
      #グラフ用に最初の時間と最後の時間を設定
      fir <- reactive(min(full_data()$create))
      fin <- reactive(max(full_data()$create))
      
      first_time <- reactive(tibble(userId=mem()$ID, displayname=mem()$名前, created=fir(), strdist = 0))
      finish_time <- reactive(tibble(userId=mem()$ID, displayname=mem()$名前, created=fin(), strdist = 0))
      
      #表示範囲を設定する入力欄を表示
      output$date_choice <- renderUI(
        dateRangeInput("dateRangeInput", "集計期間",
                       start=as_datetime(fir(), tz="Asia/Tokyo") %>% format("%Y-%m-%d"),
                       end  =as_datetime(fin(), tz="Asia/Tokyo") %>% format("%Y-%m-%d"))
      )
      
      #メンバー選択のたびに
      observeEvent(input$user,{
        #テーブルの生成
        mem_ope <- reactive(full_data() %>%
                              bind_rows(first_time(), finish_time()) %>%
                              filter(userId %in% input$user) %>%
                              select(名前 = displayname, 距離=strdist, ID = userId) %>%
                              group_by(名前, ID) %>%
                              summarise(操作数 = sum(距離)))
       
        #グラフの生成
        output$mem_ope_table <- renderTable(mem_ope() %>% select(距離=操作数))
        
        output$content <- renderUI({
          if(length(input$user) > 0){
            tagList(
              plotOutput('mem_ope_bar'),
              plotOutput('mem_ope_pie'))
          }
          else
            p("一人以上選択してください")
        })

        output$mem_ope_bar <- renderPlot(
          if(length(input$user) > 0)
            mem_ope() %>%
            ggplot()+
            aes(x=名前, y = 操作数)+
            geom_bar(stat="identity")+
            geom_text(aes(label = 操作数, y = 操作数*0.5), colour = "white")+
            theme_gray(base_family = "HiraKakuProN-W3"))
        output$mem_ope_pie <- renderPlot(
          if(length(input$user) > 0)
            mem_ope() %>%
            ggplot()+
            aes(x="", y = 操作数, fill = 名前)+
            geom_col()+
            coord_polar(theta="y")+
            geom_text(aes(label = 操作数), position = position_stack(vjust = 0.5),colour = "black")+
            theme_gray(base_family = "HiraKakuProN-W3"))
        
        observeEvent(input$dateRangeInput,{
          #合計データを生成
          TimeRange <- reactive(input$dateRangeInput)
          time_data_all <- reactive(full_data() %>%
                                      filter(userId %in% input$user) %>%
                                      select(created, strdist) %>%
                                      bind_rows(c(created=fin(), strdist=0)) %>%
                                      mutate("displayname" = "合計"))
          min_x <- reactive(as.POSIXct("1970-01-01 00:00:00", tz = "Japan") + as.integer(TimeRange()[1]) * 24 * 60 * 60)
          max_x <- reactive(as.POSIXct("1970-01-01 00:00:00", tz = "Japan") + as.integer(TimeRange()[2]+1) * 24 * 60 * 60)
          time_data <- reactive(full_data() %>%
                                  bind_rows(first_time(), finish_time()) %>%#みんなの開始時刻終了時刻合わせる
                                  filter(userId %in% input$user) %>%
                                  select(displayname, created, strdist) %>%
                                  bind_rows(time_data_all()))
          time_data2 <- reactive(time_data() %>%
                                  mutate(created_date = as_datetime(created, tz="Asia/Tokyo") %>% format(input$mode) %>% as.POSIXct()) %>%
                                  group_by(displayname, created_date) %>%
                                  summarize(total = sum(strdist)) %>%
                                  mutate("cumsum" = cumsum(total)))
          min_y <- reactive(min(time_data2()$cumsum[time_data2()$created_date >= min_x()]))
          max_y <- reactive(max(time_data2()$cumsum[time_data2()$created_date <= max_x()]))
          time_data3 <- reactive(time_data2() %>%
                                  ggplot()+
                                  aes(x = created_date, y = cumsum, col = displayname)+
                                  geom_line()+
                                  geom_point()+
                                  coord_cartesian(xlim=c(min_x(), max_x()), ylim=c(min_y(),max_y()))+
                                  labs(x = "操作日時", y = "累計操作数")+
                                  theme_gray(base_family = "HiraKakuProN-W3"))
          output$time_line <- renderPlot(time_data3())
        })
      })
    }
  })
})
