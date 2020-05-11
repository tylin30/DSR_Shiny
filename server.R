library(ggplot2); library(shiny); library(dplyr); library(plotly)
# 資料讀取一般是獨立在 shinyServer()函式之外
load("CA_shiny_lin.RData")
pd <- position_dodge(0.1)
#Sys.setlocale(category = "LC_ALL", locale = "zh_TW.big5")

# Text size
maintitle = 23; subtitle = 20; axistitle = 20; axistext = 16; captionsize = 14
legtitlesize = 16; legtextsize = 14

# save(df,df_no2_cha, num, num_ch, featurelong, featurelong_ch, featurewide, featurewide_ch,
#     clustername_en, clustername_ch, clustername, eng_ch, SOcompare, namevector,
#     SOcompare_long, top30,other,num_other,other_long,other_mani,media,religion,organic,
#     politics,other_sedf,list, file = "/Users/shot/Desktop/DSRshiny_upload/CA_shiny_lin.RData")


# df_no2 = df %>% filter(cluster!="2")
# df_no2_cha = df_no2
# df_no2_cha[,3] = as.character(df_no2_cha[,3])
# 
# df_no2_cha$cluster_chi[df_no2_cha$cluster=="1"] <- "反核"
# df_no2_cha$cluster_chi[df_no2_cha$cluster=="3"] <- "同婚"
# df_no2_cha$cluster_chi[df_no2_cha$cluster=="4"] <- "反同婚"
# df_no2_cha$cluster_chi[df_no2_cha$cluster=="5"] <- "積極死刑"
# df_no2_cha$cluster_chi[df_no2_cha$cluster=="6"] <- "死刑"
# df_no2_cha$cluster_chi[df_no2_cha$cluster=="7"] <- "台獨"
# df_no2_cha$cluster_chi[df_no2_cha$cluster=="8"] <- "統一"
# df_no2_cha$cluster_chi[df_no2_cha$cluster=="9"] <- "挺核"
# df_no2_cha$cluster_chi[df_no2_cha$cluster=="10"] <- "廢死"
# df_no2_cha$cluster_chi = as.factor(df_no2_cha$cluster_chi)

# colnames(featurelong_ch) = c("群集","同溫層","議題","人數","百分比","新百分比","Z分數","標準差","標準誤","區間")
# featurelong_ch[,2] = as.character(featurelong_ch[,2])
# featurelong_ch$同溫層[featurelong_ch$群集=="1"] <- "反核"
# featurelong_ch$同溫層[featurelong_ch$群集=="2"] <- "無外顯意見"
# featurelong_ch$同溫層[featurelong_ch$群集=="3"] <- "同婚"
# featurelong_ch$同溫層[featurelong_ch$群集=="4"] <- "反同婚"
# featurelong_ch$同溫層[featurelong_ch$群集=="5"] <- "積極廢死"
# featurelong_ch$同溫層[featurelong_ch$群集=="6"] <- "支持死刑"
# featurelong_ch$同溫層[featurelong_ch$群集=="7"] <- "台獨"
# featurelong_ch$同溫層[featurelong_ch$群集=="8"] <- "統一"
# featurelong_ch$同溫層[featurelong_ch$群集=="9"] <- "挺核"
# featurelong_ch$同溫層[featurelong_ch$群集=="10"] <- "反核"
# 
# 
# 
# featurelong_ch[,3] = as.character(featurelong_ch[,3])
# featurelong_ch$議題[c(1:10)] = "廢死"
# featurelong_ch$議題[c(11:20)] = "臺獨"
# featurelong_ch$議題[c(21:30)] = "同婚"
# featurelong_ch$議題[c(31:40)] = "反核"
# featurelong_ch$議題[c(41:50)] = "挺死"
# featurelong_ch$議題[c(51:60)] = "統一"
# featurelong_ch$議題[c(61:70)] = "反同婚"
# featurelong_ch$議題[c(71:80)] = "擁核"
# 
# colnames(num_ch) = c("群集","同溫層","人數","百分比","新百分比")
# num_ch[,2] = as.character(num_ch[,2])
# list = c("反核 (Antinuclear)","廢死 (EndPenalty)","無外顯意見 (No explicit opinion)","同婚 (GayMarriage)","反同婚 (Homophobia)","積極挺死 (Extreme Penalty)","挺死 (Penalty)","臺獨 (Independence)","統一 (Reunification)","擁核 (Nuclear)")
# for(i in 1:10){
#   num_ch[i,2] = list[i]
# }
# num_ch[,2] = as.factor(num_ch[,2])

# eng_ch = cbind(eng_ch,ch=c("反核","無外顯意見","同婚","反同婚","積極挺死","挺死","臺獨","統一","擁核","廢死","死刑存廢","臺灣統獨","同婚與否","反核有理"))
# eng_ch[,2]=NULL

#cltable[,1] = as.character(cltable[,1])
#list = c("反核","擁核","廢死","挺死","統一","同婚","反同婚","臺獨")
#for(i in 1:8){
#  cltable[i,1] = list[i]
#}


# for(i in 1:10){
#   colnames(top30[[i]]) = c("粉絲專頁","人數","百分比")
# }
# 
# 
# names(top30) = c("反核","無外顯意見","同婚","反同婚","積極挺死","挺死","臺獨","統一","擁核","廢死")
# 

shinyServer(function(input, output) {
  output$distPlot <- renderPlotly({
    distPlot = ggplot(num[-3,], aes(x=reorder(Cluster, -People), y=People)) +
      geom_bar(aes(text = paste0("Cluster:",Cluster)), stat = "identity") +
      geom_text(aes(label = People), size = 4, hjust = 0.5, vjust = -1, position = "stack") +
      scale_y_continuous(limits = c(0,1700)) +
      labs(x = "群聚", y = "人數", title = "各群聚人數分配", 
           subtitle = paste0("Cluster 2 (", as.character(num[which(num$Cluster == "2"),3]), ") was removed here.")) +
      theme_bw(base_family = "MicrosoftJhengHeiRegular") +
      theme(plot.title = element_text(face="bold", size=maintitle),
            plot.subtitle = element_text(size=subtitle),
            axis.title.x = element_text(size=axistitle),
            axis.title.y = element_text(size=axistitle),
            axis.text.x  = element_text(size=axistext),
            axis.text.y  = element_text(size=axistext))
    ggplotly(distPlot, tooltip = c("text","label"))
  })
  
  output$IssuePlot <- renderPlotly({
    ggplot(SOcompare_long, aes(x=議題, y=人數, fill=立場)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = paste0("同溫層人口在各議題正反立場之分配"),
           subtitle = paste0("N = ", as.character(sum(SOcompare_long$人數)))) +
      theme_bw(base_family = "MicrosoftJhengHeiRegular") +
      theme(plot.title = element_text(face="bold", size=maintitle),
            plot.subtitle = element_text(size=subtitle),
            axis.title.x = element_text(size=axistitle),
            axis.title.y = element_text(size=axistitle),
            axis.text.x  = element_text(size=axistext),
            axis.text.y  = element_text(size=axistext),
            legend.title = element_text(size=legtitlesize, face="bold"),
            legend.text = element_text(size=legtextsize))
    ggplotly()
  })
  
  
  output$Features <- renderPlotly({
    if (input$Cluster == "2"){
      Cl = ggplot(featurelong[which(featurelong$Cluster=="2"),], aes(x=Issue, y=Zscore, group = 1)) +
        geom_line(aes(y=Zscore), stat="identity", size = 1) +
        labs(y = "Z-score", 
             title = paste0("Cluster 2: No explicit opinion (N = ",as.character(num[which(num$Cluster == "2"),3]),")"),
             subtitle = "Red bars indicate the 95% CI.") +
        geom_errorbar(aes(ymin=Zscore-ci, ymax=Zscore+ci), colour="red", width=.1) +
        geom_point(size=1, colour = "red") + 
        theme_bw(base_family = "MicrosoftJhengHeiRegular") +
        scale_y_continuous(limits = c(-1,1)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size=axistext),
              axis.text.y  = element_text(size=axistext),
              plot.title = element_text(face="bold", size=maintitle),
              plot.subtitle = element_text(size=subtitle),
              axis.title.x = element_text(size=axistitle),
              axis.title.y = element_text(size=axistitle))
    }
    else{
      Cl = ggplot(featurelong[which(featurelong$Cluster==input$Cluster),], aes(x=Issue, y=Zscore, group = 1)) +
        geom_line(aes(y=Zscore), stat="identity", size = 1) +
        labs(y = "Z-score", title = paste0("Cluster ", input$Cluster, ": ",
                                          clustername_en[as.integer(input$Cluster)], " Group (N = ",
                                          num[which(num$Cluster==input$Cluster),3],")"),
             subtitle = "Red bars indicate the 95% CI.") +
        geom_errorbar(aes(ymin=Zscore-ci, ymax=Zscore+ci), colour="red", width=.1) +
        geom_point(size=1, colour = "red") + 
        theme_bw(base_family = "MicrosoftJhengHeiRegular") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size=axistext),
              axis.text.y  = element_text(size=axistext),
              plot.title = element_text(face="bold", size=maintitle),
              plot.subtitle = element_text(size=subtitle),
              axis.title.x = element_text(size=axistitle),
              axis.title.y = element_text(size=axistitle))
    }
    ggplotly(Cl)
  }#, height = 500, width = 700
  )
  
  output$AllCluFreq <- renderPlot({
    ggplot(featurelong, aes(x=Issue, y=Zscore, group = Cluster, color = Cluster)) +
        geom_line(aes(y=Zscore, colour = Cluster, size = People, alpha = .5), stat="identity") +
        geom_errorbar(aes(ymin=Zscore-ci, ymax=Zscore+ci), width=.1, position=pd) +
        geom_point(position=pd, size=1) +
        labs(y = "Z-score", title = "Tendency of all clusters", subtitle = "95% CI are shown by the bars.", 
             caption = paste0("Line size indicates number of people in that cluster. (Total N = ", as.character(sum(num$People)),")")) +
        theme_bw(base_family = "MicrosoftJhengHeiRegular") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size=axistext),
              axis.text.y  = element_text(size=axistext),
              plot.title = element_text(face="bold", size=maintitle),
              plot.subtitle = element_text(size=subtitle),
              axis.title.x = element_text(size=axistitle),
              axis.title.y = element_text(size=axistitle),
              legend.title = element_text(size=legtitlesize, face="bold"),
              legend.text = element_text(size=legtextsize)) +
        guides(size = F, alpha = F) +
        scale_colour_manual(values=c("#a6cee3", "#b2df8a", "#33a02c", "#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#ffff33","#f781bf")
                            , labels = c("Antinuclear", "EndPenalty","No explicit opinion", "GayMarriage", "AntiHomo", "(Extreme) Penalty", "Penalty", "Independence", "Reunification", "Nuclear"))
  })
  
  output$CluFreqno2 <- renderPlot({
    ggplot(featurelong[-which(featurelong$Cluster == "2"),], aes(x=Issue, y=Zscore, group = Cluster, color = Cluster)) +
        geom_line(aes(y=Zscore, colour = Cluster, size = People, alpha = .5), stat="identity") +
        geom_errorbar(aes(ymin=Zscore-ci, ymax=Zscore+ci), width=.1, position=pd) +
        geom_point(position=pd, size=1) +
        labs(y = "Z-score", title = "Tendency of all clusters except for cluster 2",
             subtitle = "95% CI are shown by the bars.",
             caption = paste0("Line size indicates number of people in that cluster. (Total N = ",as.character(sum(num$People) - num[which(num$Cluster == "2"),3]),")")) +
        theme_bw(base_family = "MicrosoftJhengHeiRegular") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size=axistext),
              axis.text.y  = element_text(size=axistext),
              plot.title = element_text(face="bold", size=maintitle),
              plot.subtitle = element_text(size=subtitle),
              plot.caption = element_text(size=captionsize),
              axis.title.x = element_text(size=axistitle),
              axis.title.y = element_text(size=axistitle),
              legend.title = element_text(size=legtitlesize, face="bold"),
              legend.text = element_text(size=legtextsize)) +
        guides(size = F, alpha = F) +
        scale_colour_manual(values=c("#a6cee3", "#b2df8a", "#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#ffff33","#f781bf")
                            , labels = c("Antinuclear", "EndPenalty", "GayMarriage", "AntiHomo", "(Extreme) Penalty", "Penalty", "Independence", "Reunification", "Nuclear"))
  })
  
  
  output$Cltable <- renderTable({
    #How to convert string to variable name? eval(parse(text = "string"))
    if (input$FeatIssue == "FeatIssue3"){
      print(num_ch[,2:4])
    }
    else if (input$FeatIssue == "FeatIssue4"){
      colnames(num_ch)[5] = "百分比"
      print(num_ch[-which(num_ch$同溫層 == "無外顯意見"),c(2,3,5)])
    }
    else{
      for (i in as.character(1:10)){
        if (input$Cluster == i){
          cltable <<- arrange(featurelong_ch[which(featurelong_ch$群集==i),c(3,7,8)],
                              desc(Z分數))
        }
      }
      print(cltable)
    }
  })
  
  output$CluFreq <- renderPlotly({
    if (!(input$Cluster == "All Clusters"||input$Cluster == "All Clusters (No Cluster 2)")){
      df %>% filter(cluster == input$Cluster) %>% select(matches(input$Issue)) %>%
        ggplot(aes_string(x=input$Issue)) + geom_histogram(aes()) +
        labs(x = paste(input$Issue,"Z-score"), 
             title = paste0("Cluster ",input$Cluster," (N = ",
                            as.character(num[which(num$Cluster==input$Cluster),3]),
                            ") Score on Issue: ", input$Issue)) +
        theme_bw(base_family = "MicrosoftJhengHeiRegular") +
        theme(plot.title = element_text(face="bold", size=maintitle),
              plot.subtitle = element_text(size=subtitle),
              axis.title.x = element_text(size=axistitle),
              axis.title.y = element_text(size=axistitle),
              axis.text.x  = element_text(size=axistext),
              axis.text.y  = element_text(size=axistext))
      ggplotly()
    }
    
  }#, height = 250, width = 700
  )
  
  output$CA3D_S<- renderPlotly({
    plot_ly(df_no2_cha, x = ~EndPenalty, y = ~Independence, z = ~GayMarriage, 
            color = ~cluster_chi, mode="markers", marker = list(size = 3)) %>%
      layout(#title = "支持",
             scene = list(
               xaxis = list(title = "廢死"), 
               yaxis = list(title = "臺獨"), 
               zaxis = list(title = "同婚")))
  })
  output$CA3D_O<- renderPlotly({
    plot_ly(df_no2_cha, x = ~Penalty, y = ~Reunification, z = ~Homophobia, 
            color = ~cluster_chi, mode="markers", marker = list(size = 3)) %>%
      layout(#title = "支持",
        scene = list(
          xaxis = list(title = "挺死"), 
          yaxis = list(title = "統一"), 
          zaxis = list(title = "反同")))
  })
  
  # output$CA3D_T<- renderPlotly({
  #   plot_ly(df_no2, x = ~P_total, y = ~T_total, z = ~G_total,
  #           color = ~cluster_chi)%>%
  #     layout(#title = "立場加總",
  #            scene = list(
  #              xaxis = list(title = "死刑存廢"), 
  #              yaxis = list(title = "臺灣統獨"), 
  #              zaxis = list(title = "同婚與否")))
  # })
  
  output$CA3Dsidertext <- renderUI({
    if (input$CA3D == "3DS"|| input$CA3D == "3DO"|| input$CA3D == "3DSOF"){
      str1 = paste("<ul><li>","每個點皆代表一個觀察值（人）。","</li>")
      str2 = paste("<li>","顏色表示該人所屬的同溫層。","</li>")
      str3 = paste("<li>","在各軸的分數越高，表示越支持該立場。","</li></ul>")
      HTML(paste(str1, str2, str3))
    }
    else {
      str1 = paste("<ul><li>","此圖即將相互對立的立場分數相減，例如：贊成廢死的分數 - 贊成挺死的分數。","</li>")
      str2 = paste("<li>","同樣地，每個點皆代表一個觀察值（人），顏色表示該人所屬的同溫層。","</li>")
      str3 = paste("<li>","在「同婚與否」的分數越大於0，則表示該人越支持同志婚姻。<br>在「臺灣統獨」的分數越大於0，則表示該人越支持臺灣獨立。<br>在「死刑存廢」的分數越大於0，則表示該人越支持廢除死刑。<br>在「反核有理」的分數越大於0，則表示該人越反對核電。","</li></ul>")
      HTML(paste(str1, str2, str3))
      }
    })
  
  output$dimchoice <- renderUI({
    if (input$dimchange == "支持"){
      tagList(
        selectInput(inputId = "dim_3DSOF_x", label = "x軸",
                  choices=c("廢死"="EndPenalty", "臺獨"="Independence", "同婚"="GayMarriage",
                            "反核"="Antinuclear", "挺死"="Penalty", "統一"="Reunification",
                            "反同婚"="Homophobia", "擁核"="Nuclear"),
                  selected="反核"
                  ),
        selectInput(inputId = "dim_3DSOF_y", label = "y軸",
                  choices=c("廢死"="EndPenalty", "臺獨"="Independence", "同婚"="GayMarriage",
                            "反核"="Antinuclear", "挺死"="Penalty", "統一"="Reunification",
                            "反同婚"="Homophobia", "擁核"="Nuclear"),
                  selected="臺獨"
                  ),
        selectInput(inputId = "dim_3DSOF_z", label = "z軸",
                  choices=c("廢死"="EndPenalty", "臺獨"="Independence", "同婚"="GayMarriage",
                            "反核"="Antinuclear", "挺死"="Penalty", "統一"="Reunification",
                            "反同婚"="Homophobia", "擁核"="Nuclear"),
                  selected="同婚"
                  )
              )
    }
    else if (input$dimchange == "立場加總"){
      tagList(
        selectInput(inputId = "dim_3DTF_x", label = "x軸",
                  choices=c("死刑存廢"="P_total", "臺灣統獨"="T_total", "同婚與否"="G_total",
                            "反核有理"="N_total"),
                  selected="同婚與否"
                    ),
        selectInput(inputId = "dim_3DTF_y", label = "y軸",
                  choices=c("死刑存廢"="P_total", "臺灣統獨"="T_total", "同婚與否"="G_total",
                            "反核有理"="N_total"),
                  selected="死刑存廢"
                    ),
        selectInput(inputId = "dim_3DTF_z", label = "z軸",
                  choices=c("死刑存廢"="P_total", "臺灣統獨"="T_total", "同婚與否"="G_total",
                            "反核有理"="N_total"),
                  selected="臺灣統獨"
                    )
            )
    }
  })
  
  output$CA3D_SOF <-renderPlotly({
    plot_ly(df_no2_cha, x = ~eval(parse(text = input$dim_3DSOF_x)), y = ~eval(parse(text = input$dim_3DSOF_y)), z = ~eval(parse(text = input$dim_3DSOF_z)), 
            color = ~cluster_chi, mode="markers", marker = list(size = 3)) %>%
      layout(
        scene = list(
          xaxis = list(title = eng_ch[which(eng_ch$eng==input$dim_3DSOF_x),2]), 
          yaxis = list(title = eng_ch[which(eng_ch$eng==input$dim_3DSOF_y),2]), 
          zaxis = list(title = eng_ch[which(eng_ch$eng==input$dim_3DSOF_z),2])))
  })
  
  output$CA3D_TF_Error <- renderUI({
    if (is.null(input$dim_3DTF_x)==T||is.null(input$dim_3DTF_y)==T||is.null(input$dim_3DTF_z)==T){
      str1 = paste("<h3>","請在左欄勾選「立場加總」來選擇維度。","</h3>")
      HTML(paste(str1))
    }
  })
  
  output$CA3D_TF <-renderPlotly({
    if (is.null(input$dim_3DTF_x)==F||is.null(input$dim_3DTF_y)==F||is.null(input$dim_3DTF_z)==F){
      plot_ly(df_no2_cha, x = ~eval(parse(text = input$dim_3DTF_x)), y = ~eval(parse(text = input$dim_3DTF_y)), z = ~eval(parse(text = input$dim_3DTF_z)), 
              color = ~cluster_chi, mode="markers", marker = list(size = 3)) %>%
        layout(
          scene = list(
            xaxis = list(title = eng_ch[which(eng_ch$eng==input$dim_3DTF_x),2]), 
            yaxis = list(title = eng_ch[which(eng_ch$eng==input$dim_3DTF_y),2]), 
            zaxis = list(title = eng_ch[which(eng_ch$eng==input$dim_3DTF_z),2])))
    }
  })
  
  # http://stackoverflow.com/questions/40760935/use-the-same-datatableoutput-in-different-tabs
  # https://rstudio.github.io/DT/shiny.html
  table_data <- reactive({
    DT::datatable(top30[[as.integer(input$Fan)]])
  })
  
  output$Fan_table1 <- DT::renderDataTable(table_data())
  output$Fan_table2 <- DT::renderDataTable(table_data())
  output$Fan_table3 <- DT::renderDataTable(table_data())
  output$Fan_table4 <- DT::renderDataTable(table_data())
  output$Fan_table5 <- DT::renderDataTable(table_data())
  output$Fan_table6 <- DT::renderDataTable(table_data())
  output$Fan_table7 <- DT::renderDataTable(table_data())
  output$Fan_table8 <- DT::renderDataTable(table_data())
  output$Fan_table9 <- DT::renderDataTable(table_data())
  output$Fan_table10 <- DT::renderDataTable(table_data())
  
  output$numtext <- renderUI({ 
    str1 = paste("<h4>","該同溫層有", as.character(num[which(num$Cluster == input$Fan),3]), "人。","<h4>")
    str2 = paste("<h4>","下表為這些人所按讚的Facebook專頁之排名。","<h4>")
    HTML(paste(str1, str2))
  })
  
  output$PotPlot <- renderPlot({
    politics = filter(other_long,Issue=="民進黨"|Issue =="國民黨"|Issue == "親民黨"|Issue =="時代力量"|Issue=="民國黨"|Issue=="無黨籍")
    ggplot(politics, aes(x=Issue, y=Zscore, group = Type, color = Type)) +
      geom_line(aes(y=Zscore, colour = Type, size = People, alpha = .5), stat="identity") +
      geom_errorbar(aes(ymin=Zscore-ci, ymax=Zscore+ci), width=.1, position=pd) +
      geom_point(position=pd, size=1) +
      labs(x=NULL,y = "Z-score", title = "Tendency of all clusters", subtitle = "95% CI are shown by the bars.", 
           caption = paste0("Line size indicates number of people in that cluster. (Total N = ", as.character(sum(num_other$People)),")")) +
      theme_bw(base_family = "MicrosoftJhengHeiRegular") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=axistext),
            axis.text.y  = element_text(size=axistext),
            plot.title = element_text(face="bold", size=maintitle),
            plot.subtitle = element_text(size=subtitle),
            axis.title.x = element_text(size=axistitle),
            axis.title.y = element_text(size=axistitle),
            legend.title = element_text(size=legtitlesize, face="bold"),
            legend.text = element_text(size=legtextsize)) +
      guides(size = F, alpha = F) +
      scale_colour_manual(values=c("#a6cee3", "#b2df8a", "#33a02c", "#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#ffff33","#f781bf")
                          #, labels = c("Antinuclear", "EndPenalty", "No explicit opinion", "GayMarriage", "Homophobia", "(Extreme) Penalty", "Penalty", "Independence", "Reunification", "Nuclear")
      )
  })
  
  output$RelPlot <- renderPlot({
    ggplot(religion, aes(x=Issue, y=Zscore, group = Type, fill=Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      #geom_line(aes(y=Zscore, colour = Cluster, size = People, alpha = .5), stat="identity") +
      geom_errorbar(aes(ymin=Zscore-ci, ymax=Zscore+ci), width=.1, position =  position_dodge(.9)) +
      #+geom_point(position=position_dodge(width = 1), size=1) +
      labs(y = "Z-score", title = "Tendency of all clusters", subtitle = "95% CI are shown by the bars.", 
           caption = paste0("Line size indicates number of people in that cluster. (Total N = ", as.character(sum(num$People)),")")) +
      theme_bw(base_family = "MicrosoftJhengHeiRegular") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=axistext),
            axis.text.y  = element_text(size=axistext),
            plot.title = element_text(face="bold", size=maintitle),
            plot.subtitle = element_text(size=subtitle),
            axis.title.x = element_text(size=axistitle),
            axis.title.y = element_text(size=axistitle),
            legend.title = element_text(size=legtitlesize, face="bold"),
            legend.text = element_text(size=legtextsize)) +
      guides(size = F, alpha = F) +
      scale_fill_manual(name = "Cluster",values=c("#a6cee3", "#b2df8a","#33a02c", "#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#ffff33","#f781bf")
                        #, labels = c("Antinuclear", "EndPenalty","No explicit opinion", "GayMarriage", "Homophobia", "(Extreme) Penalty", "Penalty", "Independence", "Reunification", "Nuclear")
      )+
    scale_colour_manual(name = "Cluster",values=c("#305366", "#4c722a","#075103", "#7f201f","#5b0607","#8c6027","#8c4907","#693384","#999904","#911556"))    
  })
  
  output$MedPlot <- renderPlot({
    media = filter(other_long,Issue=="蘋果日報"|Issue=="中央社"|Issue== "中時電子報"|Issue== "中視新聞"|Issue== "大紀元"|Issue== "東森新聞"|Issue== "自由時報"|Issue== "新頭殼"|Issue== "壹週刊"|Issue== "新唐人"|Issue=="三立新聞"|Issue== "風傳媒"|Issue== "TVBS"|Issue== "聯合新聞網")
    
    ggplot(media, aes(x=Issue, y=Zscore, group = Type, color = Type)) +
      geom_line(aes(y=Zscore, colour = Type, size = People, alpha = .5), stat="identity") +
      geom_errorbar(aes(ymin=Zscore-ci, ymax=Zscore+ci), width=.1, position=pd) +
      geom_point(position=pd, size=1) +
      labs(x = NULL,y = "Z-score", title = "Tendency of all clusters", subtitle = "95% CI are shown by the bars.", 
           caption = paste0("Line size indicates number of people in that cluster. (Total N = ", as.character(sum(num_other$People)),")")) +
      theme_bw(base_family = "MicrosoftJhengHeiRegular") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=axistext),
            axis.text.y  = element_text(size=axistext),
            plot.title = element_text(face="bold", size=maintitle),
            plot.subtitle = element_text(size=subtitle),
            axis.title.x = element_text(size=axistitle),
            axis.title.y = element_text(size=axistitle),
            legend.title = element_text(size=legtitlesize, face="bold"),
            legend.text = element_text(size=legtextsize)) +
      guides(size = F, alpha = F) +
      scale_colour_manual(values=c("#a6cee3", "#33a02c", "#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#ffff33","#f781bf", "#b2df8a")
                          #, labels = c("Antinuclear","No explicit opinion", "GayMarriage", "Homophobia", "(Extreme) Penalty", "Penalty", "Independence", "Reunification", "Nuclear", "EndPenalty")
      )
  })
  
  output$OrgPlot <- renderPlot({
    organic = filter(other_long,Issue=="有機食物")
    ggplot(organic, aes(x=Issue, y=Zscore, group = Type,fill=Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      #geom_line(aes(y=Zscore, colour = Cluster, size = People, alpha = .5), stat="identity") +
      geom_errorbar(aes(ymin=Zscore-ci, ymax=Zscore+ci), width=.1, position =  position_dodge(.9)) +
      #+geom_point(position=position_dodge(width = 1), size=1) +
      labs(x =NULL,y = "Z-score", title = "Tendency of all clusters", subtitle = "95% CI are shown by the bars.", 
           caption = paste0("Line size indicates number of people in that cluster. (Total N = ", as.character(sum(num_other$People)),")")) +
      theme_bw(base_family = "MicrosoftJhengHeiRegular") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=axistext),
            axis.text.y  = element_text(size=axistext),
            plot.title = element_text(face="bold", size=maintitle),
            plot.subtitle = element_text(size=subtitle),
            axis.title.x = element_text(size=axistitle),
            axis.title.y = element_text(size=axistitle),
            legend.title = element_text(size=legtitlesize, face="bold"),
            legend.text = element_text(size=legtextsize)) +
      guides(size = F, alpha = F) +
      scale_fill_manual(name = "Cluster",values=c("#a6cee3", "#b2df8a", "#33a02c", "#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#ffff33","#f781bf")
                        #, labels = c("Antinuclear", "EndPenalty","No explicit opinion", "GayMarriage", "Homophobia", "(Extreme) Penalty", "Penalty", "Independence", "Reunification", "Nuclear")
      )
  })
  
})