library(shiny)
library(shinydashboard)
library(ggplot2)
library(reshape2)
library(stats)
library(Rlab)
library(dplyr)
library(formattable)
library(shinyWidgets)
library(XML)
library(reshape2)
library(plyr)
library(gridExtra)
library(plotly)

shinyServer <- function(session, input, output){
  observeEvent(input$go, {
    updateTabItems(session, "tabs", "survival_rate")
  })
  observeEvent(input$info, {
  sendSweetAlert(
    session = session,
    title = "Hint:",
    type = NULL,
    closeOnClickOutside = TRUE,
    text="Please click the Challenge in the sidebar menu you'd like to test"
  )
  })
  
  data<- read.csv("survival_rate.csv")
  age <- data[,1]
  data2 <- read.csv("Pyramid.csv")
  us_2010 <- read.csv("us-est00int-alldata.csv", header = TRUE, sep=",")
  us_1979 <- read.csv("us_1900_1979.csv", header = FALSE)
  us_1989 <- read.csv("us_1980_1989.csv", header = FALSE)
  us_1999 <- read.csv("us_1990_1999.csv", header = FALSE)
  uk_inter <- read.csv("1991_2018_uk.csv", header = TRUE)
  fertility <- read.csv("fertility.csv", header = TRUE)
  
  #survival rate tab:
  observeEvent(input$selectAll_s,{
    # if(input$selectAll_s == 0) return(NULL) 
    # else 
    if ((input$selectAll_s %% 2) == 0) {
      updateButton(session, "selectAll_s", label = "Select All")
      updateCheckboxGroupInput(session,"check", choices=c("United Kingdom-Male", "United Kingdom-Female", "United States-Male", "United States-Female", "China-Male", "China-Female"))
    }
    else{
      updateButton(session, "selectAll_s", label = "Unselect")
      updateCheckboxGroupInput(session,"check",choices=c("United Kingdom-Male", "United Kingdom-Female", "United States-Male", "United States-Female", "China-Male", "China-Female"), 
                               selected = c("United Kingdom-Male", "United Kingdom-Female", "United States-Male", "United States-Female", "China-Male", "China-Female"))
    }
    
  })
  
  output$lineChart <- renderPlot({  
   #  yrange <- c(0,1)
   #  xrange <- range(age)
   #  plot(xrange,yrange,type="n",cex.lab=0.0000001,
   #       xaxs="i", yaxs="i",
   #       main=paste("Survival Rate for comparison"))
   #  title(xlab="Age", ylab="Survival Rate")
      country <- input$check
   #   colors <-c()
   #   if ("United Kingdom-Male" %in% country){
   #     chartdata1 <- data[,2]
   #     lines(age,chartdata1,col="#5E95A8",lwd=3)
   #     colors<-c(colors, "#5E95A8")
   #   }
   #   if ("United Kingdom-Female" %in% country){
   #     chartdata2 <- data[,3]
   #     lines(age,chartdata2,col="#F38770",lwd=3)
   #     colors<-c(colors, "#F38770")
   #   }
   #   if ("United States-Male" %in% country){
   #     chartdata3 <- data[,4]
   #     lines(age[1:100],chartdata3[1:100],col="#487C44",lwd=3)
   #     colors<-c(colors, "#487C44")
   #   }
   #   if ("United States-Female" %in% country){
   #     chartdata4 <- data[,5]
   #     lines(age[1:100],chartdata4[1:100],col="#CACA3B",lwd=3)
   #     colors<-c(colors, "#CACA3B")
   #   }
   #   if ("China-Male" %in% country){
   #     chartdata5 <- data[,6]
   #     lines(age,chartdata5,col="#7C627B",lwd=3)
   #     colors<-c(colors, "#7C627B")
   #   }
   #   if ("China-Female" %in% country){
   #     chartdata6 <- data[,7]
   #     lines(age,chartdata6,col="#FEA54C",lwd=3)
   #     colors<-c(colors, "#FEA54C")
   #   }
   #   if (length(country) !=0){
   #     legend("bottomleft",country, 
   #            col=colors,pch=15,ncol=1,bty ="n",cex=1.1)}
   # },height = 500, width = 600)
    
    longData <- cbind(rep(data$age, 6), stack(data[,2:7]))
    names(longData) <- c("age", "sRate", "grp")
    longData <- 
      transform(longData, color = ifelse(
        grp == "uk.m",
        '#5E95A8',
        ifelse(
          grp == "uk.f",
          '#F38770',
          ifelse(
            grp == "us.m",
            '#487C44',
            ifelse(
              grp == "us.f",
              '#CACA3B',
              ifelse(
                grp == "china.m",
                '#7C627B',
                ifelse(
                  grp == "china.f", 
                  '#FEA54C',
                  NA
                )
              )
            )
          )
        )
      ))
    
    longData$grp <- 
      plyr::revalue(
        longData$grp,
        c(
          "uk.m" = "United Kingdom-Male",
          "uk.f" = "United Kingdom-Female",
          "us.m" = "United States-Male",
          "us.f" = "United States-Female",
          "china.m" = "China-Male",
          "china.f" = "China-Female"
        )
      )
    
    subLData <- dplyr::filter(longData, longData$grp %in% country)
    sp <- ggplot(data = subLData, aes(x = age, y = sRate, color = grp)) +
      scale_y_continuous(expand = expand_scale(mult = 0, add = c(0, 0.05)),
                         limits = c(0,1)) +
      scale_x_continuous(expand = expand_scale(mult = 0, add = c(0, 5)), limits = c(0, 100)) +
      theme(
        panel.background = element_rect(fill = 'white', color = 'black'),
        text = element_text(size = 14),
        plot.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.position = c(0.2, 0.3),
        legend.title = element_blank()
      ) +
      labs(title = "Survival Rate for Comparison", 
           y = "Survival Rate",
           x = "Age(yrs)") +
      geom_line(lwd = 1) +
      scale_color_manual(breaks = longData$grp, values = unique(as.character(longData$color)))
    sp
  })

  
  #pyramid:
  #uk:
  uk <- data2[,c(1,6,7)]  
  names(uk) <- c("Age", "UK_Female", "UK_Male")
  uk$UK_Male <- -1 * uk$UK_Male
  uk$Age <- factor(uk$Age, levels = uk$Age, labels = uk$Age)
  df1 <- melt(uk, 
              value.name='Population', 
              variable.name = 'Gender', 
              id.vars='Age' ) 
  uk_plot <- ggplot(df1, mapping = aes(x = Age, y = Population, fill = Gender)) + 
    #ggtitle("United Kingdom")+
    geom_bar(subset = .(Gender == "UK_Male"), stat = "identity", show.legend = F) + 
    geom_bar(subset = .(Gender == "UK_Female"), stat = "identity", show.legend = F) + 
    scale_y_continuous(breaks = seq(-100000, 100000, 20000), 
                       labels = paste0(as.character(c(seq(100, 0, -20), seq(20, 100, 20)))))+
    scale_x_discrete(breaks = seq(0,100,5), 
                     labels = paste0(seq(0,100,5)))+
    coord_flip() + 
    #scale_fill_brewer(palette = "Set1") + 
    scale_fill_manual(values=c("#F38770", "#5E95A8"))+
    theme_bw()+
    ylab("Percentage of Still Live")
  
  #us:
  us <- data2[,c(1,2,3)]  
  names(us) <- c("Age", "US_Female", "US_Male")
  us$US_Male <- -1 * us$US_Male
  us$Age <- factor(us$Age, levels = us$Age, labels = us$Age)
  df2 <- melt(us, 
              value.name='Population', 
              variable.name = 'Gender', 
              id.vars='Age' ) 
  us_plot <- ggplot(df2, mapping = aes(x = Age, y = Population, fill = Gender)) + 
    #ggtitle("United States")+
    geom_bar(subset = .(Gender == "US_Male"), stat = "identity", show.legend = F) + 
    geom_bar(subset = .(Gender == "US_Female"), stat = "identity", show.legend = F) + 
    scale_y_continuous(breaks = seq(-100000, 100000, 20000), 
                       labels = paste0(as.character(c(seq(100, 0, -20), seq(20, 100, 20))), "%" )) +
    scale_x_discrete(breaks = seq(0,100,5), 
                     labels = paste0(seq(0,100,5)))+
    coord_flip() + 
    #scale_fill_brewer(palette = "Set1") + 
    scale_fill_manual(values=c("#CACA3B", "#487C44"))+
    theme_bw()+
    ylab("Percentage of Still Live")
  #cn:
  cn <- data2[,c(1,4,5)]  
  names(cn) <- c("Age", "CN_Female", "CN_Male")
  cn$CN_Male <- -1 * cn$CN_Male
  cn$Age <- factor(cn$Age, levels = cn$Age, labels = cn$Age)
  df3 <- melt(cn, 
              value.name='Population', 
              variable.name = 'Gender', 
              id.vars='Age' ) 
  cn_plot <- ggplot(df3, mapping = aes(x = Age, y = Population, fill = Gender)) +
    #ggtitle("China")+
    geom_bar(subset = .(Gender == "CN_Male"), stat = "identity") + 
    geom_bar(subset = .(Gender == "CN_Female"), stat = "identity") + 
    scale_y_continuous(breaks = seq(-100000, 100000, 20000), 
                       labels = paste0(as.character(c(seq(100, 0, -20), seq(20, 100, 20))))) +
    scale_x_discrete(breaks = seq(0,100,5), 
                     labels = paste0(seq(0,100,5)))+
    coord_flip() + 
    # scale_fill_brewer(palette = "Set1") + 
    # scale_fill_hue(c=45, l=75)
    scale_fill_manual(values=c("#FEA54C", "#7C627B"))+
    theme_bw() +
    ylab("Percentage of Still Live")
  # g_legend<-function(a.gplot){
  #   tmp <- ggplot_gtable(ggplot_build(a.gplot))
  #   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  #   legend <- tmp$grobs[[leg]]
  #  return(legend)}
  #pyramid tab (1):
  output$title1 <- renderUI({
    h4(input$check2) 
  })
  output$pyramid <- renderPlotly({
    if ("United Kingdom-United States" %in% input$check2){
      p1 <- ggplotly(uk_plot) %>% layout(xaxis = list(showgrid = F), yaxis = list(showgrid = F))
      p2 <- ggplotly(us_plot) %>% layout(xaxis = list(showgrid = F), yaxis = list(showgrid = F))
    }
    if ("United Kingdom-China" %in% input$check2){
      #p1 <- uk_plot + theme(legend.position="bottom")
      p1 <- ggplotly(uk_plot) %>% layout(xaxis = list(showgrid = F), yaxis = list(showgrid = F))
      p2 <- ggplotly(cn_plot) %>% layout(xaxis = list(showgrid = F), yaxis = list(showgrid = F))
    }
    if ("United States-China" %in% input$check2){
      p1 <- ggplotly(us_plot) %>% layout(xaxis = list(showgrid = F), yaxis = list(showgrid = F))
      p2 <- ggplotly(cn_plot) %>% layout(xaxis = list(showgrid = F), yaxis = list(showgrid = F))
    }
    p <- subplot(p1,p2, shareX=T) %>% layout(legend = list(orientation='h', x=0, y=10))
    
    # mylegend<-g_legend(p1)
    # p3 <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
    #                                p2 + theme(legend.position="none"),
    #                                nrow=1),
    #                   mylegend, nrow=2,heights=c(10, 1))
    
  })
  
  #pyramid (2):
  color_r <- c()
  color_l <- c()
  output$pyramid2 <- renderPlotly({
    if ("United Kingdom-Male" %in% input$check4){
      color_r <- c(color_r, "#5E95A8")
      if ("United States-Male" %in% input$check5){
        com <- data2[,c(1,3,7)]
        color_l <- c(color_l, "#487C44")
      }
      if ("United States-Female" %in% input$check5){
        com <- data2[,c(1,2,7)]
        color_l <- c(color_l, "#CACA3B")
      }
      if ("China-Male" %in% input$check5){
        com <- data2[,c(1,5,7)]
        color_l <- c(color_l, "#7C627B")
      }
      if ("China-Female" %in% input$check5){
        com <- data2[,c(1,4,7)]
        color_l <- c(color_l, "#FEA54C")
      }
      if ("United Kingdom-Female" %in% input$check5){
        com <- data2[,c(1,6,7)]
        color_l <- c(color_l, "#F38770")
      }
      if ("United Kingdom-Male" %in% input$check5){
        sendSweetAlert(
          session = session,
          title = "Hint:",
          type = NULL,
          closeOnClickOutside = TRUE,
          text="Please choose different countries or genders to compare"
        )
        return(NULL)
      }
    }
    if ("United Kingdom-Female" %in% input$check4){
      color_r<- c(color_r, "#F38770")
      if ("United States-Male" %in% input$check5){
        com <- data2[,c(1,3,6)]
        color_l <- c(color_l, "#487C44")
      }
      if ("United States-Female" %in% input$check5){
        com <- data2[,c(1,2,6)]
        color_l <- c(color_l, "#CACA3B")
      }
      if ("China-Male" %in% input$check5){
        com <- data2[,c(1,5,6)]
        color_l <- c(color_l, "#7C627B")
      }
      if ("China-Female" %in% input$check5){
        com <- data2[,c(1,4,6)]
        color_l <- c(color_l, "#FEA54C")
      }
      if ("United Kingdom-Male" %in% input$check5){
        com <- data2[,c(1,7,6)]
        color_l <- c(color_l, "#5E95A8")
      }
      if ("United Kingdom-Female" %in% input$check5){
        sendSweetAlert(
          session = session,
          title = "Hint:",
          type = NULL,
          closeOnClickOutside = TRUE,
          text="Please choose different countries or genders to compare"
        )
        return(NULL)
      }
    }
    if ("United States-Female" %in% input$check4){
      color_r<- c(color_r, "#CACA3B")
      if("United Kingdom-Male" %in% input$check5){
        com <- data2[,c(1,7,2)]
        color_l <- c(color_l, "#5E95A8")
      }
      if ("United Kingdom-Female" %in% input$check5){
        com <- data2[,c(1,6,2)]
        color_l <-c(color_l, "#F38770")
      }
      if("China-Male" %in% input$check5){
        com <- data2[,c(1,5,2)]
        color_l <- c(color_l, "#7C627B")
      }
      if("China-Female" %in% input$check5){
        com <- data2[,c(1,4,2)]
        color_l <- c(color_l, "#FEA54C")
      }
      if("United States-Male" %in% input$check5){
        com <- data2[,c(1,3,2)]
        color_l <- c(color_l, "#487C44")
      }
      if ("United States-Female" %in% input$check5){
        sendSweetAlert(
          session = session,
          title = "Hint:",
          type = NULL,
          closeOnClickOutside = TRUE,
          text="Please choose different countries or genders to compare"
        )
        return(NULL)
      }
    }
    if ("United States-Male" %in% input$check4){
      color_r <- c(color_r, "#487C44")
      if("United Kingdom-Male" %in% input$check5){
        com <- data2[,c(1,7,3)]
        color_l <- c(color_l, "#5E95A8")
      }
      if ("United Kingdom-Female" %in% input$check5){
        com <- data2[,c(1,6,3)]
        color_l <- c(color_l, "#F38770")
      }
      if("China-Male" %in% input$check5){
        com <- data2[,c(1,5,3)]
        color_l <- c(color_l, "#7C627B")
      }
      if("China-Female" %in% input$check5){
        com <- data2[,c(1,4,3)]
        color_l <- c(color_l, "#FEA54C")
      }
      if("United States-Female" %in% input$check5){
        com <- data2[,c(1,2,3)]
        color_l <- c(color_l, "#CACA3B")
      }
      if ("United States-Male" %in% input$check5){
        sendSweetAlert(
          session = session,
          title = "Hint:",
          type = NULL,
          closeOnClickOutside = TRUE,
          text="Please choose different countries or genders to compare"
        )
        return(NULL)
      }
    }
    if ("China-Female" %in% input$check4){
      color_r<- c(color_r, "#FEA54C")
      if("United Kingdom-Male" %in% input$check5){
        com <- data2[,c(1,7,4)]
        color_l <- c(color_l, "#5E95A8")
      }
      if("United Kingdom-Female" %in% input$check5){
        com <- data2[,c(1,6,4)]
        color_l <-c(color_l, "#F38770")
      }
      if("United States-Male" %in% input$check5){
        com <- data2[,c(1,3,4)]
        color_l <- c(color_l, "#487C44")
      }
      if("United States-Female" %in% input$check5){
        com <- data2[,c(1,2,4)]
        color_l <- c(color_l, "#CACA3B")
      }
      if ("China-Male" %in% input$check5){
        com <- data2[,c(1,5,4)]
        color_l <- c(color_l, "#7C627B")
      }
      if ("China-Female" %in% input$check5){
        sendSweetAlert(
          session = session,
          title = "Hint:",
          type = NULL,
          closeOnClickOutside = TRUE,
          text="Please choose different countries or genders to compare"
        )
        return(NULL)
      }
    }
    if ("China-Male" %in% input$check4){
      color_r<- c(color_r, "#7C627B")
      if("United Kingdom-Male" %in% input$check5){
        com <- data2[,c(1,7,5)]
        color_l <- c(color_l, "#5E95A8")
      }
      if("United Kingdom-Female" %in% input$check5){
        com <- data2[,c(1,6,5)]
        color_l <- c(color_l, "#F38770")
      }
      if("United States-Male" %in% input$check5){
        com <- data2[,c(1,3,5)]
        color_l <- c(color_l, "#487C44")
      }
      if("United States-Female" %in% input$check5){
        com <- data2[,c(1,2,5)]
        color_l <- c(color_l, "#CACA3B")
      }
      if ("China-Female" %in% input$check5){
        com <- data2[,c(1,4,5)]
        color_l <- c(color_l, "#FEA54C")
      }
      if ("China-Male" %in% input$check5){
        sendSweetAlert(
          session = session,
          title = "Hint:",
          type = NULL,
          closeOnClickOutside = TRUE,
          text="Please choose different countries or genders to compare"
        )
        return(NULL)
      }
    }
    
    names(com) <- c("Age", paste0(input$check5), paste0(input$check4))
    com[,2] <- -1 * com[,2]
    com$Age <- factor(com$Age, levels = com$Age, labels = com$Age)
    df_com <- melt(com, 
                   value.name='Population', 
                   variable.name = 'Country_Gender', 
                   id.vars='Age' )
    plot_com <- ggplot(df_com, mapping = aes(x = Age, y = Population, fill = Country_Gender)) +
      geom_bar(subset = .(Country_Gender == paste0(input$check4)), stat = "identity") + 
      geom_bar(subset = .(Country_Gender == paste0(input$check5)), stat = "identity") +
      scale_y_continuous(breaks = seq(-100000, 100000, 10000), 
                         labels = paste0(as.character(c(seq(100, 0, -10), seq(10, 100, 10))))) +
      scale_x_discrete(breaks = seq(0,100,5), 
                       labels = paste0(seq(0,100,5)))+
      coord_flip() + 
      theme_bw() +
      #scale_fill_brewer(palette = "Spectral") + 
      scale_fill_manual(values=c(color_l,color_r))+
      ylab("Percentage of Still Live")
    ggplotly(plot_com) %>% layout(legend = list(orientation='h', x=0, y=10))
  })
  # output$info1 <- renderText({
  #   paste0("Age = ", round(input$plot_click1$y-1), "\nPopulation=", input$plot_click1$x/1000, "%")
  # })
  
  #intercensal_us
  output$intercensal_us <- renderPlot({
    #1900-1909
    if (1900 == input$check3){
      us_est <- us_1979[1:76, c(1,4,3)]
    }
    if (1901 == input$check3){
      us_est <- us_1979[77:152, c(1,4,3)]
    }
    if (1902 == input$check3){
      us_est <- us_1979[153:228, c(1,4,3)]
    }
    if (1903 == input$check3){
      us_est <- us_1979[229:304, c(1,4,3)]
    }
    if (1904 == input$check3){
      us_est <- us_1979[305:380, c(1,4,3)]
    }
    if (1905 == input$check3){
      us_est <- us_1979[381:456, c(1,4,3)]
    }
    if (1906 == input$check3){
      us_est <- us_1979[457:532, c(1,4,3)]
    }
    if (1907 == input$check3){
      us_est <- us_1979[533:608, c(1,4,3)]
    }
    if (1908 == input$check3){
      us_est <- us_1979[609:684, c(1,4,3)]
    }
    if (1909 == input$check3){
      us_est <- us_1979[685:760, c(1,4,3)]
    }
    #1910-1919
    if (1910 == input$check3){
      us_est <- us_1979[761:836, c(1,4,3)]
    }
    if (1911 == input$check3){
      us_est <- us_1979[837:912, c(1,4,3)]
    }
    if (1912 == input$check3){
      us_est <- us_1979[913:988, c(1,4,3)]
    }
    if (1913 == input$check3){
      us_est <- us_1979[989:1064, c(1,4,3)]
    }
    if (1914 == input$check3){
      us_est <- us_1979[1065:1140, c(1,4,3)]
    }
    if (1915 == input$check3){
      us_est <- us_1979[1141:1216, c(1,4,3)]
    }
    if (1916 == input$check3){
      us_est <- us_1979[1217:1292, c(1,4,3)]
    }
    if (1917 == input$check3){
      us_est <- us_1979[1293:1368, c(1,4,3)]
    }
    if (1918 == input$check3){
      us_est <- us_1979[1369:1444, c(1,4,3)]
    }
    if (1919 == input$check3){
      us_est <- us_1979[1445:1520, c(1,4,3)]
    }
    #1920-1929
    if (1920 == input$check3){
      us_est <- us_1979[1521:1596, c(1,4,3)]
    }
    if (1921 == input$check3){
      us_est <- us_1979[1597:1672, c(1,4,3)]
    }
    if (1922 == input$check3){
      us_est <- us_1979[1673:1748, c(1,4,3)]
    }
    if (1923 == input$check3){
      us_est <- us_1979[1749:1824, c(1,4,3)]
    }
    if (1924 == input$check3){
      us_est <- us_1979[1825:1900, c(1,4,3)]
    }
    if (1925 == input$check3){
      us_est <- us_1979[1901:1976, c(1,4,3)]
    }
    if (1926 == input$check3){
      us_est <- us_1979[1977:2052, c(1,4,3)]
    }
    if (1927 == input$check3){
      us_est <- us_1979[2053:2128, c(1,4,3)]
    }
    if (1928 == input$check3){
      us_est <- us_1979[2129:2204, c(1,4,3)]
    }
    if (1929 == input$check3){
      us_est <- us_1979[2205:2280, c(1,4,3)]
    }
    #1930-1939
    if (1930 == input$check3){
      us_est <- us_1979[2281:2356, c(1,4,3)]
    }
    if (1931 == input$check3){
      us_est <- us_1979[2357:2432, c(1,4,3)]
    }
    if (1932 == input$check3){
      us_est <- us_1979[2433:2508, c(1,4,3)]
    }
    if (1933 == input$check3){
      us_est <- us_1979[2509:2584, c(1,4,3)]
    }
    if (1934 == input$check3){
      us_est <- us_1979[2585:2660, c(1,4,3)]
    }
    if (1935 == input$check3){
      us_est <- us_1979[2661:2736, c(1,4,3)]
    }
    if (1936 == input$check3){
      us_est <- us_1979[2737:2812, c(1,4,3)]
    }
    if (1937 == input$check3){
      us_est <- us_1979[2813:2888, c(1,4,3)]
    }
    if (1938 == input$check3){
      us_est <- us_1979[2889:2964, c(1,4,3)]
    }
    if (1939 == input$check3){
      us_est <- us_1979[2965:3040, c(1,4,3)]
    }
    #1940-1949
    if (1940 == input$check3){
      us_est <- us_1979[3041:3126, c(1,4,3)]
    }
    if (1941 == input$check3){
      us_est <- us_1979[3127:3212, c(1,4,3)]
    }
    if (1942 == input$check3){
      us_est <- us_1979[3213:3298, c(1,4,3)]
    }
    if (1943 == input$check3){
      us_est <- us_1979[3299:3384, c(1,4,3)]
    }
    if (1944 == input$check3){
      us_est <- us_1979[3385:3470, c(1,4,3)]
    }
    if (1945 == input$check3){
      us_est <- us_1979[3471:3556, c(1,4,3)]
    }
    if (1946 == input$check3){
      us_est <- us_1979[3557:3642, c(1,4,3)]
    }
    if (1947 == input$check3){
      us_est <- us_1979[3643:3728, c(1,4,3)]
    }
    if (1948 == input$check3){
      us_est <- us_1979[3729:3814, c(1,4,3)]
    }
    if (1949 == input$check3){
      us_est <- us_1979[3815:3900, c(1,4,3)]
    }
    #1950-1959
    if (1950 == input$check3){
      us_est <- us_1979[3901:3986, c(1,4,3)]
    }
    if (1951 == input$check3){
      us_est <- us_1979[3987:4072, c(1,4,3)]
    }
    if (1952 == input$check3){
      us_est <- us_1979[4073:4158, c(1,4,3)]
    }
    if (1953 == input$check3){
      us_est <- us_1979[4159:4244, c(1,4,3)]
    }
    if (1954 == input$check3){
      us_est <- us_1979[4245:4330, c(1,4,3)]
    }
    if (1955 == input$check3){
      us_est <- us_1979[4331:4416, c(1,4,3)]
    }
    if (1956 == input$check3){
      us_est <- us_1979[4417:4502, c(1,4,3)]
    }
    if (1957 == input$check3){
      us_est <- us_1979[4503:4588, c(1,4,3)]
    }
    if (1958 == input$check3){
      us_est <- us_1979[4589:4674, c(1,4,3)]
    }
    if (1959 == input$check3){
      us_est <- us_1979[4675:4760, c(1,4,3)]
    }
    #1960-1961
    if (1960 == input$check3){
      us_est <- us_1979[4761:4846, c(1,4,3)]
    }
    if (1961 == input$check3){
      us_est <- us_1979[4847:4932, c(1,4,3)]
    }
    if (1962 == input$check3){
      us_est <- us_1979[4933:5018, c(1,4,3)]
    }
    if (1963 == input$check3){
      us_est <- us_1979[5019:5104, c(1,4,3)]
    }
    if (1964 == input$check3){
      us_est <- us_1979[5105:5190, c(1,4,3)]
    }
    if (1965 == input$check3){
      us_est <- us_1979[5191:5276, c(1,4,3)]
    }
    if (1966 == input$check3){
      us_est <- us_1979[5277:5362, c(1,4,3)]
    }
    if (1967 == input$check3){
      us_est <- us_1979[5363:5448, c(1,4,3)]
    }
    if (1968 == input$check3){
      us_est <- us_1979[5449:5534, c(1,4,3)]
    }
    if (1969 == input$check3){
      us_est <- us_1979[5535:5620, c(1,4,3)]
    }
    #1970-1979
    if (1970 == input$check3){
      us_est <- us_1979[5621:5706, c(1,4,3)]
    }
    if (1971 == input$check3){
      us_est <- us_1979[5707:5792, c(1,4,3)]
    }
    if (1972 == input$check3){
      us_est <- us_1979[5793:5878, c(1,4,3)]
    }
    if (1973 == input$check3){
      us_est <- us_1979[5879:5964, c(1,4,3)]
    }
    if (1974 == input$check3){
      us_est <- us_1979[5965:6050, c(1,4,3)]
    }
    if (1975 == input$check3){
      us_est <- us_1979[6051:6136, c(1,4,3)]
    }
    if (1976 == input$check3){
      us_est <- us_1979[6137:6222, c(1,4,3)]
    }
    if (1977 == input$check3){
      us_est <- us_1979[6223:6308, c(1,4,3)]
    }
    if (1978 == input$check3){
      us_est <- us_1979[6309:6394, c(1,4,3)]
    }
    if (1979 == input$check3){
      us_est <- us_1979[6395:6480, c(1,4,3)]
    }
    #1980-1989
    if (1980 == input$check3){
      us_est <- us_1989[1:86, c(3, 5, 4)]
    }
    if (1981 == input$check3){
      us_est <- us_1989[87:172, c(3, 5, 4)]
    }
    if (1982 == input$check3){
      us_est <- us_1989[173:258, c(3, 5, 4)]
    }
    if (1983 == input$check3){
      us_est <- us_1989[259:344, c(3, 5, 4)]
    }
    if (1984 == input$check3){
      us_est <- us_1989[345:430, c(3, 5, 4)]
    }
    if (1985 == input$check3){
      us_est <- us_1989[431:516, c(3, 5, 4)]
    }
    if (1986 == input$check3){
      us_est <- us_1989[517:602, c(3, 5, 4)]
    }
    if (1987 == input$check3){
      us_est <- us_1989[603:688, c(3, 5, 4)]
    }
    if (1988 == input$check3){
      us_est <- us_1989[689:774, c(3, 5, 4)]
    }
    if (1989 == input$check3){
      us_est <- us_1989[775:860, c(3, 5, 4)]
    }
    #1990-1999
    if (1990 == input$check3){
      us_est <- us_1999[1:86, c(1, 4, 3)]
    }
    if (1991 == input$check3){
      us_est <- us_1999[87:172, c(1, 4, 3)]
    }
    if (1992 == input$check3){
      us_est <- us_1999[173:258, c(1, 4, 3)]
    }
    if (1993 == input$check3){
      us_est <- us_1999[259:344, c(1, 4, 3)]
    }
    if (1994 == input$check3){
      us_est <- us_1999[345:430, c(1, 4, 3)]
    }
    if (1995 == input$check3){
      us_est <- us_1999[431:516, c(1, 4, 3)]
    }
    if (1996 == input$check3){
      us_est <- us_1999[517:602, c(1, 4, 3)]
    }
    if (1997 == input$check3){
      us_est <- us_1999[603:688, c(1, 4, 3)]
    }
    if (1998 == input$check3){
      us_est <- us_1999[689:774, c(1, 4, 3)]
    }
    if (1999 == input$check3){
      us_est <- us_1999[775:860, c(1, 4, 3)]
    }
    #2000-2010
    if (2000 == input$check3){
      us_est <- us_2010[89:174,c(3, 6, 5)]
    }
    if (2001 == input$check3){
      us_est <- us_2010[176:261,c(3, 6, 5)]
    }
    if (2002 == input$check3){
      us_est <- us_2010[263:348,c(3, 6, 5)]
    }
    if (2003 == input$check3){
      us_est <- us_2010[350:435,c(3, 6, 5)]
    }
    if (2004 == input$check3){
      us_est <- us_2010[437:522,c(3, 6, 5)]
    }
    if (2005 == input$check3){
      us_est <- us_2010[525:609,c(3, 6, 5)]
    }
    if (2006 == input$check3){
      us_est <- us_2010[611:696,c(3, 6, 5)]
    }
    if (2007 == input$check3){
      us_est <- us_2010[698:783,c(3, 6, 5)]
    }
    if (2008 == input$check3){
      us_est <- us_2010[785:870,c(3, 6, 5)]
    }
    if (2009 == input$check3){
      us_est <- us_2010[872:957,c(3, 6, 5)]
    }
    if (2010 == input$check3){
      us_est <- us_2010[1046:1131,c(3, 6, 5)]
    }
    
    yearshow1 <- paste(input$check3,"Year",sep="")
    names(us_est) <- c("Age", "Female", "Male")
    us_est$Male <- -1 * us_est$Male
    us_est$Age <- factor(us_est$Age, levels = us_est$Age, labels = us_est$Age)
    df_1 <- melt(us_est, 
                 value.name='Population', 
                 variable.name = 'Gender', 
                 id.vars='Age' )
    my_theme <- theme(axis.text.x  = element_text(face="bold",angle=45, size=50), 
                      axis.text.y  = element_text(face="bold",angle=45, size=50))
    ggplot(df_1, mapping = aes(x = Age, y = Population, fill = Gender)) + 
      geom_bar(subset = .(Gender == "Male"), stat = "identity") + 
      geom_bar(subset = .(Gender == "Female"), stat = "identity") +
      scale_y_continuous(breaks = seq(-5000000, 5000000, 500000), 
                         labels = paste0(as.character(c(seq(5, 0, -0.5), seq(0.5, 5, 0.5))), "million" )) +
      scale_x_discrete(breaks = c("0","5","10","15","20","25","30","35","40","45","50","55","60","65","70","75","80","85","90","95","100"), 
                       labels = c("0","5","10","15","20","25","30","35","40","45","50","55","60","65","70","75+","80","85+","90","95","100+")) +
      my_theme +
      coord_flip() + 
      scale_fill_manual(values=c("#CACA3B", "#487C44")) +
      theme_bw(base_size = 20) +
      theme(legend.position = 'bottom')+
      annotate(geom="text", x=73, y=0, label=yearshow1,
               color="black")
  })
  
  #intercensal_us:
  output$intercensal_uk <- renderPlot({
    if (1991 == input$check6){
      uk_est <- uk_inter[1:85, c(1,3,2)]
    }
    if (1992 == input$check6){
      uk_est <- uk_inter[1:85, c(1,5,4)]
    }
    if (1993 == input$check6){
      uk_est <- uk_inter[1:85, c(1,7,6)]
    }
    if (1994 == input$check6){
      uk_est <- uk_inter[1:85, c(1,9,8)]
    }
    if (1995 == input$check6){
      uk_est <- uk_inter[1:85, c(1,11,10)]
    }
    if (1996 == input$check6){
      uk_est <- uk_inter[1:85, c(1,13,12)]
    }
    if (1997 == input$check6){
      uk_est <- uk_inter[1:85, c(1,15,14)]
    }
    if (1998 == input$check6){
      uk_est <- uk_inter[1:85, c(1,17,16)]
    }
    if (1999 == input$check6){
      uk_est <- uk_inter[1:85, c(1,19,18)]
    }
    if (2000 == input$check6){
      uk_est <- uk_inter[1:90, c(1,21,20)]
    }
    if (2001 == input$check6){
      uk_est <- uk_inter[1:90, c(1,23,22)]
    }
    if (2002 == input$check6){
      uk_est <- uk_inter[1:90, c(1,25,24)]
    }
    if (2003 == input$check6){
      uk_est <- uk_inter[1:90, c(1,27,26)]
    }
    if (2004 == input$check6){
      uk_est <- uk_inter[1:90, c(1,29,28)]
    }
    if (2005 == input$check6){
      uk_est <- uk_inter[1:90, c(1,31,30)]
    }
    if (2006 == input$check6){
      uk_est <- uk_inter[1:90, c(1,33,32)]
    }
    if (2007 == input$check6){
      uk_est <- uk_inter[1:90, c(1,35,34)]
    }
    if (2008 == input$check6){
      uk_est <- uk_inter[1:90, c(1,37,36)]
    }
    if (2009 == input$check6){
      uk_est <- uk_inter[1:90, c(1,39,38)]
    }
    if (2010 == input$check6){
      uk_est <- uk_inter[1:90, c(1,41,40)]
    }
    if (2011 == input$check6){
      uk_est <- uk_inter[1:90, c(1,43,42)]
    }
    if (2012 == input$check6){
      uk_est <- uk_inter[1:90, c(1,45,44)]
    }
    if (2013 == input$check6){
      uk_est <- uk_inter[1:90, c(1,47,46)]
    }
    if (2014 == input$check6){
      uk_est <- uk_inter[1:90, c(1,49,48)]
    }
    if (2015 == input$check6){
      uk_est <- uk_inter[1:90, c(1,51,50)]
    }
    if (2016 == input$check6){
      uk_est <- uk_inter[1:90, c(1,53,52)]
    }
    if (2017 == input$check6){
      uk_est <- uk_inter[1:90, c(1,55,54)]
    }
    if (2018 == input$check6){
      uk_est <- uk_inter[1:90, c(1,57,56)]
    }
    yearshow2 <- paste(input$check6,"Year",sep="")
    names(uk_est) <- c("Age", "Female", "Male")
    uk_est$Male <- -1 * uk_est$Male
    uk_est$Age <- factor(uk_est$Age, levels = uk_est$Age, labels = uk_est$Age)
    df_1 <- melt(uk_est, 
                 value.name='Population', 
                 variable.name = 'Gender', 
                 id.vars='Age' )
    my_theme <- theme(axis.text.x  = element_text(face="bold",angle=45, size=50), 
                      axis.text.y  = element_text(face="bold",angle=45, size=50))
    ggplot(df_1, mapping = aes(x = Age, y = Population, fill = Gender)) + 
      geom_bar(subset = .(Gender == "Male"), stat = "identity") + 
      geom_bar(subset = .(Gender == "Female"), stat = "identity") +
      scale_y_continuous(breaks = seq(-5000000, 5000000, 500000), 
                         labels = paste0(as.character(c(seq(5, 0, -0.5), seq(0.5, 5, 0.5))), "million" )) +
      scale_x_discrete(breaks = c("0","5","10","15","20","25","30","35","40","45","50","55","60","65","70","75","80","85","90"), 
                       labels = c("0","5","10","15","20","25","30","35","40","45","50","55","60","65","70","75","80","85+","90+")) +
      my_theme +
      coord_flip() + 
      scale_fill_manual(values=c("#F38770", "#5E95A8")) +
      theme_bw(base_size = 20) +
      annotate(geom="text", x=83, y=0, label=yearshow2,
               color="black")+
      theme(legend.position = 'bottom')
  })
  
  ############>>>>Fecundity rate tab:
  observeEvent(input$selectAll_f,{
    #if(input$selectAll_f == 0) return(NULL) 
    #else 
    if ((input$selectAll_f%%2) == 0) {
      updateButton(session, "selectAll_f", label = "Select All")
      updateCheckboxGroupInput(session,"check7", choices=c("United Kingdom", "United States", "China"))
    }
    else{
      updateButton(session, "selectAll_f", label="Unselect")
      updateCheckboxGroupInput(session,"check7",choices=c("United Kingdom", "United States", "China"), 
                               selected = c("United Kingdom", "United States", "China"))
    }
    
  })
  
  output$lineChart_2 <- renderPlot({  
  longData2 <- cbind(rep(fertility$age, 3), stack(fertility[,2:4]))
  names(longData2) <- c("age", "fRate", "grp")
  longData2 <- 
    transform(longData2, color = ifelse(
        grp == "uk",
        '#F38770',
          ifelse(
            grp == "cn",
            '#CACA3B',
              ifelse(
                grp == "us", 
                '#FEA54C',
                NA
              )
      )
    ))
  
  longData2$grp <- 
    plyr::revalue(
      longData2$grp,
      c(
        "uk" = "United Kingdom",
        "us" = "United States",
        "cn" = "China"
      )
    )
  
  subLData2 <- dplyr::filter(longData2, longData2$grp %in% input$check7)
  ggplot(data=na.omit(subLData2), aes(x=age, y=fRate, color=grp)) +
    geom_path(lwd = 1) +
    scale_y_continuous(expand=expand_scale(mult = 0, add = 0), limits = c(0,120)) +
    theme(panel.background = element_rect(fill = 'white', color = 'black'),
          text = element_text(size = 14),
          plot.title = element_text(size = 14),
          axis.text = element_text(size = 14),
          legend.position = c(0.85,0.80),
          legend.title = element_blank()) +
    labs(title = "Fecundity Rate (per 1,000 women) for comparison",
         y = "Number of Birth per 1,000 Women",
         x = "Age(yrs)") +
    scale_color_manual(breaks = longData2$grp, values = unique(as.character(longData2$color)))
  })
  
  
  #reference:
  observeEvent(input$ref1, {
    if ((input$ref1 %% 2) == 0) {
      output$refer1 <- renderText("")
      updateButton(session=session, inputId = "ref1", label = "Show Reference")
    }
    else {
      updateButton(session=session, inputId = "ref1", label = "Hide Reference")
      output$refer1 <- renderText({
        paste(
          h5("Dataset based on 2015:"),
          h5(tags$a(href="https://www.cdc.gov/nchs/data/nvsr/nvsr67/nvsr67_07-508.pdf", "United States Click here")),
          h5(tags$a(href="https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables", "United Kingdom Click here")),
          h5(tags$a(href="http://bxjg.circ.gov.cn/web/site0/tab5216/info4054990.htm", "China Click here (data based on populaiton with Social Security Retirement Benefits)")))
      })
    }
  })
  observeEvent(input$ref2, {
    if ((input$ref2 %% 2) == 0) {
      output$refer2 <- renderText("")
      updateButton(session=session, inputId = "ref2", label = "Show Reference")
    }
    else {
      updateButton(session=session, inputId = "ref2", label = "Hide Reference")
      output$refer2 <- renderText({
        paste(
          h5("Dataset based on 2015:"),
          h5(tags$a(href="https://www.cdc.gov/nchs/data/nvsr/nvsr67/nvsr67_07-508.pdf", "United States Click here")),
          h5(tags$a(href="https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables", "United Kingdom Click here")),
          h5(tags$a(href="http://bxjg.circ.gov.cn/web/site0/tab5216/info4054990.htm", "China Click here (data based on populaiton with Social Security Retirement Benefits)")))
      })
    }
  })
  observeEvent(input$ref3, {
    if ((input$ref3 %% 2) == 0) {
      output$refer3 <- renderText("")
      updateButton(session=session, inputId = "ref3", label = "Show Reference")
    }
    else {
      updateButton(session=session, inputId = "ref3", label = "Hide Reference")
      output$refer3 <- renderText({
        paste(
          h5("Dataset based on 2015:"),
          h5(tags$a(href="https://www.cdc.gov/nchs/data/nvsr/nvsr67/nvsr67_07-508.pdf", "United States Click here")),
          h5(tags$a(href="https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables", "United Kingdom Click here")),
          h5(tags$a(href="http://bxjg.circ.gov.cn/web/site0/tab5216/info4054990.htm", "China Click here (data based on populaiton with Social Security Retirement Benefits)")))
      })
    }
  })
  observeEvent(input$ref4, {
    if ((input$ref4 %% 2) == 0) {
      output$refer4 <- renderText("")
      updateButton(session=session, inputId = "ref4", label = "Show Reference")
    }
    else {
      updateButton(session=session, inputId = "ref4", label = "Hide Reference")
      output$refer4 <- renderText({
        paste(
          h5("Dataset based on 2015:"),
          h5(tags$a(href="https://www.cdc.gov/nchs/data/nvsr/nvsr67/nvsr67_07-508.pdf", "United States Click here")),
          h5(tags$a(href="https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables", "United Kingdom Click here")),
          h5(tags$a(href="http://bxjg.circ.gov.cn/web/site0/tab5216/info4054990.htm", "China Click here (data based on populaiton with Social Security Retirement Benefits)")))
      })
    }
  })
  observeEvent(input$ref5, {
    if ((input$ref5 %% 2) == 0) {
      output$refer5 <- renderText("")
      updateButton(session=session, inputId = "ref5", label = "Show Reference")
    }
    else {
      updateButton(session=session, inputId = "ref5", label = "Hide Reference")
      output$refer5 <- renderText({
        paste(
          h5("Dataset based on 2015:"),
          h5(tags$a(href="https://www.cdc.gov/nchs/data/nvsr/nvsr67/nvsr67_07-508.pdf", "United States Click here")),
          h5(tags$a(href="https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables", "United Kingdom Click here")),
          h5(tags$a(href="http://bxjg.circ.gov.cn/web/site0/tab5216/info4054990.htm", "China Click here (data based on populaiton with Social Security Retirement Benefits)")))
      })
    }
  })
  observeEvent(input$ref6, {
    if ((input$ref6 %% 2) == 0) {
      output$refer6 <- renderText("")
      updateButton(session=session, inputId = "ref6", label = "Show Reference")
    }
    else {
      updateButton(session=session, inputId = "ref6", label = "Hide Reference")
      output$refer6 <- renderText({
        paste(
          h5("Dataset based on 2011:"),
          h5(tags$a(href="https://www.cdc.gov/nchs/data/nvsr/nvsr66/nvsr66_01.pdf", "United States Click here")),
          h5(tags$a(href="https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/adhocs/005806totalfertilityratestfruk1985to2014", "United Kingdom Click here")),
          h5(tags$a(href="http://data.stats.gov.cn/easyquery.htm?cn=C01&zb=A03060H&sj=2018", "China Click here (data based on populaiton with Social Security Retirement Benefits)")))
      })
    }
  })
}
