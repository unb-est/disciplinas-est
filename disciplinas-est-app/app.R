# Pacotes -----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(reshape2)
library(ggpubr)
library(tm)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(scales)

# Dados -------------------------------------------------------------------

historico <- read_rds("historico_limpo.rds")

h <- historico %>% separate(periodo, into = c("p1", "p2"), sep = "/", convert = TRUE) %>%
  unite(periodo, p1, p2, sep = "")

h$periodo <- as.numeric(h$periodo)

for (i in 1:length(h$periodo)) {
  
  if (h$periodo[i] %% 2 == 0 & h$periodo[i] %% 10 != 0) {
    h$periodo[i] <- (h$periodo[i] + 3)
  }
}

h$periodo <- h$periodo/10

bacharelado <- h %>% filter(tipo == "Bacharelado")
servico <- h %>% filter(tipo == "Serviço")


conditional <- function(condition, success) {
  if (condition) success else TRUE
}

professores_ativos <- c('ALAN RICARDO DA SILVA', 'ANA MARIA NOGALES VASCONCELOS', 'ANDRE LUIZ FERNANDES CANCADO',
                        'ANTONIO EDUARDO GOMES', 'BERNARDO BORBA DE ANDRADE', 'BERNARDO NOGUEIRA SCHLEMPER',
                        'CIBELE QUEIROZ DA SILVA', 'CIRA ETHEOWALDA GUEVARA OTINIANO', 'CLAUDETE RUAS',
                        'DEMERSON ANDRE POLLI', 'DONALD MATTHEW PIANTO', 'EDUARDO FREITAS DA SILVA',
                        'EDUARDO MONTEIRO DE CASTRO GOMES', 'EDUARDO YOSHIO NAKANO', 'GEORGE FREITAS VON BORRIES',
                        'GERALDO DA SILVA E SOUZA', 'GLADSTON LUIZ DA SILVA', 'GUILHERME SOUZA RODRIGUES',
                        'GUSTAVO LEONEL GILARDONI AVALLE', 'HELTON SAULO BEZERRA DOS SANTOS', 'ISRAEL DE FREITAS MADUREIRA',
                        'JHAMES MATOS SAMPAIO', 'JOANLISE MARCO DE LEON ANDRADE', 'JOSE ANGELO BELLONI',
                        'JOSE AUGUSTO FIORUCCI', 'JULIANA BETINI FACHINI GOMES', 'LEANDRO TAVARES CORREIA',
                        'LUCAS MOREIRA', 'LUIS GUSTAVO DO AMARAL VINHA', 'MARIA TERESA LEAO COSTA', 'PETER ZORNIG', 
                        'RAUL YUKIHIRO MATSUSHITA', 'ROBERTO VILA GABRIEL', 'THAIS CARVALHO VALADARES RODRIGUES')

professores_dout <-  data.frame(Professores=c('ALAN RICARDO DA SILVA', 'ANA MARIA NOGALES VASCONCELOS', 'ANDRE LUIZ FERNANDES CANCADO',
                                              'ANTONIO EDUARDO GOMES', 'BERNARDO BORBA DE ANDRADE', 
                                              'CIBELE QUEIROZ DA SILVA', 'CIRA ETHEOWALDA GUEVARA OTINIANO', 
                                              'DEMERSON ANDRE POLLI', 'DONALD MATTHEW PIANTO', 'EDUARDO FREITAS DA SILVA',
                                              'EDUARDO MONTEIRO DE CASTRO GOMES', 'EDUARDO YOSHIO NAKANO', 'GEORGE FREITAS VON BORRIES',
                                              'GERALDO DA SILVA E SOUZA', 'GLADSTON LUIZ DA SILVA', 'GUILHERME SOUZA RODRIGUES',
                                              'GUSTAVO LEONEL GILARDONI AVALLE', 'HELTON SAULO BEZERRA DOS SANTOS', 
                                              'JHAMES MATOS SAMPAIO', 'JOANLISE MARCO DE LEON ANDRADE', 'JOSE ANGELO BELLONI',
                                              'JOSE AUGUSTO FIORUCCI', 'JULIANA BETINI FACHINI GOMES', 'LEANDRO TAVARES CORREIA',
                                              'LUCAS MOREIRA', 'LUIS GUSTAVO DO AMARAL VINHA', 'PETER ZORNIG', 
                                              'RAUL YUKIHIRO MATSUSHITA', 'ROBERTO VILA GABRIEL', 'THAIS CARVALHO VALADARES RODRIGUES'),
                                Ano_dout=c(2009,2001,2009,1999,2007,1999,2006,2020,2008,1998,2013,2010,2008,1979,2010,2017,1989,2013,2012,2008,2000,2016,2011,2015,2012,2016,1988,2012,2016,2017))

periodo<-levels(historico$periodo)[grep("/[1 2]",levels(historico$periodo))]

# App ---------------------------------------------------------------------

sidebar<-dashboardSidebar(
  sidebarMenu(id="teste",
              menuItem("Estatística", tabName = "geral", icon = icon("university")),
              menuItem("Disciplinas de Serviço", tabName = "serviço", icon = icon("handshake")),
              menuItem("Disciplinas do Bacharelado", tabName = "bacharelado", icon = icon("chart-line")),
              menuItem("Professores", tabName = "prof", icon = icon("address-card"))
  ),
  conditionalPanel(
    condition = "input.teste == 'geral'",
    selectInput("period", "Semestre:", 
                choices = periodo,
                selected = periodo[length(periodo)])
  )
  
)

header<-dashboardHeader(title = "Disciplinas da EST")

body<-dashboardBody(
  
  # Página Inicial ----------------------------------------------------------
  
  tabItems(
    tabItem(tabName = "geral",
            fluidRow(
              column(width = 3,
                     infoBoxOutput("alunos", width = 12),
                     infoBoxOutput("disciplinas", width = 12),
                     infoBoxOutput("professores_ativ", width = 12),
                     infoBoxOutput("aprovacoes_geral", width = 12),
                     infoBoxOutput("reprovacoes_geral", width = 12)                     
              ),
              box(width = 4,
                  title = "Tipo de professor",
                  plotlyOutput("professores_ativ2")),
              box(width = 4,
                  title = "Tipo de disciplina",
                  plotlyOutput("disciplinas_bach"))
            )            
    ),
    
    
    # Disciplinas de Serviço --------------------------------------------------
    
    tabItem(tabName = "serviço",
            
            fluidRow(
              column(width = 3,
                     
                     box(title="Parâmetros",status = "warning",solidHeader = T,width=NULL,
                         uiOutput("serv_periodo"),
                         uiOutput("serv_disc"),
                         uiOutput("curso"),
                         uiOutput("serv_professor"),
                         uiOutput("serv_horario"),
                         uiOutput("serv_turma")
                     ),
                     
                     
                     #################################################
                     infoBoxOutput("serv_aprovacoes", width = 12),
                     infoBoxOutput("serv_reprovacoes", width = 12)
              ),
              box(width = 9,
                  title = "Menções",
                  plotlyOutput("serv_mencoes", height = "665px")),
              box(width = 12,
                  title = "Proporção de Aprovação, Reprovação e Trancamentos",
                  plotlyOutput("serv_resultados")  
              )
            )             
    ),
    
    # Bacharelado -------------------------------------------------------------
    
    tabItem(tabName = "bacharelado",
            fluidRow(
              column(width = 3,
                     
                     box(title="Parâmetros",status = "warning",solidHeader = T,width=NULL,
                         uiOutput("bach_periodo"),
                         uiOutput("bach_disc"),
                         uiOutput("bach_professor"),
                         uiOutput("bach_horario"),
                         uiOutput("bach_turma")
                     ),
                     
                     infoBoxOutput("bach_aprovacoes", width = 12),
                     infoBoxOutput("bach_reprovacoes", width = 12)
              ),
              box(width = 9,
                  title = "Menções",
                  plotlyOutput("bach_mencoes", height = "800px")),
              box(width = 12,
                  title = "Proporção de Aprovação, Reprovação e Trancamentos",
                  plotlyOutput("bach_resultados")  
              )
            )
    )
  )
)

ui<-dashboardPage(sidebar = sidebar,header = header,body= body)

server <- function(input, output) {
  
  output$alunos <- renderInfoBox({
    infoBox(
      "Alunos", nrow(historico %>% filter(curso=="Estatística",periodo==input$period) %>%
                       summarise(unique(matricula))) ,
      color = "purple", fill = TRUE,icon=icon("user-friends")
    )
  })
  
  prof_ativ<-reactive({
    prof_ativ<-historico %>% filter(periodo==input$period) %>%
      summarise(professor=unique(professor))
    prof_ativ
  })
  
  output$professores_ativ <- renderInfoBox({
    infoBox(
      "Prof. ativos", nrow(prof_ativ()) ,
      color = "yellow", fill = TRUE,icon=icon("chalkboard-teacher")
    )
  })
  
  prof_dout<-reactive({
    prof_dout<-merge(prof_ativ(),
                     professores_dout,by.x="professor",by.y="Professores",all.x=T) %>%
      mutate(Ano_dout=if_else(Ano_dout<=as.numeric(str_sub(factor(input$period), end = 4)),1,Ano_dout),
             Ano_dout=if_else(Ano_dout>as.numeric(str_sub(factor(input$period), end = 4)),0,Ano_dout),
             Ano_dout=replace_na(Ano_dout, 0),
             Ano_dout=as.character(Ano_dout),
             Ano_dout=replace(Ano_dout, Ano_dout=="1", "Doutor"),
             Ano_dout=replace(Ano_dout, Ano_dout=="0", "Professor")) %>% count(Ano_dout)
    prof_dout
  })
  
  output$professores_ativ2<-renderPlotly({
    
    prof_dout()%>%
      plot_ly(labels = ~factor(Ano_dout), values = ~n,
              sort = FALSE, marker = list(colors = c("rgb(107, 0, 59)",
                                                     "rgb(100, 0, 110)")))%>%
      add_pie(hole = 0.6) %>% layout(showlegend = T,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  count_disc<-reactive({
    count_disc<-unique(historico %>% filter(periodo==input$period) %>% select(disciplina,tipo))
    count_disc
    
  })
  
  output$disciplinas <- renderInfoBox({
    infoBox(
      "Disciplinas", nrow(count_disc())  ,
      color = "navy", fill = FALSE,icon=icon("book-reader")
    )
  })
  
  output$disciplinas_bach <- renderPlotly({
    
    count_disc() %>% count(tipo) %>%
      plot_ly(labels = ~factor(tipo), values = ~n,
              sort = FALSE, marker = list(colors = c("rgb(3, 177, 143)","rgb(3, 202, 177)")))%>%
      add_pie(hole = 0.6) %>% layout(showlegend = T,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  # Serviço -----------------------------------------------------------------
  
  
  ###### reactive
  
  output$serv_periodo <- renderUI({
    
    sliderTextInput("serv_periodo", "Filtre pelo(s) período(s):", 
                    choices = sort(unique(servico$periodo)),
                    selected = c((min(servico$periodo)), (max(servico$periodo)))
    )
    
  })
  
  data0 <- reactive({
    data <- servico %>% filter(periodo <= input$serv_periodo[2] & periodo >= input$serv_periodo[1])
    data
  })
  
  output$serv_disc <- renderUI({
    
    selectInput('serv_disc', 'Filtre pela(s) disciplina(s):', 
                choices = sort(unique(data0()$disciplina)),
                selected = "None",
                multiple = TRUE
    )
    
  })
  
  data1 <- reactive({
    if (length(input$serv_disc) == 0){
      data <- data0()
    }
    else{
      data <- data0() %>% filter(disciplina %in% input$serv_disc)
    }
    data
  })
  
  output$curso <- renderUI({
    
    selectInput('curso', 'Filtre pelo(s) curso(s):', 
                choices = sort(unique(data1()$curso)),
                selected = "None", 
                multiple = TRUE
    )
    
  })
  
  data2 <- reactive({
    if ((length(input$curso)  == 0)){
      data <- data1()
    }
    else{
      data <- data1() %>% filter(curso %in% input$curso)
    }
    data
  })
  
  output$serv_professor <- renderUI({
    
    selectInput('serv_professor', 'Filtre pelo(s) professor(es):', 
                choices = sort(unique(data2()$professor)),
                selected = "None",
                multiple = TRUE
    )
    
  })
  
  data3 <- reactive({
    if ((length(input$serv_professor) == 0)){
      data <- data2()
    }
    else{
      data <- data2() %>% filter(professor %in% input$serv_professor)
    }
    data
  })
  
  output$serv_horario <- renderUI({
    
    selectInput("serv_horario", "Filtre pelo(s) horário(s):", 
                choices = sort(unique(data3()$horario)),
                selected = "None",
                multiple = TRUE
    )
    
  })
  
  data4 <- reactive({
    if ((length(input$serv_horario) == 0)){
      data <- data3()
    }
    else{
      data <- data3() %>% filter(horario %in% input$serv_horario)
    }
    data
  })
  
  output$serv_turma <- renderUI({
    
    selectInput("serv_turma", "Filtre pela(s) turma(s):", 
                choices = sort(as.character(unique(data4()$turma))),
                selected = "None",
                multiple = TRUE
    )
    
  })
  
  data5 <- reactive({
    if ((length(input$serv_turma) == 0)){
      data <- data4()
    }
    else{
      data <- data4() %>% filter(turma %in% input$serv_turma)
    }
    data
  })
  
  ########
  
  taxa_aprovacao <- reactive({
    (historico %>% filter(tipo=="Bacharelado",periodo==input$period) %>% 
       count(resultado) %>% 
       summarise(prop = n/sum(n), resultado = resultado) %>% 
       filter(resultado == "Aprovação"))$prop
  })
  
  taxa_reprovacao <- reactive({
    (historico %>% filter(tipo=="Bacharelado",periodo==input$period) %>% 
       count(resultado) %>% 
       summarise(prop = n/sum(n), resultado = resultado) %>% 
       filter(resultado == "Reprovação"))$prop
  })  
  
  
  output$aprovacoes_geral <- renderInfoBox({
    infoBox(
      "Aprovação", label_percent(accuracy = 0.1, decimal.mark = ",")(taxa_aprovacao()) ,
      color = "aqua", fill = TRUE,icon=icon("check")
    )
  })
  
  output$reprovacoes_geral <- renderInfoBox({
    infoBox(
      "Reprovação", label_percent(accuracy = 0.1, decimal.mark = ",")(taxa_reprovacao()) ,
      color = "red", fill = TRUE,icon=icon("times")
    )
  })
  
  serv_filtrado <- reactive({
    data5() %>%
      filter(
        conditional(!is.null(input$serv_disc), disciplina %in% input$serv_disc),
        conditional(TRUE, periodo >= input$serv_periodo[1] & periodo <= input$serv_periodo[2]),
        conditional(!is.null(input$curso), curso %in% input$curso),
        conditional(!is.null(input$serv_professor), professor %in% input$serv_professor),
        conditional(!is.null(input$serv_horario), horario %in% input$serv_horario),
        conditional(!is.null(input$serv_turma), turma %in% input$serv_turma)
      )
  })
  
  serv_taxa_aprovacao <- reactive({
    (serv_filtrado() %>% 
       count(resultado) %>% 
       summarise(prop = n/sum(n), resultado = resultado) %>% 
       filter(resultado == "Aprovação"))$prop
  })
  
  serv_taxa_reprovacao <- reactive({
    (serv_filtrado() %>% 
       count(resultado) %>% 
       summarise(prop = n/sum(n), resultado = resultado) %>% 
       filter(resultado == "Reprovação"))$prop
  })
  
  output$serv_aprovacoes <- renderInfoBox({
    infoBox(
      "Aprovação média",  label_percent(accuracy = 0.1, decimal.mark = ",")(serv_taxa_aprovacao()),
      color = "aqua", fill = TRUE,icon=icon("check")
    )
  })
  
  output$serv_reprovacoes <- renderInfoBox({
    infoBox(
      "Reprovação média", label_percent(accuracy = 0.1, decimal.mark = ",")(serv_taxa_reprovacao()),
      color = "red", fill = TRUE,icon=icon("times")
    )
  })
  
  
  output$serv_mencoes <- renderPlotly({
    p <- serv_filtrado() %>% 
      filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
      mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("SR", "II", "MI")))) %>% 
      ggplot() + 
      geom_bar(aes(x = fct_rev(disciplina), fill = fct_rev(mencao), text = paste0("Proporção: ", round((..count../tapply(..count.., ..x.. ,sum)[..x..]), 3))), position = "fill") + 
      labs(x="", y="Proporção", fill = "") +
      scale_fill_brewer(palette = "RdBu", direction = -1) + 
      coord_flip()
    
    ggplotly(p , tooltip = "text")
  })
  
  output$serv_resultados <-renderPlotly({
    p <- serv_filtrado() %>% 
      group_by(periodo, resultado) %>% summarise(n = n()) %>% 
      summarise(prop = n/sum(n), resultado = resultado) %>% 
      complete(resultado, fill = list(prop = 0)) %>% 
      ggplot(aes(x = periodo, y = prop, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")), text = paste0('Período: ', periodo, '\n', 'Proporção: ', round(prop, 5)))) + 
      geom_line() + 
      labs(x="Período", y="Proporção") +
      scale_x_discrete(limits = c(seq(1993, 2020, 1))) +
      scale_colour_manual(name="", values = c("#00A4CD", "#F08080", "yellow")) + 
      scale_y_continuous(labels = scales::label_percent()) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
  })
  
  
  # Bacharelado -------------------------------------------------------------
  
  ###### reactive
  
  output$bach_periodo <- renderUI({
    
    sliderTextInput("bach_periodo", "Filtre pelo(s) período(s):", 
                    choices = sort(unique(bacharelado$periodo)),
                    selected = c((min(bacharelado$periodo)), (max(bacharelado$periodo)))
    )
    
  })
  
  data0_ <- reactive({
    data <- bacharelado %>% filter(periodo <= input$bach_periodo[2] & periodo >= input$bach_periodo[1])
    data
  })
  
  output$bach_disc <- renderUI({
    
    selectInput('bach_disc', 'Filtre pela(s) disciplina(s):', 
                choices = sort(unique(data0_()$disciplina)),
                selected = "None",
                multiple = TRUE
    )
    
  })
  
  data1_ <- reactive({
    if (length(input$bach_disc) == 0){
      data <- data0_()
    }
    else{
      data <- data0_() %>% filter(disciplina %in% input$bach_disc)
    }
    data
  })
  
  
  output$bach_professor <- renderUI({
    
    selectInput('bach_professor', 'Filtre pelo(s) professor(es):', 
                choices = sort(unique(data1_()$professor)),
                selected = "None",
                multiple = TRUE
    )
    
  })
  
  data2_ <- reactive({
    if ((length(input$bach_professor) == 0)){
      data <- data1_()
    }
    else{
      data <- data1_() %>% filter(professor %in% input$bach_professor)
    }
    data
  })
  
  output$bach_horario <- renderUI({
    
    selectInput("bach_horario", "Filtre pelo(s) horário(s):", 
                choices = sort(unique(data2_()$horario)),
                selected = "None",
                multiple = TRUE
    )
    
  })
  
  data3_ <- reactive({
    if ((length(input$bach_horario) == 0)){
      data <- data2_()
    }
    else{
      data <- data2_() %>% filter(horario %in% input$bach_horario)
    }
    data
  })
  
  output$bach_turma <- renderUI({
    
    selectInput("bach_turma", "Filtre pela(s) turma(s):", 
                choices = sort(as.character(unique(data3_()$turma))),
                selected = "None",
                multiple = TRUE
    )
    
  })
  
  data4_ <- reactive({
    if ((length(input$bach_turma) == 0)){
      data <- data3_()
    }
    else{
      data <- data3_() %>% filter(turma %in% input$bach_turma)
    }
    data
  })
  ################
  
  bach_filtrado <- reactive({
    data4_() %>%
      filter(
        conditional(!is.null(input$bach_disc), disciplina %in% input$bach_disc),
        conditional(TRUE, periodo >= input$bach_periodo[1] & periodo <= input$bach_periodo[2]),
        conditional(!is.null(input$bach_professor), professor %in% input$bach_professor),
        conditional(!is.null(input$bach_horario), horario %in% input$bach_horario),
        conditional(!is.null(input$bach_turma), turma %in% input$bach_turma)
      )
  })
  
  bach_taxa_aprovacao <- reactive({
    (bach_filtrado() %>% 
       count(resultado) %>% 
       summarise(prop = n/sum(n), resultado = resultado) %>% 
       filter(resultado == "Aprovação"))$prop
  })
  
  bach_taxa_reprovacao <- reactive({
    (bach_filtrado() %>% 
       count(resultado) %>% 
       summarise(prop = n/sum(n), resultado = resultado) %>% 
       filter(resultado == "Reprovação"))$prop
  })
  
  output$bach_aprovacoes <- renderInfoBox({
    infoBox(
      "Aprovação",  label_percent(accuracy = 0.1, decimal.mark = ",")(bach_taxa_aprovacao()),
      color = "aqua", fill = TRUE,icon=icon("check")
    )
  })
  
  output$bach_reprovacoes <- renderInfoBox({
    infoBox(
      "Reprovação", label_percent(accuracy = 0.1, decimal.mark = ",")(bach_taxa_reprovacao()),
      color = "red", fill = TRUE,icon=icon("times")
    )
  })
  
  
  output$bach_mencoes <- renderPlotly({
    p <- bach_filtrado() %>% 
      filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
      mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("SR", "II", "MI")))) %>% 
      ggplot() + 
      geom_bar(aes(x = fct_rev(disciplina), fill = fct_rev(mencao), text = paste0("Proporção: ", round((..count../tapply(..count.., ..x.. ,sum)[..x..]), 3))), position = "fill") + 
      labs(x="", y="Proporção", fill = "") +
      scale_fill_brewer(palette = "RdBu", direction = -1) + 
      coord_flip()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$bach_resultados <-renderPlotly({
    p <- bach_filtrado() %>% 
      group_by(periodo, resultado) %>% summarise(n = n()) %>% 
      summarise(prop = n/sum(n), resultado = resultado) %>% 
      complete(resultado, fill = list(prop = 0)) %>% 
      ggplot(aes(x = periodo, y = prop, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")), text = paste0('Período: ', periodo, '\n', 'Proporção: ', round(prop, 5)))) + 
      geom_line() + 
      labs(x="Período", y="Proporção") +
      scale_x_discrete(limits = c(seq(1993, 2020, 1))) +
      scale_colour_manual(name="", values = c("#00A4CD", "#F08080", "yellow")) + 
      scale_y_continuous(labels = scales::label_percent()) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
  })
}

shinyApp(ui, server)