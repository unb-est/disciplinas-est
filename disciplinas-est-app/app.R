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
library(feather)

# Dados -------------------------------------------------------------------

historico <- read_rds("historico_limpo.rds")
formandos <- read_feather("formandos")
professores_dout <- read_feather("professores_dout")

bacharelado <- historico %>% filter(tipo == "Bacharelado")
servico <- historico %>% filter(tipo == "Serviço")

periodo<-levels(historico$periodo)

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



# UI - Barra Lateral ------------------------------------------------------

sidebar<-dashboardSidebar(
    sidebarMenu(id="est",
        menuItem("Estatística", tabName = "geral", icon = icon("university")),
        conditionalPanel(
          condition = "input.est == 'geral'",
          selectInput("period", "Período:", 
                      choices = c(as.character(sort(unique(historico$periodo))), "Todos"),
                      selected = "Todos")
        ),
        menuItem("Disciplinas do Bacharelado", tabName = "bacharelado", icon = icon("chart-line")),
        conditionalPanel(
          condition = "input.est == 'bacharelado'",
          switchInput(inputId = "porcent_bach", value = T, 
                      label = "% | N", offLabel = "N", onLabel = "%", labelWidth = 40)
        ),
        menuItem("Disciplinas de Serviço", tabName = "serviço", icon = icon("handshake")),
        conditionalPanel(
          condition = "input.est == 'serviço'",
          switchInput(inputId = "porcent_serv", value = T, 
                      label = "% | N", offLabel = "N", onLabel = "%", labelWidth = 40)
        ),
        menuItem("Professores", tabName = "prof", icon = icon("address-card")),
        conditionalPanel(
          condition = "input.est == 'prof'",
          switchInput(inputId = "porcent_prof", value = T, 
                      label = "% | N", offLabel = "N", onLabel = "%", labelWidth = 40)
        )
    )
    
)

header<-dashboardHeader(title = "Disciplinas da EST")

body<-dashboardBody(

# UI - Página 1 Geral -----------------------------------------------------

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
            box(width = 3,
                title = "Formação dos Professores",
                plotlyOutput("professores_doutores")),
            box(width = 3, 
                title = "Tipo de Disciplina",
                plotlyOutput("disciplinas_tipo"))),
          fluidRow(
            box(width = 9,
                title = "Quantidade de Formandos",
                plotlyOutput("formandos_prop")  
            )
          )            
  ),
        
# UI - Página 2 Bacharelado -----------------------------------------------

        tabItem(tabName = "bacharelado",
                fluidRow(
                  column(width = 3,
                    box(title="Parâmetros",status = "primary",solidHeader = T,width = NULL,
                        selectInput('bach_disc', 'Filtre pela(s) disciplina(s):', 
                                    choices = sort(unique(bacharelado$disciplina)),
                                    selected = "None",
                                    multiple = TRUE
                        ),
                        sliderTextInput("bach_periodo", "Filtre pelo(s) período(s):", 
                                        choices = sort(unique(bacharelado$periodo)),
                                        selected = c(as.character(min(bacharelado$periodo)), as.character(max(bacharelado$periodo)))
                        ),
                        selectInput('bach_professor', 'Filtre pelo(s) professor(es):', 
                                    choices = sort(professores_ativos),
                                    selected = "None",
                                    multiple = TRUE
                        ),
                        selectInput("bach_horario", "Filtre pelo(s) horário(s):", 
                                    choices = sort(unique(bacharelado$horario)),
                                    multiple = TRUE
                        ),
                        selectInput("bach_turma", "Filtre pela(s) turma(s):", 
                                    choices = sort(as.character(unique(bacharelado$turma))),
                                    multiple = TRUE
                        ),
                        actionBttn(
                          inputId = "bach_limpar",
                          label = "Limpar",
                          style = "stretch", 
                          color = "primary"
                        )
                    ),
                    
                    infoBoxOutput("bach_aprovacoes", width = 12),
                    infoBoxOutput("bach_reprovacoes", width = 12)
                    ),
                  tabBox(
                    title = "Disciplinas",
                    id = "tabset0", width = 9,
                    tabPanel("Menções", plotlyOutput("bach_mencoes", height = "800px")),
                    tabPanel("Aprovações", plotlyOutput("bach_aprov", height = "800px"))
                    
                  ),
                  
                  tabBox(
                    title = "Histórico",
                    id = "tabset1", width = 12,
                    tabPanel("Menções", plotlyOutput("bach_mencoes_hist")),
                    tabPanel("Aprovações", plotlyOutput("bach_aprov_hist"))
                  )
                )
        ),

# UI - Página 3 Serviço ---------------------------------------------------

tabItem(tabName = "serviço",
        fluidRow(
          column(width = 3,
                 box(title="Parâmetros",status = "primary",solidHeader = T,width = NULL,
                     selectInput('serv_disc', 'Filtre pela(s) disciplina(s):', 
                                 choices = sort(unique(servico$disciplina)),
                                 selected = "None",
                                 multiple = TRUE
                     ),
                     #TODO: update selections
                     sliderTextInput("serv_periodo", "Filtre pelo(s) período(s):", 
                                     choices = sort(unique(servico$periodo)),
                                     selected = c(as.character(min(servico$periodo)), as.character(max(servico$periodo)))
                     ),
                     selectInput('curso', 'Filtre pelo(s) curso(s):', 
                                 choices = sort(unique(servico$curso)),
                                 selected = "None", 
                                 multiple = TRUE
                     ),
                     selectInput('serv_professor', 'Filtre pelo(s) professor(es):', 
                                 choices = sort(professores_ativos),
                                 selected = "None",
                                 multiple = TRUE
                     ),
                     selectInput("serv_horario", "Filtre pelo(s) horário(s):", 
                                 choices = sort(unique(servico$horario)),
                                 multiple = TRUE
                     ),
                     selectInput("serv_turma", "Filtre pela(s) turma(s):", 
                                 choices = sort(as.character(unique(servico$turma))),
                                 multiple = TRUE
                     ),
                     #TODO: limpar campos
                     actionBttn(
                       inputId = "serv_limpar",
                       label = "Limpar",
                       style = "stretch", 
                       color = "primary"
                     ),
                 ),
                 infoBoxOutput("serv_aprovacoes", width = 12),
                 infoBoxOutput("serv_reprovacoes", width = 12)
          ),
          tabBox(
            title = "Disciplinas",
            id = "tabset2", width = 9,
            tabPanel("Menções", plotlyOutput("serv_mencoes", height = "665px")),
            tabPanel("Aprovações", plotlyOutput("serv_aprov", height = "665px"))
          ),
          tabBox(
            title = "Histórico",
            id = "tabset3", width = 12,
            tabPanel("Menções", plotlyOutput("serv_mencoes_hist")),
            tabPanel("Aprovações", plotlyOutput("serv_aprov_hist"))
            )
          )             
        ),

tabItem(tabName = "prof",
        fluidRow(
          column(width = 3,
                 box(title="Parâmetros",status = "primary",solidHeader = T,width = NULL,
                     selectInput('prof_disc', 'Filtre pela(s) disciplina(s):', 
                                 choices = sort(unique(historico$disciplina)),
                                 selected = "None",
                                 multiple = TRUE
                     ),
                     sliderTextInput("prof_periodo", "Filtre pelo(s) período(s):", 
                                     choices = sort(unique(historico$periodo)),
                                     selected = c(as.character(min(historico$periodo)), as.character(max(historico$periodo)))
                     ),
                     selectInput('prof_professor', 'Filtre pelo(s) professor(es):', 
                                 choices = sort(professores_ativos),
                                 selected = "None",
                                 multiple = TRUE
                     ),
                     selectInput("prof_horario", "Filtre pelo(s) horário(s):", 
                                 choices = sort(unique(historico$horario)),
                                 multiple = TRUE
                     ),
                     selectInput("prof_turma", "Filtre pela(s) turma(s):", 
                                 choices = sort(as.character(unique(historico$turma))),
                                 multiple = TRUE
                     ),
                     checkboxInput("prof_ativos", "Filtrar professores ativos", 
                                 value = TRUE
                     ),
                     actionBttn(
                       inputId = "prof_limpar",
                       label = "Limpar",
                       style = "stretch", 
                       color = "primary"
                     )
                 ),
                 
                 infoBoxOutput("prof_aprovacoes", width = 12),
                 infoBoxOutput("prof_reprovacoes", width = 12)
          ),
          tabBox(
            title = "Disciplinas",
            id = "tabset4", width = 9,
            tabPanel("Menções", 
                     radioButtons("prof_mencoes_ordem", "Ordenar por", 
                                  choices = c("Reprovação", "SR", "II", "MI", "MM", "MS", "SS"),
                                  inline = TRUE,
                     ),
                     plotlyOutput("prof_mencoes", height = "1000px")),
            tabPanel("Aprovações", 
                     radioButtons("prof_aprov_ordem", "Ordenar por", 
                                  choices = c("Reprovação + Trancamento", "Trancamento"),
                                  inline = TRUE,
                     ),
                     plotlyOutput("prof_aprov", height = "1000px"))
            )
          )
        )
    )
)

ui<-dashboardPage(sidebar = sidebar,header = header,body= body)

server <- function(input, output, session) {

# Server - Página 1 Geral -------------------------------------------------

  opcao_hist<- reactive({
    if(input$period=="Todos"){
      opcao_hist<- historico
    } else{
      opcao_hist<- historico %>% filter(periodo==input$period)
    }
    
    opcao_hist
    
  })
  
  output$alunos <- renderInfoBox({
    infoBox(
      "Alunos matriculados", nrow(opcao_hist() %>% filter(curso=="Estatística") %>%
                                    summarise(unique(nome, ))) ,
      color = "purple", fill = TRUE,icon=icon("user-friends")
    )
  })
  
  output$disciplinas <- renderInfoBox({
    infoBox(
      "Disciplinas ofertadas", nrow(unique(opcao_hist() %>% select(disciplina,tipo))),
      color = "navy", fill = TRUE, icon=icon("book-reader")
    )
  })
  
  prof_ativ<-reactive({
    prof_ativ<-opcao_hist() %>% summarise(professor=unique(professor))
    prof_ativ
  })
  
  output$professores_ativ <- renderInfoBox({
    infoBox(
      "Professores", nrow(prof_ativ()) ,
      color = "light-blue", fill = TRUE, icon=icon("chalkboard-teacher")
    )
  })
  
  taxa_aprovacao <- reactive({
    (opcao_hist() %>% 
       count(resultado) %>% 
       summarise(prop = n/sum(n), resultado = resultado) %>% 
       filter(resultado == "Aprovação"))$prop      
  })
  
  taxa_reprovacao <- reactive({
    (opcao_hist() %>%
       count(resultado) %>% 
       summarise(prop = n/sum(n), resultado = resultado) %>% 
       filter(resultado == "Reprovação"))$prop      
  })  
  
  
  output$aprovacoes_geral <- renderInfoBox({
    infoBox(
      "Aprovação média", label_percent(accuracy = 0.1, decimal.mark = ",")(taxa_aprovacao()) ,
      color = "aqua", fill = TRUE,icon=icon("check")
    )
  })
  
  output$reprovacoes_geral <- renderInfoBox({
    infoBox(
      "Reprovação média", label_percent(accuracy = 0.1, decimal.mark = ",")(taxa_reprovacao()) ,
      color = "red", fill = TRUE,icon=icon("times")
    )
  })
  
  #TODO: simplificar calculos dos professores doutores e gráfico
  prof_dout <- reactive({
    
    if(input$period == "Todos"){
      Sem_ano_dout<-as.numeric(str_sub(factor(periodo[length(periodo)]), end = 4))
    } else{
      Sem_ano_dout<-as.numeric(str_sub(factor(input$period), end = 4))
    }
    
    prof_dout<-merge(prof_ativ(),
                     professores_dout,by.x="professor",by.y="Professores",all.x=T) %>%
      mutate(Ano_dout=if_else(Ano_dout<=Sem_ano_dout,1,Ano_dout),
             Ano_dout=if_else(Ano_dout>Sem_ano_dout,0,Ano_dout),
             Ano_dout=replace_na(Ano_dout, 0),
             Ano_dout=as.character(Ano_dout),
             Ano_dout=replace(Ano_dout, Ano_dout=="1", "Doutor"),
             Ano_dout=replace(Ano_dout, Ano_dout=="0", "Mestre")) %>% count(Ano_dout)
    
    prof_dout
  })
  
  output$professores_doutores<-renderPlotly({
    prof_dout()%>%
      plot_ly(labels = ~factor(Ano_dout), values = ~n,
              sort = FALSE, marker = list(colors = c("#D0F5FF",
                                                     "#79C1F4")))%>%
      add_pie(hole = 0.6) %>% layout(showlegend = T,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  output$disciplinas_tipo <- renderPlotly({
    unique(opcao_hist() %>% select(disciplina,tipo)) %>% count(tipo) %>%
      plot_ly(labels = ~factor(tipo), values = ~n,
              sort = FALSE, marker = list(colors = c("#00C5C3","#C5F1E9")))%>%
      add_pie(hole = 0.6) %>% layout(showlegend = T,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  #TODO: Usar dados confiáveis de formandos (e precisa mesmo desse gráfico na 1a pagina?)
  
  prop_formandos<- reactive({
    
    if(input$period=="Todos"){
      ano_form <- tail(periodo,1)
    }else{
      ano_form <- input$period
    }
    
    prop_formandos <- formandos %>% 
      group_by(periodo) %>% summarise(n = n()) %>% 
      summarise(n=n, periodo=periodo) %>%
      filter(periodo <= ano_form)
    prop_formandos
  })
  
  output$formandos_prop <-renderPlotly({
    p <- prop_formandos()%>%
      ggplot(aes(x = periodo, y = n,group=1, text = paste0('Período: ', periodo, '\n', 'Quantidade: ', n))) + 
      geom_line(color="#00C5C3") + 
      labs(x="Período", y="Quantidade") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
  })


# Server - Página 2 Bacharelado -------------------------------------------

    bach_filtrado <- reactive({
      bacharelado %>%
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
    
    observeEvent(input$bach_limpar, {
      updateSelectInput(session, "bach_disc", selected = "None")
      updateSliderTextInput(session, "bach_periodo", selected = c(as.character(min(bacharelado$periodo)), as.character(max(bacharelado$periodo))))
      updateSelectInput(session, "bach_professor", selected = "None")
      updateSelectInput(session, "bach_horario", selected = "None")
      updateSelectInput(session, "bach_turma", selected = "None")
    })
    
    output$bach_mencoes <- renderPlotly({
      
      if(input$porcent_bach == TRUE){
        p <- bach_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("SR", "II", "MI")))) %>% 
            ggplot() + 
            geom_bar(aes(x = fct_rev(disciplina), fill = fct_rev(mencao),
                         text = paste0('Disciplina: ', disciplina, 
                                       '\nMenção: ', mencao)), position = "fill") + 
            labs(x="", y="Porcentagem de Alunos", fill = "") +
            scale_fill_brewer(palette = "RdBu", direction = -1) + 
            coord_flip() +
            theme_minimal() +
          scale_y_continuous(labels = scales::percent)
        
        ggplotly(p, tooltip = c('text', 'count')) #TODO: consertar 'count'
      } else{
        p <- bach_filtrado() %>% 
          filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
          mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) sum(.x %in% c("SR", "II", "MI")))) %>% 
          ggplot() + 
          geom_bar(aes(x = fct_rev(disciplina), fill = fct_rev(mencao),
                       text = paste0('Disciplina: ', disciplina, 
                                     '\nMenção: ', mencao))) + 
          labs(x="", y="Número de Alunos", fill = "") +
          scale_fill_brewer(palette = "RdBu", direction = -1) + 
          coord_flip() +
          theme_minimal()
        
        ggplotly(p, tooltip = c('text', 'count'))
      }
    })
    
    output$bach_aprov <- renderPlotly({
      if(input$porcent_bach == TRUE){
      p <- bach_filtrado() %>% 
        mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) mean(.x %in% c("Reprovação", "Trancamento")))) %>% 
        ggplot() + 
        geom_bar(aes(x = fct_rev(disciplina), fill = fct_rev(resultado),
                     text = paste0('Disciplina: ', disciplina, 
                                   '\nResultado: ', resultado)), position = "fill") + 
        labs(x="", y="Porcentagem", fill = "") +
        scale_fill_manual(values = c("#089abf", "gray80", "#dd4b39")) + 
        coord_flip() +
        theme_minimal() +
        scale_y_continuous(labels = scales::percent)
      
      ggplotly(p, tooltip = c('text', 'count'))
      } else{
        p <- bach_filtrado() %>% 
          mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) sum(.x %in% c("Reprovação", "Trancamento")))) %>% 
          ggplot() + 
          geom_bar(aes(x = fct_rev(disciplina), fill = fct_rev(resultado),
                       text = paste0('Disciplina: ', disciplina, 
                                     '\nResultado: ', resultado))) + 
          labs(x="", y="Número de Alunos", fill = "") +
          scale_fill_manual(values = c("#089abf", "gray80", "#dd4b39")) + 
          coord_flip() +
          theme_minimal()
        
        ggplotly(p, tooltip = c('text', 'count'))
      }
    })
    
    output$bach_aprov_hist <-renderPlotly({
      if(input$porcent_bach == TRUE){
        p <- bach_filtrado() %>% 
          group_by(periodo, resultado) %>% summarise(n = n()) %>% 
          summarise(prop = n/sum(n), resultado = resultado) %>% 
          complete(resultado, fill = list(prop = 0)) %>% 
          ggplot(aes(x = periodo, y = prop, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
          geom_line(aes(text = paste0('Período: ', periodo, 
                                    '\nResultado:', resultado,
                                    '\nPorcentagem de Alunos: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop)))) + 
          labs(x="Período", y="Porcentagem de Alunos") +
          scale_colour_manual(name="", values = c("#089abf", "#dd4b39", "gray80")) + 
          scale_y_continuous(labels = scales::label_percent()) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
      } else{
        p <- bach_filtrado() %>% 
          group_by(periodo, resultado) %>% 
          summarise(n = n(), resultado = resultado) %>% 
          complete(resultado, fill = list(n = 0)) %>% 
          ggplot(aes(x = periodo, y = n, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
          geom_line(aes(text = paste0('Período: ', periodo, 
                                      '\nResultado:', resultado,
                                      '\nNúmero de Alunos: ', n))) + 
          labs(x="Período", y="Número de Alunos") +
          scale_colour_manual(name="", values = c("#089abf", "#dd4b39", "gray80")) + 
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))        
      }
    })
    
    output$bach_mencoes_hist <-renderPlotly({
      if(input$porcent_bach == TRUE){
        p <- bach_filtrado() %>% 
          group_by(periodo, mencao) %>% summarise(n = n()) %>% 
          summarise(prop = n/sum(n), mencao = mencao) %>% 
          complete(mencao, fill = list(prop = 0)) %>% 
          filter(mencao %in% c("SR", "II", "MI", "MM", "MS", "SS")) %>% 
          ggplot(aes(x = periodo, y = prop, group = mencao, color = mencao)) + 
          geom_line(aes(text = paste0('Período: ', periodo, 
                                      '\nMenção:', mencao,
                                      '\nPorcentagem de Alunos: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop)))) + 
          labs(x="Período", y="Porcentagem") +
          #scale_colour_manual(name="", values = c("#00A4CD", "#F08080", "yellow")) + 
          scale_color_brewer(palette = "RdBu") +
          scale_y_continuous(labels = scales::label_percent()) +
          theme_minimal() + 
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
      ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
      } else{
        p <- bach_filtrado() %>% 
          group_by(periodo, mencao) %>% 
          summarise(n = n(), mencao = mencao) %>% 
          complete(mencao, fill = list(n = 0)) %>% 
          filter(mencao %in% c("SR", "II", "MI", "MM", "MS", "SS")) %>% 
          ggplot(aes(x = periodo, y = n, group = mencao, color = mencao)) + 
          geom_line(aes(text = paste0('Período: ', periodo, 
                                      '\nMenção:', mencao,
                                      '\nNúmero de Alunos: ', n))) + 
          labs(x="Período", y="Número de Alunos") +
          #scale_colour_manual(name="", values = c("#00A4CD", "#F08080", "yellow")) + 
          scale_color_brewer(palette = "RdBu") +
          theme_minimal() + 
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))        
      }
    })
    

# Server - Página 3 Serviço -----------------------------------------------

    output$aprovacoes <- renderInfoBox({
      infoBox(
        "Aprovação média", "230 (30%)" ,
        color = "aqua", fill = TRUE,icon=icon("check")
      )
    })
    
    serv_filtrado <- reactive({
      servico %>%
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
    
    observeEvent(input$serv_limpar, {
      updateSelectInput(session, "serv_disc", selected = "None")
      updateSliderTextInput(session, "serv_periodo", selected = c(as.character(min(servico$periodo)), as.character(max(servico$periodo))))
      updateSelectInput(session, "curso", selected = "None")
      updateSelectInput(session, "serv_professor", selected = "None")
      updateSelectInput(session, "serv_horario", selected = "None")
      updateSelectInput(session, "serv_turma", selected = "None")
    })
    
    output$serv_mencoes <- renderPlotly({
      if(input$porcent_serv == TRUE){
        p <- serv_filtrado() %>% 
          filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
          mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("SR", "II", "MI")))) %>% 
          ggplot() + 
          geom_bar(aes(x = fct_rev(disciplina), fill = fct_rev(mencao),
                       text = paste0('Disciplina: ', disciplina, 
                                     '\nMenção: ', mencao)), position = "fill") + 
          labs(x="", y="Porcentagem de Alunos", fill = "") +
          scale_fill_brewer(palette = "RdBu", direction = -1) + 
          coord_flip() +
          theme_minimal() +
          scale_y_continuous(labels = scales::percent)
        
        ggplotly(p, tooltip = c('text', 'count'))
        
      } else{
        p <- serv_filtrado() %>% 
          filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
          mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) sum(.x %in% c("SR", "II", "MI")))) %>% 
          ggplot() + 
          geom_bar(aes(x = fct_rev(disciplina), fill = fct_rev(mencao),
                       text = paste0('Disciplina: ', disciplina, 
                                     '\nMenção: ', mencao))) + 
          labs(x="", y="Número de Alunos", fill = "") +
          scale_fill_brewer(palette = "RdBu", direction = -1) + 
          coord_flip() +
          theme_minimal()
        
        ggplotly(p, tooltip = c('text', 'count'))  
      }
    })
    
    output$serv_aprov <- renderPlotly({
      if(input$porcent_serv == TRUE){
        p <- serv_filtrado() %>% 
          mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) mean(.x == "Reprovação"))) %>% 
          ggplot() + 
          geom_bar(aes(x = fct_rev(disciplina), fill = fct_rev(resultado),
                       text = paste0('Disciplina: ', disciplina, 
                                     '\nResultado: ', resultado)), position = "fill") + 
          labs(x="", y="Porcenetagem de Alunos", fill = "") +
          scale_fill_manual(values = c("#089abf", "gray80", "#dd4b39")) + 
          coord_flip() +
          theme_minimal() +
          scale_y_continuous(labels = scales::percent)
        
        ggplotly(p, tooltip = c('text', 'count'))
      } else {
        p <- serv_filtrado() %>% 
          mutate(disciplina = fct_reorder(disciplina, resultado, function(.x) sum(.x == "Reprovação"))) %>% 
          ggplot() + 
          geom_bar(aes(x = fct_rev(disciplina), fill = fct_rev(resultado),
                       text = paste0('Disciplina: ', disciplina, 
                                     '\nResultado: ', resultado))) + 
          labs(x="", y="Número de Alunos", fill = "") +
          scale_fill_manual(values = c("#089abf", "gray80", "#dd4b39")) + 
          coord_flip() +
          theme_minimal()
        
        ggplotly(p, tooltip = c('text', 'count'))        
      }
    })
    
    output$serv_aprov_hist <-renderPlotly({
      if(input$porcent_serv == TRUE){
        p <- serv_filtrado() %>% 
          group_by(periodo, resultado) %>% summarise(n = n()) %>% 
          summarise(prop = n/sum(n), resultado = resultado) %>% 
          complete(resultado, fill = list(prop = 0)) %>% 
          ggplot(aes(x = periodo, y = prop, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
          geom_line(aes(text = paste0('Período: ', periodo, 
                                      '\nResultado:', resultado,
                                      '\nPorcentagem de Alunos: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop)))) + 
          labs(x="Período", y="Porcentagem de Alunos") +
          scale_colour_manual(name="", values = c("#089abf", "#dd4b39", "gray80")) + 
          scale_y_continuous(labels = scales::label_percent()) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
      } else{
        p <- serv_filtrado() %>% 
          group_by(periodo, resultado) %>% 
          summarise(n = n(), resultado = resultado) %>% 
          complete(resultado, fill = list(n = 0)) %>% 
          ggplot(aes(x = periodo, y = n, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
          geom_line(aes(text = paste0('Período: ', periodo, 
                                      '\nResultado:', resultado,
                                      '\nNúmero de Alunos: ', n))) + 
          labs(x="Período", y="Número de Alunos") +
          scale_colour_manual(name="", values = c("#089abf", "#dd4b39", "gray80")) + 
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
      }
      
    })
    
    output$serv_mencoes_hist <-renderPlotly({
      if(input$porcent_serv == TRUE){
        p <- serv_filtrado() %>% 
          group_by(periodo, mencao) %>% summarise(n = n()) %>% 
          summarise(prop = n/sum(n), mencao = mencao) %>% 
          complete(mencao, fill = list(prop = 0)) %>% 
          filter(mencao %in% c("SR", "II", "MI", "MM", "MS", "SS")) %>% 
          ggplot(aes(x = periodo, y = prop, group = mencao, color = mencao)) + 
          geom_line(aes(text = paste0('Período: ', periodo, 
                                      '\nMenção:', mencao,
                                      '\nPorcentagem: ', label_percent(accuracy = 0.1, decimal.mark = ",")(prop)))) + 
          labs(x="Período", y="Porcentagem") +
          scale_color_brewer(palette = "RdBu") +
          scale_y_continuous(labels = scales::label_percent()) +
          theme_minimal() + 
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
      } else{
        p <- serv_filtrado() %>% 
          group_by(periodo, mencao) %>% 
          summarise(n = n(), mencao = mencao) %>% 
          complete(mencao, fill = list(n = 0)) %>% 
          filter(mencao %in% c("SR", "II", "MI", "MM", "MS", "SS")) %>% 
          ggplot(aes(x = periodo, y = n, group = mencao, color = mencao)) + 
          geom_line(aes(text = paste0('Período: ', periodo, 
                                      '\nMenção:', mencao,
                                      '\nNúmero de Alunos: ', n))) + 
          labs(x="Período", y="Número de Alunos") +
          scale_color_brewer(palette = "RdBu") +
          theme_minimal() + 
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))        
      }
    })


# Server - Página 4 Professores -------------------------------------------
    
    observeEvent(input$prof_limpar, {
      updateSelectInput(session, "prof_disc", selected = "None")
      updateSliderTextInput(session, "prof_periodo", selected = c(as.character(min(historico$periodo)), as.character(max(historico$periodo))))
      updateSelectInput(session, "prof_professor", selected = "None")
      updateSelectInput(session, "prof_horario", selected = "None")
      updateSelectInput(session, "prof_turma", selected = "None")
    })
    
    hist_filtrado <- reactive({
      if(input$prof_ativos == FALSE){
        historico %>%
          filter(
            conditional(!is.null(input$prof_disc), disciplina %in% input$prof_disc),
            conditional(TRUE, periodo >= input$prof_periodo[1] & periodo <= input$prof_periodo[2]),
            conditional(!is.null(input$prof_professor), professor %in% input$prof_professor),
            conditional(!is.null(input$prof_horario), horario %in% input$prof_horario),
            conditional(!is.null(input$prof_turma), turma %in% input$prof_turma)
          )
      } else{
        historico %>%
          filter(
            professor %in% professores_ativos,
            conditional(!is.null(input$prof_disc), disciplina %in% input$prof_disc),
            conditional(TRUE, periodo >= input$prof_periodo[1] & periodo <= input$prof_periodo[2]),
            conditional(!is.null(input$prof_professor), professor %in% input$prof_professor),
            conditional(!is.null(input$prof_horario), horario %in% input$prof_horario),
            conditional(!is.null(input$prof_turma), turma %in% input$prof_turma)
          )
      } 
    })
    
    output$prof_mencoes <- renderPlotly({
      
      if(input$porcent_prof == TRUE){
        if(input$prof_mencoes_ordem == "Reprovação"){
          p <- hist_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
            mutate(professor = fct_reorder(professor, mencao, function(.x) mean(.x %in% c("SR", "II", "MI")))) %>% 
            ggplot() + 
            geom_bar(aes(x = fct_rev(professor), fill = fct_rev(mencao),
                         text = paste0('Professor: ', professor, 
                                       '\nMenção: ', mencao)), position = "fill") + 
            labs(x="", y="Porcentagem de Alunos", fill = "") +
            scale_fill_brewer(palette = "RdBu", direction = -1) + 
            coord_flip() +
            theme_minimal() +
            scale_y_continuous(labels = scales::percent)
        
          ggplotly(p, tooltip = c('text', 'count')) #TODO: consertar 'count'
        
          } else if(input$prof_mencoes_ordem == "SR"){
            p <- hist_filtrado() %>% 
              filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
              mutate(professor = fct_reorder(professor, mencao, function(.x) mean(.x %in% c("SR")))) %>% 
              ggplot() + 
              geom_bar(aes(x = fct_rev(professor), fill = fct_rev(mencao),
                           text = paste0('Professor: ', professor, 
                                         '\nMenção: ', mencao)), position = "fill") + 
              labs(x="", y="Porcentagem de Alunos", fill = "") +
              scale_fill_brewer(palette = "RdBu", direction = -1) + 
              coord_flip() +
              theme_minimal() +
              scale_y_continuous(labels = scales::percent)
            
            ggplotly(p, tooltip = c('text', 'count')) #TODO: consertar 'count'          
          
          } else if(input$prof_mencoes_ordem == "II"){
            p <- hist_filtrado() %>% 
              filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
              mutate(professor = fct_reorder(professor, mencao, function(.x) mean(.x %in% c("II")))) %>% 
              ggplot() + 
              geom_bar(aes(x = fct_rev(professor), fill = fct_rev(mencao),
                           text = paste0('Professor: ', professor, 
                                         '\nMenção: ', mencao)), position = "fill") + 
              labs(x="", y="Porcentagem de Alunos", fill = "") +
              scale_fill_brewer(palette = "RdBu", direction = -1) + 
              coord_flip() +
              theme_minimal() +
              scale_y_continuous(labels = scales::percent)
            
            ggplotly(p, tooltip = c('text', 'count')) #TODO: consertar 'count'          
       
          } else if(input$prof_mencoes_ordem == "MI"){
            p <- hist_filtrado() %>% 
              filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
              mutate(professor = fct_reorder(professor, mencao, function(.x) mean(.x %in% c("MI")))) %>% 
              ggplot() + 
              geom_bar(aes(x = fct_rev(professor), fill = fct_rev(mencao),
                           text = paste0('Professor: ', professor, 
                                         '\nMenção: ', mencao)), position = "fill") + 
              labs(x="", y="Porcentagem de Alunos", fill = "") +
              scale_fill_brewer(palette = "RdBu", direction = -1) + 
              coord_flip() +
              theme_minimal() +
              scale_y_continuous(labels = scales::percent)
            
            ggplotly(p, tooltip = c('text', 'count')) #TODO: consertar 'count'          
          
          } else if(input$prof_mencoes_ordem == "MM"){
            p <- hist_filtrado() %>% 
              filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
              mutate(professor = fct_reorder(professor, mencao, function(.x) mean(.x %in% c("MM")))) %>% 
              ggplot() + 
              geom_bar(aes(x = fct_rev(professor), fill = fct_rev(mencao),
                           text = paste0('Professor: ', professor, 
                                         '\nMenção: ', mencao)), position = "fill") + 
              labs(x="", y="Porcentagem de Alunos", fill = "") +
              scale_fill_brewer(palette = "RdBu", direction = -1) + 
              coord_flip() +
              theme_minimal() +
              scale_y_continuous(labels = scales::percent)
            
            ggplotly(p, tooltip = c('text', 'count')) #TODO: consertar 'count'          
          
          } else if(input$prof_mencoes_ordem == "MS"){
            p <- hist_filtrado() %>% 
              filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
              mutate(professor = fct_reorder(professor, mencao, function(.x) mean(.x %in% c("MS")))) %>% 
              ggplot() + 
              geom_bar(aes(x = fct_rev(professor), fill = fct_rev(mencao),
                           text = paste0('Professor: ', professor, 
                                         '\nMenção: ', mencao)), position = "fill") + 
              labs(x="", y="Porcentagem de Alunos", fill = "") +
              scale_fill_brewer(palette = "RdBu", direction = -1) + 
              coord_flip() +
              theme_minimal() +
              scale_y_continuous(labels = scales::percent)
            
            ggplotly(p, tooltip = c('text', 'count')) #TODO: consertar 'count'          
          
          } else if(input$prof_mencoes_ordem == "SS"){
            p <- hist_filtrado() %>% 
              filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
              mutate(professor = fct_reorder(professor, mencao, function(.x) mean(.x %in% c("SS")))) %>% 
              ggplot() + 
              geom_bar(aes(x = fct_rev(professor), fill = fct_rev(mencao),
                           text = paste0('Professor: ', professor, 
                                         '\nMenção: ', mencao)), position = "fill") + 
              labs(x="", y="Porcentagem de Alunos", fill = "") +
              scale_fill_brewer(palette = "RdBu", direction = -1) + 
              coord_flip() +
              theme_minimal() +
              scale_y_continuous(labels = scales::percent)
            
            ggplotly(p, tooltip = c('text', 'count')) #TODO: consertar 'count'          
          }
        
      } else{
        if(input$prof_mencoes_ordem == "Reprovação"){
          p <- hist_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
            mutate(professor = fct_reorder(professor, mencao, function(.x) sum(.x %in% c("SR", "II", "MI")))) %>% 
            ggplot() + 
            geom_bar(aes(x = fct_rev(professor), fill = fct_rev(mencao),
                         text = paste0('Professor: ', professor, 
                                       '\nMenção: ', mencao))) + 
            labs(x="", y="Número de Alunos", fill = "") +
            scale_fill_brewer(palette = "RdBu", direction = -1) + 
            coord_flip() +
            theme_minimal()
        
          ggplotly(p, tooltip = c('text', 'count'))
          
          } else if(input$prof_mencoes_ordem == "SR"){
            p <- hist_filtrado() %>% 
              filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
              mutate(professor = fct_reorder(professor, mencao, function(.x) sum(.x %in% c("SR")))) %>% 
              ggplot() + 
              geom_bar(aes(x = fct_rev(professor), fill = fct_rev(mencao),
                           text = paste0('Professor: ', professor, 
                                         '\nMenção: ', mencao))) + 
              labs(x="", y="Número de Alunos", fill = "") +
              scale_fill_brewer(palette = "RdBu", direction = -1) + 
              coord_flip() +
              theme_minimal()
            
            ggplotly(p, tooltip = c('text', 'count'))
      
          } else if(input$prof_mencoes_ordem == "II"){
            p <- hist_filtrado() %>% 
              filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
              mutate(professor = fct_reorder(professor, mencao, function(.x) sum(.x %in% c("II")))) %>% 
              ggplot() + 
              geom_bar(aes(x = fct_rev(professor), fill = fct_rev(mencao),
                           text = paste0('Professor: ', professor, 
                                         '\nMenção: ', mencao))) + 
              labs(x="", y="Número de Alunos", fill = "") +
              scale_fill_brewer(palette = "RdBu", direction = -1) + 
              coord_flip() +
              theme_minimal()
            
            ggplotly(p, tooltip = c('text', 'count'))
          
          } else if(input$prof_mencoes_ordem == "MI"){
            p <- hist_filtrado() %>% 
              filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
              mutate(professor = fct_reorder(professor, mencao, function(.x) sum(.x %in% c("MI")))) %>% 
              ggplot() + 
              geom_bar(aes(x = fct_rev(professor), fill = fct_rev(mencao),
                           text = paste0('Professor: ', professor, 
                                         '\nMenção: ', mencao))) + 
              labs(x="", y="Número de Alunos", fill = "") +
              scale_fill_brewer(palette = "RdBu", direction = -1) + 
              coord_flip() +
              theme_minimal()
            
            ggplotly(p, tooltip = c('text', 'count'))
            
          } else if(input$prof_mencoes_ordem == "MM"){
            p <- hist_filtrado() %>% 
              filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
              mutate(professor = fct_reorder(professor, mencao, function(.x) sum(.x %in% c("MM")))) %>% 
              ggplot() + 
              geom_bar(aes(x = fct_rev(professor), fill = fct_rev(mencao),
                           text = paste0('Professor: ', professor, 
                                         '\nMenção: ', mencao))) + 
              labs(x="", y="Número de Alunos", fill = "") +
              scale_fill_brewer(palette = "RdBu", direction = -1) + 
              coord_flip() +
              theme_minimal()
            
            ggplotly(p, tooltip = c('text', 'count'))
          
          } else if(input$prof_mencoes_ordem == "MS"){
            p <- hist_filtrado() %>% 
              filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
              mutate(professor = fct_reorder(professor, mencao, function(.x) sum(.x %in% c("MS")))) %>% 
              ggplot() + 
              geom_bar(aes(x = fct_rev(professor), fill = fct_rev(mencao),
                           text = paste0('Professor: ', professor, 
                                         '\nMenção: ', mencao))) + 
              labs(x="", y="Número de Alunos", fill = "") +
              scale_fill_brewer(palette = "RdBu", direction = -1) + 
              coord_flip() +
              theme_minimal()
            
            ggplotly(p, tooltip = c('text', 'count'))
            
          } else if(input$prof_mencoes_ordem == "SS"){
            p <- hist_filtrado() %>% 
              filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
              mutate(professor = fct_reorder(professor, mencao, function(.x) sum(.x %in% c("SS")))) %>% 
              ggplot() + 
              geom_bar(aes(x = fct_rev(professor), fill = fct_rev(mencao),
                           text = paste0('Professor: ', professor, 
                                         '\nMenção: ', mencao))) + 
              labs(x="", y="Número de Alunos", fill = "") +
              scale_fill_brewer(palette = "RdBu", direction = -1) + 
              coord_flip() +
              theme_minimal()
            
            ggplotly(p, tooltip = c('text', 'count'))
          }
      }
    })
    
    output$prof_aprov <- renderPlotly({
      if(input$porcent_prof == TRUE){
        if(input$prof_aprov_ordem == "Reprovação + Trancamento"){
          p <- hist_filtrado() %>% 
            mutate(professor = fct_reorder(professor, resultado, function(.x) mean(.x %in% c("Reprovação", "Trancamento")))) %>% 
           ggplot() + 
            geom_bar(aes(x = fct_rev(professor), fill = fct_rev(resultado),
                         text = paste0('Disciplina: ', professor, 
                                       '\nResultado: ', resultado)), position = "fill") + 
            labs(x="", y="Porcentagem", fill = "") +
            scale_fill_manual(values = c("#089abf", "gray80", "#dd4b39")) + 
            coord_flip() +
            theme_minimal() +
            scale_y_continuous(labels = scales::percent)
        
          ggplotly(p, tooltip = c('text', 'count'))
        } else{
          p <- hist_filtrado() %>% 
            mutate(professor = fct_reorder(professor, resultado, function(.x) mean(.x %in% c("Trancamento")))) %>% 
            ggplot() + 
            geom_bar(aes(x = fct_rev(professor), fill = fct_rev(resultado),
                         text = paste0('Disciplina: ', professor, 
                                       '\nResultado: ', resultado)), position = "fill") + 
            labs(x="", y="Porcentagem", fill = "") +
            scale_fill_manual(values = c("#089abf", "gray80", "#dd4b39")) + 
            coord_flip() +
            theme_minimal() +
            scale_y_continuous(labels = scales::percent)
          
          ggplotly(p, tooltip = c('text', 'count'))
        }
      } else{
        if(input$prof_aprov_ordem == "Reprovação + Trancamento"){
          p <- hist_filtrado() %>% 
            mutate(professor = fct_reorder(professor, resultado, function(.x) sum(.x %in% c("Reprovação", "Trancamento")))) %>% 
            ggplot() + 
            geom_bar(aes(x = fct_rev(professor), fill = fct_rev(resultado),
                         text = paste0('Disciplina: ', professor, 
                                       '\nResultado: ', resultado))) + 
            labs(x="", y="Número de Alunos", fill = "") +
            scale_fill_manual(values = c("#089abf", "gray80", "#dd4b39")) + 
            coord_flip() +
            theme_minimal()
        
          ggplotly(p, tooltip = c('text', 'count'))
        
        } else{
          p <- hist_filtrado() %>% 
            mutate(professor = fct_reorder(professor, resultado, function(.x) sum(.x %in% c("Trancamento")))) %>% 
            ggplot() + 
            geom_bar(aes(x = fct_rev(professor), fill = fct_rev(resultado),
                         text = paste0('Disciplina: ', professor, 
                                       '\nResultado: ', resultado))) + 
            labs(x="", y="Número de Alunos", fill = "") +
            scale_fill_manual(values = c("#089abf", "gray80", "#dd4b39")) + 
            coord_flip() +
            theme_minimal()
          
          ggplotly(p, tooltip = c('text', 'count'))
          }
        }
    })
    
}

shinyApp(ui, server)
