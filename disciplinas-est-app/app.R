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
        menuItem("Disciplinas de Serviço", tabName = "serviço", icon = icon("handshake")),
        conditionalPanel(
          condition = "input.est == 'serviço'",
          switchInput(inputId = "abs_serv", value = T, 
                      label = "% | N", offLabel = "N", onLabel = "%", labelWidth = 40)
        ),
        menuItem("Disciplinas do Bacharelado", tabName = "bacharelado", icon = icon("chart-line")),
        conditionalPanel(
          condition = "input.est == 'bacharelado'",
          switchInput(inputId = "abs_bach", value = T, 
                      label = "% | N", offLabel = "N", onLabel = "%", labelWidth = 40)
        ),
        menuItem("Professores", tabName = "prof", icon = icon("address-card"))
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
        

# UI - Página 2 Serviço ---------------------------------------------------

        tabItem(tabName = "serviço",
                
                fluidRow(
                  column(width = 3,
                         box(title="Parâmetros",status = "primary",solidHeader = T,width = NULL,
                             selectInput('serv_disc', 'Filtre pela(s) disciplina(s):', 
                                         choices = sort(unique(servico$disciplina)),
                                         selected = "None",
                                         multiple = TRUE
                                         ),
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
                    ),
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
        

# UI - Página 3 Bacharelado -----------------------------------------------

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
                        )
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
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p, tooltip = 'text') %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
  })
  

# Server - Página 2 Serviço -----------------------------------------------


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
    
    
    output$serv_mencoes <- renderPlotly({
      p <- serv_filtrado() %>% 
        filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
        mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("SR", "II", "MI")))) %>% 
        ggplot() + 
        geom_bar(aes(x = fct_rev(disciplina), fill = fct_rev(mencao)), position = "fill") + 
        labs(x="", y="Proporção", fill = "") +
        scale_fill_brewer(palette = "RdBu", direction = -1) + 
        coord_flip()
      
      ggplotly(p)
    })
    
    output$serv_resultados <-renderPlotly({
      p <- serv_filtrado() %>% 
        group_by(periodo, resultado) %>% summarise(n = n()) %>% 
        summarise(prop = n/sum(n), resultado = resultado) %>% 
        complete(resultado, fill = list(prop = 0)) %>% 
        ggplot(aes(x = periodo, y = prop, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
        geom_line() + 
        labs(x="Período", y="Proporção") +
        scale_colour_manual(name="", values = c("#00A4CD", "#F08080", "yellow")) + 
        scale_y_continuous(labels = scales::label_percent()) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
    })
    

# Bacharelado -------------------------------------------------------------

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
    
    
    output$bach_mencoes <- renderPlotly({
        p <- bach_filtrado() %>% 
            filter(!mencao %in% c("CC", "DP", "TJ", "TR")) %>% 
            mutate(disciplina = fct_reorder(disciplina, mencao, function(.x) mean(.x %in% c("SR", "II", "MI")))) %>% 
            ggplot() + 
            geom_bar(aes(x = fct_rev(disciplina), fill = fct_rev(mencao)), position = "fill") + 
            labs(x="", y="Proporção", fill = "") +
            scale_fill_brewer(palette = "RdBu", direction = -1) + 
            coord_flip()
        
        ggplotly(p)
    })
    
    output$bach_resultados <-renderPlotly({
      p <- bach_filtrado() %>% 
        group_by(periodo, resultado) %>% summarise(n = n()) %>% 
        summarise(prop = n/sum(n), resultado = resultado) %>% 
        complete(resultado, fill = list(prop = 0)) %>% 
        ggplot(aes(x = periodo, y = prop, group = resultado, color = factor(resultado, levels = c("Aprovação", "Reprovação", "Trancamento")))) + 
        geom_line() + 
        labs(x="Período", y="Proporção") +
        scale_colour_manual(name="", values = c("#00A4CD", "#F08080", "yellow")) + 
        scale_y_continuous(labels = scales::label_percent()) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
    })
}

shinyApp(ui, server)
