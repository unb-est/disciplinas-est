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

bacharelado <- historico %>% filter(tipo == "Bacharelado")
servico <- historico %>% filter(tipo == "Serviço")

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

# App ---------------------------------------------------------------------

sidebar<-dashboardSidebar(
    sidebarMenu(
        menuItem("Estatística", tabName = "geral", icon = icon("university")),
        menuItem("Disciplinas de Serviço", tabName = "serviço", icon = icon("handshake")),
        menuItem("Disciplinas do Bacharelado", tabName = "bacharelado", icon = icon("chart-line")),
        switchInput(
            inputId = "Id015",
            label = "%", 
            size = "mini"
        )
    )
    
)

header<-dashboardHeader(title = "Disciplinas da EST")

body<-dashboardBody(
    
# Página Inicial ----------------------------------------------------------
    
    tabItems(
        tabItem(tabName = "geral",
                fluidRow(
                    infoBoxOutput("alunos"),
                    infoBoxOutput("professores"),
                    infoBoxOutput("disciplinas"),
                    infoBoxOutput("aprovacoes")
                )            
        ),
        
        
# Disciplinas de Serviço --------------------------------------------------
        
        tabItem(tabName = "serviço",
                
                fluidRow(
                  column(width = 3,
                         box(title="Parâmetros",status = "warning",solidHeader = T,width = NULL,
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
        
# Bacharelado -------------------------------------------------------------
        
        tabItem(tabName = "bacharelado",
                fluidRow(
                  column(width = 3,
                    box(title="Parâmetros",status = "warning",solidHeader = T,width = NULL,
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
    
    output$alunos <- renderInfoBox({
        infoBox(
            "Alunos", "100" ,
            color = "blue", fill = TRUE,icon=icon("user-friends")
        )
    })
    
    output$professores <- renderInfoBox({
        infoBox(
            "Professores", "30" ,
            color = "yellow", fill = TRUE,icon=icon("chalkboard-teacher")
        )
    })
    
    output$disciplinas <- renderInfoBox({
        infoBox(
            "Disciplinas", "20" ,
            color = "red", fill = TRUE,icon=icon("book-reader")
        )
    })


# Serviço -----------------------------------------------------------------

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
