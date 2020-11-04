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

# Dados -------------------------------------------------------------------

historico <- read_rds("historico_limpo.rds")
vagas <- read_rds("vagas_limpo.rds")

bacharelado <- historico %>% filter(tipo == "Bacharelado")
servico <- historico %>% filter(tipo == "Serviço")

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
                    box(title="Parâmetros",status = "warning",solidHeader = T,width=3,
                        selectInput(
                            'disc', 'Selecione a disciplina:', choices = c("Estatística aplicada","Cálculo de probabilidade 1"),
                            selectize = FALSE,selected = "None"
                        ),
                        sliderTextInput(
                            inputId = "semestre",
                            label = "Selecione o(s) semestre(s) a ser(em) avaliado(s):", 
                            choices = c("1/2016","2/2016","1/2017","2/2017","1/2018","2/2018","1/2019","2/2019","1/2020","2/2020"),
                            selected = c("1/2018", "1/2018")
                        ),
                        selectInput(
                            'curso', 'Selecione o curso:', choices = c("Biologia","Economia","Ciência da computação","Contabilidade"),
                            selectize = FALSE,selected = "None"
                        ),
                        selectInput(
                            'professor', 'Selecione o professor:', choices = c("Ana Maria Nogales","Eduardo Monteiro","Jhames Sampaio","Maria Teresa"),
                            selectize = FALSE,selected = "None"
                        ),
                        sliderTextInput(
                            inputId = "horário",
                            label = "Selecione o horário:", 
                            choices = c("8h", "10h", "12h", "14h", "16h",'18h')
                        ),
                        awesomeCheckboxGroup(
                            inputId = "turma",
                            label = "Selecione a(s) turma(s):", 
                            choices = c( "A", "B","C"),
                            selected = "A",
                            inline = TRUE, 
                            status = "warning"
                        ),
                        actionBttn(
                            inputId = "button",
                            label = "Atualizar",
                            style = "unite", 
                            color = "primary"
                        )
                    ),
                    infoBoxOutput("aprovacoes1"),
                    infoBoxOutput("reprovacoes1")
                )             
        ),
        
# Bacharelado -------------------------------------------------------------

        tabItem(tabName = "bacharelado",
                
                fluidRow(
                    box(title="Parâmetros",status = "warning",solidHeader = T,width=3,
                        selectInput(
                            'disc', 'Selecione a disciplina:', choices = sort(unique(bacharelado$disciplina)),
                            selectize = FALSE,selected = "None"
                        ),
                        sliderTextInput(
                            inputId = "período",
                            label = "Selecione o(s) periodo(s) a serem avaliados:", 
                            choices = unique(bacharelado$periodo),
                            selected = c("1/2018", "1/2018")
                        ),
                        selectInput(
                            'professor', 'Selecione o professor:', choices = sort(unique(bacharelado$professor)),
                            selectize = FALSE, selected = "None"
                        ),
                        sliderTextInput(
                            inputId = "horário",
                            label = "Selecione o horário:", 
                            choices = c("8h", "10h", "12h", "14h", "16h",'18h')
                        ),
                        awesomeCheckboxGroup(
                            inputId = "turma",
                            label = "Selecione a(s) turma(s):", 
                            choices = c("A", "B","C"),
                            selected = "A",
                            inline = TRUE, 
                            status = "info"
                        ),
                        actionBttn(
                            inputId = "button",
                            label = "Atualizar",
                            style = "unite", 
                            color = "primary"
                        )
                    ),
                    infoBoxOutput("aprovacoes2"),
                    infoBoxOutput("reprovacoes2")
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
    
    output$aprovacoes <- renderInfoBox({
        infoBox(
            "Aprovação média", "230 (30%)" ,
            color = "aqua", fill = TRUE,icon=icon("check")
        )
    })
    
    output$aprovacoes1 <- renderInfoBox({
        infoBox(
            "Aprovação média", "230 (30%)" ,
            color = "aqua", fill = TRUE,icon=icon("check")
        )
    })
    
    output$reprovacoes1 <- renderInfoBox({
        infoBox(
            "Reprovação média", "230 (30%)" ,
            color = "red", fill = TRUE,icon=icon("prohibited")
        )
    })
    
    output$aprovacoes2 <- renderInfoBox({
        infoBox(
            "Aprovação média", "230 (30%)" ,
            color = "aqua", fill = TRUE,icon=icon("check")
        )
    })
    
    output$reprovacoes2 <- renderInfoBox({
        infoBox(
            "Reprovação média", "230 (30%)" ,
            color = "red", fill = TRUE,icon=icon("prohibited")
        )
    })
    
}

shinyApp(ui, server)
