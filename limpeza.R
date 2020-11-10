######################## Limpeza dos dados #######################

library(readxl)
library(tidyverse)
library(lubridate)
library(stringr)
library(abjutils) # remover acentos
library(janitor)
library(stringi)

# Disciplinas (historico) -------------------------------------------------

historico <- read_excel('disciplinas.xlsx')

str(historico)

summary(historico)

historico_limpo <- historico %>%
  transmute(matricula = as.integer(Matrícula),
            cpf = as.factor(ifelse(CPF != "-", CPF, NA)),
            nome = rm_accent(toupper(Nome)),
            curso = as.factor(Curso),
            disciplina_cod = as.factor(`Cód. Disciplina`),
            disciplina = as.factor(rm_accent(stri_trans_totitle(Disciplina))),
            ano = as.integer(str_sub(Período, 1, 4)),
            semestre = as.factor(str_sub(Período, -1, -1)),
            periodo = factor(paste(ano, semestre, sep = "/"), ordered = TRUE),
            matricula_prof = as.factor(`Matricula Prof`),
            professor = rm_accent(toupper(Professor)),
            turma = as.factor(Turma),
            mencao = factor(Menção, ordered = TRUE, levels = c("SR", "II", "MI", "TR", "TJ", "DP", "CC", "MM", "MS", "SS")),
            faltas = as.numeric(str_replace(Faltas, "%", ""))/100,
            horario = factor(paste0(`Hora início`,'-', `Hora Fim`), ordered = TRUE),
            resultado = fct_collapse(mencao,
                                     "Aprovação" = c("SS", "MS", "MM", "CC", "DP"),
                                     "Reprovação" = c("MI", "II", "SR"),
                                     "Trancamento" = c("TR", "TJ")
                                     )
         ) %>% arrange(matricula)

str(historico_limpo)

summary(historico_limpo)

table(historico_limpo$disciplina) # De Series Temporais e Das Series Temporais

servico <- c("Bioestatistica", "Estatistica Aplicada", "Probabilidade E Estatistica", "Probabilidade E Estatistica 2")

professores_ativos <- c('ALAN RICARDO DA SILVA', 'ANA MARIA NOGALES VASCONCELOS', 'ANDRE LUIZ FERNANDES CANCADO',
                        'ANTONIO EDUARDO GOMES', 'BERNARDO BORBA DE ANDRAD', 'BERNARDO NOGUEIRA SCHLEMPER',
                        'CIBELE QUEIROZ DA SILVA', 'CIRA ETHEOWALDA GUEVARA OTINIANO', 'CLAUDETE RUAS',
                        'DEMERSON ANDRE POLLI', 'DONALD MATTHEW PIANTO', 'EDUARDO FREITAS DA SILVA',
                        'EDUARDO MONTEIRO DE CASTRO GOMES', 'EDUARDO YOSHIO NAKANO', 'GEORGE FREITAS VON BORRIES',
                        'GERALDO DA SILVA E SOUZA', 'GLADSTON LUIZ DA SILVA', 'GUILHERME SOUZA RODRIGUES',
                        'GUSTAVO LEONEL GILARDONI AVALLE', 'HELTON SAULO BEZERRA DOS SANTOS', 'ISRAEL DE FREITAS MADUREIRA',
                        'JHAMES MATOS SAMPAIO', 'JOANLISE MARCO DE LEON ANDRADE', 'JOSE ANGELO BELLONI',
                        'JOSE AUGUSTO FIORUCCI', 'JULIANA BETINI FACHINI GOMES', 'LEANDRO TAVARES CORREIA',
                        'LUCAS MOREIRA', 'LUIS GUSTAVO DO AMARAL VINHA', 'MARIA TERESA LEAO COSTA', 'PETER ZORNIG', 
                        'RAUL YUKIHIRO MATSUSHITA', 'ROBERTO VILA GABRIEL', 'THAIS CARVALHO VALADARES RODRIGUES')

historico_limpo <- historico_limpo %>% 
  mutate(disciplina = str_replace(disciplina, "Das Series Temporais", "De Series Temporais"),
         disciplina = fct_collapse(disciplina, "Estatística Exploratoria" = c("Estatistica Exploratoria 1", "Estatistica Exploratoria")),
         tipo = as.factor(ifelse(disciplina %in% servico, "Serviço", "Bacharelado")),
         prof_ativo = ifelse(professor %in% professores_ativos, TRUE, FALSE))

# Colocar acento

historico_limpo <- historico_limpo %>%
  mutate(disciplina = str_replace_all(disciplina, c("tistica" = "tística", "Analise" = "Análise", "coes" = "ções", 
                                                    "cao" = "ção", "Series" = "Séries", "encia" = "ência",
                                                    "tistico" = "tístico", "tagio" = "tágio", "Historia" = "História", 
                                                    "Met" = "Mét", "casticos" = "cásticos", "Item" = "Ítem",
                                                    "Topicos" = "Tópicos", "sao" = "são", "Exploratoria" = "Exploratória")))
  
str(historico_limpo)

table(historico_limpo$disciplina)

table(historico_limpo$curso) # Aparentemente existe curso de computação (sem ser ciencia da computacao)
# Comunicacao (sem ser social), só com 10 obs, parece estar errado
# Cursos com apenas 1 obs: Teatro, Saúde Animal. Filtrar talvez? 

### Duplicadas

historico_limpo %>% count(nome, disciplina, ano, periodo) %>% filter(n > 1) %>% nrow # 18 mil casos
historico_limpo %>% get_dupes(nome, disciplina, ano, periodo)

historico_limpo %>% count(nome, disciplina, ano, periodo, horario) %>% filter(n > 1) %>% nrow # 14 mil
historico_limpo %>% get_dupes(nome, disciplina, ano, periodo, horario)

historico_limpo %>% count(nome, curso, disciplina, ano, periodo) %>% filter(n > 1) %>% nrow # 12 mil

historico_limpo %>% get_dupes(nome, disciplina, ano, periodo, horario)
historico_limpo %>% count(nome, curso, disciplina, ano, periodo, horario) %>% filter(n > 1) %>% nrow # 6 mil
historico_limpo %>% count(nome, curso, disciplina, professor, ano, periodo, horario) %>% filter(n > 1) %>% nrow # 2 mil
historico_limpo %>% count(nome, curso, disciplina, professor, ano, periodo, horario) %>% filter(n > 1) %>% nrow # 2 mil

# Aparentemente tem muitas observações que repetem pela quantidade de créditos (tem valores repetidos até 10 vezes)
# Substituir nome por matricula reduz os casos repetidos, provavelmente porque a pessoa trocou de curso/matricula

historico_limpo <- historico_limpo %>% distinct(nome, disciplina, ano, periodo, turma, mencao, .keep_all = TRUE)
# Removi considierando matrícula mais recente 
# Importante: tinha deixado antes a matricula como string, aí na hora de remover duplicadas, não removeu a certa "1100102410" < "9913246"

# Menções Diferentes
historico_limpo %>% arrange(desc(mencao)) %>% get_dupes(nome, matricula, disciplina, curso, ano, periodo, horario)

historico_limpo <- historico_limpo %>% 
  arrange(desc(mencao)) %>% 
  distinct(nome, matricula, disciplina, curso, ano, periodo, horario, .keep_all = TRUE)

# Horarios Diferentes
levels(historico_limpo$horario)

historico_limpo <- historico_limpo %>% 
  mutate(horario = fct_collapse(horario,
                                "08:00-09:50" = c("08:00-09:50", "08:00-10:00"),
                                "14:00-15:50" = c("14:00-15:50", "14:00-16:00"),
                                "16:00-17:50" = c("16:00-17:50", "16:00-18:00"),
                                "19:00-20:50" = c("19:00-20:50", "19:00-20:40"),
                                "20:50-22:30" = c("20:50-22:30", "20:50-22:40"),
                                "14:00-17:40" = c("14:00-17:40", "14:00-16:45", "14:00-17:00"))) # ultimos dois só tem 1 obs

historico_limpo %>% get_dupes(nome, disciplina, ano, periodo) # 36
historico_limpo %>% get_dupes(nome, disciplina, ano, periodo, matricula) # 0

historico_limpo <- historico_limpo %>% 
  arrange(matricula) %>% 
  distinct(nome, disciplina, ano, periodo, .keep_all = TRUE)

saveRDS(historico_limpo, "historico_limpo.rds")
  
# Disciplinas (vagas) -----------------------------------------------------

vagas <- read_excel('quantitativo_vagas.xlsx')

str(vagas)

vagas_limpo <- vagas %>% 
  transmute(
    ano = as.integer(str_sub(Semestre, 1, 4)),
    semestre = as.factor(str_sub(Semestre, -1, -1)),
    periodo = factor(paste(ano, semestre, sep = "/"), ordered = TRUE),
    disciplina_cod = as.factor(`Cód. Disciplina`),
    disciplina = as.factor(rm_accent(stri_trans_totitle(Disciplina))),
    quant_alunos_matriculados_ajuste = as.integer(`Qtde de Alunos que Conseguiram Vaga (Etapa de Ajuste)`),
    quant_alunos_nao_matriculados_ajuste = as.integer(`Quantitativo de alunos que não conseguiram se matricular por falta de vagas`)
  )

str(vagas_limpo)

table(vagas_limpo$disciplina)

servico <- c("Bioestatistica", "Estatistica Aplicada", "Probabilidade E Estatistica", "Probabilidade E Estatistica 2")

vagas_limpo <- vagas_limpo %>% 
  mutate(disciplina = str_replace(disciplina, "Das Series Temporais", "De Series Temporais"),
         disciplina = fct_collapse(disciplina, "Estatstica Exploratoria" = c("Estatistica Exploratoria 1", "Estatistica Exploratoria")),
         tipo = as.factor(ifelse(disciplina %in% servico, "Serviço", "Bacharelado")))

# Colocar acento

vagas_limpo <- vagas_limpo %>%
  mutate(disciplina = str_replace_all(disciplina, c("tistica" = "tística", "Analise" = "Análise", "coes" = "ções", 
                                                    "cao" = "ção", "Series" = "Séries", "encia" = "ência",
                                                    "tistico" = "tístico", "tagio" = "tágio", "Historia" = "História", 
                                                    "Met" = "Mét", "casticos" = "cásticos", "Item" = "Ítem",
                                                    "Topicos" = "Tópicos", "sao" = "são", "Exploratoria" = "Exploratória")))

str(vagas_limpo)

table(vagas_limpo$disciplina)

saveRDS(vagas_limpo, file = 'vagas_limpo.rds')
