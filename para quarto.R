library(tidyverse)
library(data.table)
library(readxl)
library(utils)
library(archive)
library(scales)


dir.create("dados-brutos")


#options(scipen=999)




# Download e descompressão de bases ---------------------------------------

## Tabela de Recursos e Usos -----------------------------------------------
url <- "https://ftp.ibge.gov.br/Contas_Nacionais/Sistema_de_Contas_Nacionais/2021/tabelas_xls/tabelas_de_recursos_e_usos/nivel_68_2010_2021_xls.zip"
download.file(url, "dados-brutos/tru.zip")
unzip("dados-brutos/tru.zip", "68_tab1_2018.xls", exdir = "dados-brutos")
unzip("dados-brutos/tru.zip", "68_tab2_2018.xls", exdir = "dados-brutos")
unzip("dados-brutos/tru.zip", "68_tab3_2018.xls", exdir = "dados-brutos")

## POF ---------------------------------------------------------------------
url <- "https://ftp.ibge.gov.br/Orcamentos_Familiares/Pesquisa_de_Orcamentos_Familiares_2017_2018/Microdados/Dados_20230713.zip"
download.file(url, "dados-brutos/pof.zip")
unzip("dados-brutos/pof.zip", "DESPESA_INDIVIDUAL.txt", exdir = "dados-brutos")

## RAIS --------------------------------------------------------------------
url <- "ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/2018/RAIS_ESTAB_PUB.7z"
download.file(url, "dados-brutos/rais.7z", mode="wb")
archive_extract("dados-brutos/rais.7z", dir="dados-brutos/")



## PAS ---------------------------------------------------------------------
url <- "https://ftp.ibge.gov.br/Comercio_e_Servicos/Pesquisa_Anual_de_Servicos/pas2018/xls/tabelas_2018_xls_20230529.zip"
download.file(url, "dados-brutos/pas.zip")
unzip("dados-brutos/pas.zip", "Tabela 26.xlsx", exdir = "dados-brutos")







# Leitura dos dados da POF ---------------------------

C.POF <- read.fwf("dados-brutos/DESPESA_INDIVIDUAL.txt",
                  widths = c(2,4,1,9,2,1,2,2,2,7,2,10,2,2,1,1,1,12,10,1,2,14,14,10),
                  na.strings=c(" "),
                  col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE", "QUADRO", "SEQ", "V9001", "V9002", "V8000", "V9010", "V9011", "V9012", "V4104", "V4105", "DEFLATOR", "V8000_DEFLA", "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL", "RENDA_TOTAL"),
                  dec=".")
head(C.POF)
saveRDS(C.POF, "dados-brutos/POF.rds")
rm(C.POF)
gc()







rm(list=ls())
# Importação de dados -----------------------------------------------------

## Tabela de Recursos e Usos -----------------------------------------------

VP.TRU1 <- read_xls("dados-brutos/68_tab1_2018.xls", sheet = "producao",
                    skip = 3, .name_repair = "unique_quiet")
P.i <- VP.TRU1[2:129, 1][[1]]
P.nomes <- VP.TRU1[2:129, 2][[1]]
A.j <- str_sub(names(VP.TRU1)[3:70], 1, 4)
A.nomes <- str_sub(names(VP.TRU1)[3:70], 6)
VP.j <- as.vector(t(VP.TRU1[131, 3:70]))
VP.i <- VP.TRU1[2:129, 71][[1]]
VP <- VP.TRU1[131, 71][[1]]
rm(VP.TRU1)

A.nomes <- recode(A.nomes, "Refino de petróleo e coquerias"="Combustível veicular",
                           "Armazenamento, atividades auxiliares dos transportes e correio"="Estacionamentos e pedágios",
                           "Alojamento"="Hotéis e similares",
                           "Alimentação"="Serviços de alimentação",
                           "Atividades de televisão, rádio, cinema e  gravação/edição de som e imagem"="Cinema",
                           "Atividades imobiliárias"="Aluguel de imóveis",
                           "Aluguéis não-imobiliários e gestão de ativos de propriedade intelectual"="Aluguel de veículos",
                           "Outras atividades administrativas e serviços complementares"="Agenciamento de viagens",
                           "Atividades artísticas, criativas e de espetáculos"="Artes, cultura, esporte e recreação")
P.nomes <- recode(P.nomes, "Refino de petróleo e coquerias"="Combustível veicular",
                  "Armazenamento, atividades auxiliares dos transportes e correio"="Estacionamentos e pedágios",
                  "Alojamento"="Hotéis e similares",
                  "Alimentação"="Serviços de alimentação",
                  "Atividades de televisão, rádio, cinema e  gravação/edição de som e imagem"="Cinema",
                  "Atividades imobiliárias"="Aluguel de imóveis",
                  "Aluguéis não-imobiliários e gestão de ativos de propriedade intelectual"="Aluguel de veículos",
                  "Outras atividades administrativas e serviços complementares"="Agenciamento de viagens",
                  "Atividades artísticas, criativas e de espetáculos"="Artes, cultura, esporte e recreação")





M.TRU1 <- read_xls("dados-brutos/68_tab1_2018.xls", sheet = "importacao",
                   skip = 3, .name_repair = "unique_quiet")
M.i <- as.vector(t(M.TRU1[2:129, 3]))
M <- M.TRU1[131, 3][[1]]
rm(M.TRU1)

O.TRU1 <- read_xls("dados-brutos/68_tab1_2018.xls", sheet = "oferta",
                   skip = 3, .name_repair = "unique_quiet")
IP <- O.TRU1[131, 10][[1]]
#I.i <- O.TRU1[2:129, 10][[1]]
#O <- O.TRU1[131, 3][[1]]
DT.i <- O.TRU1[2:129, 3][[1]]
#MGCT.l <- as.vector(t(O.TRU1[131, 4:5]))
MGC.i <- O.TRU1[2:129, 4][[1]]
MGT.i <- O.TRU1[2:129, 5][[1]]
MGT.i
#IP.k <- as.vector(t(O.TRU1[131, 6:9]))
IP.ik <- as.matrix(O.TRU1[2:129, 6:9])
rm(O.TRU1)


CI.TRU2 <- read_xls("dados-brutos/68_tab2_2018.xls", sheet = "CI",
                    skip = 3, .name_repair = "unique_quiet")
CI.ij <- data.matrix(CI.TRU2[2:129, 3:70])
CI.j <- as.vector(t(CI.TRU2[131, 3:70]))
CI.i <- CI.TRU2[2:129, 71][[1]]
CI <- CI.TRU2[131, 71][[1]]
rm(CI.TRU2)


DF.TRU2 <- read_xls("dados-brutos/68_tab2_2018.xls", sheet = "demanda",
                    skip = 3, .name_repair = "unique_quiet")
C.i <- (DF.TRU2[2:129, 6])[[1]]
X.i <- (DF.TRU2[2:129, 3])[[1]]
C <- DF.TRU2[131, 6][[1]]
X <- DF.TRU2[131, 3][[1]]
DF <- DF.TRU2[131, 9][[1]]
rm(DF.TRU2)


VAB.TRU3 <- read_xls("dados-brutos/68_tab2_2018.xls", sheet = "VA",
                     skip = 3, .name_repair = "unique_quiet")
VAB <- VAB.TRU3[2, 70][[1]]
VAB.jm <- data.matrix(VAB.TRU3[2:15, 2:69])
VAB.jm[14,] <- VAB.jm[14,]/1000 # empregos em milhares
VAB.j <- as.vector(t(VAB.TRU3[2, 2:69]))
VAB.nomes <- VAB.TRU3[2:15, 1][[1]]
VAB.TRU3
rm(VAB.TRU3)



pas26 <- read_xlsx("dados-brutos/Tabela 26.xlsx", sheet = "Tab26a",
                   skip = 5, .name_repair = "unique_quiet")
pas26 <- data.table(pas26)
pas26 <- pas26[, c(1,8)]
names(pas26) <- c("variavel", "valor")
D.agencias <- pas26[variavel=="1 - Receita operacional líquida", valor]/1000
rm(pas26)









## Dados originais da pesquisa ---------------------------------------------

tipo.estimacao <- fread("dados-originais/tipo.estimacao.csv")
tipo.estimacao


tradutor.PT.i <- fread("dados-originais/tradutor.PT.i.csv", encoding = "UTF-8")
tradutor.PT.i
tradutor.PT.i <- tradutor.PT.i[!is.na(tru.cod)]
#PT.i <- distinct(data.table(tradutor.PT.i[tru.cod!=-1]$tru.cod))$V1
#PT.i <- sort(PT.i)
#A.j <- as.character(floor((PT.i)/10))
#PT.nomes <- distinct(data.table(tradutor.PT.i[, .(tru.cod, tru.desc)]))[order(tru.cod)][, tru.desc]


dados.pontuais <- fread("dados-originais/dados.pontuais.csv", encoding = "UTF-8")
participacao.familias.D.agencias <- dados.pontuais[variavel=="participacao.familias.D.agencias", valor]
participacao.passageiros.receita.ciasaereas <- dados.pontuais[variavel=="participacao.passageiros.receita.ciasaereas", valor]
dados.pontuais[variavel=="participacao.passageiros.receita.ciasaereas", valor]
dados.pontuais
rm(dados.pontuais)












# Tratamento preliminar de dados ----------------------------------------

#PT.i <- unique(tradutor.PT.i[, tru.cod])
#PT.i <- sort(PT.i)
#PT.i



#A.j <- as.character(floor((PT.i)/10))
#A.j



#PT.nomes <- unique(data.table(tradutor.PT.i[, .(tru.cod, tru.desc)]))[order(tru.cod)][, tru.desc]
#PT.nomes




## RAIS --------------------------------------------------------------------
rais <- fread("dados-brutos/RAIS_ESTAB_PUB.txt", encoding = "Latin-1",
              select = c("CNAE 2.0 Classe", "Qtd Vínculos Ativos", "CNAE 2.0 Subclasse"))
names(rais) <- c("cnae", "vinculos", "cnaesub")
rais <- rais[(cnae==55108 | cnae==55906)]
rais <- rais[, .(vinculos=sum(vinculos)), by=.(cnaesub)]
participacao.hoteis.alojamento <- rais[cnaesub==5510801, vinculos]/sum(rais$vinculos)
participacao.hoteis.alojamento
rm(rais)
gc()



## Tratamento de dados da POF 2017-2018 ------------------------------------
C.POF <- readRDS("dados-brutos/POF.rds")
C.POF <- data.table(C.POF)
C.POF <- left_join(C.POF, tradutor.PT.i[, .(V9001, tru.cod)], by = "V9001")
C.POF <- C.POF[(!is.na(tru.cod))]
C.POF$valor.peso <- C.POF$V8000_DEFLA*C.POF$FATOR_ANUALIZACAO*C.POF$PESO_FINAL
C.POF$turismo <- ifelse(C.POF$QUADRO==41, "CT.i.POF", "CR.i.POF")



CT.i.POF <- C.POF[, .(C=sum(valor.peso)/1000000), by = .(tru.cod, turismo)]
CT.i.POF <- dcast(CT.i.POF, tru.cod ~ turismo, value.var = "C")
CT.i.POF[is.na(CR.i.POF)]$CR.i.POF <- 0
CT.i.POF



pof.D.alojalim <- C.POF[((tru.cod==55001 | tru.cod==56001) & QUADRO==41)]
pof.D.alojalim <- dcast(pof.D.alojalim, COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE ~ tru.cod, fun.aggregate = sum,  value.var = "valor.peso")
pof.D.alojalim <- colSums(pof.D.alojalim[`55001`>0, .(`55001`, `56001`)])
prop.despesa.alim.aloj <- pof.D.alojalim[2] / pof.D.alojalim[1]
prop.despesa.alim.aloj






# Consumo de servicos de agenciamento de viagens --------------------------
DF.agencias <- D.agencias * participacao.familias.D.agencias
CI.agencias <- D.agencias - DF.agencias
DF.intermediado.agencias <- CT.i.POF[tru.cod==78802, CT.i.POF] - DF.agencias

lista.produtos.intermediacao <- tradutor.PT.i[produto.intermediacao==1, V9001]
lista.produtos.intermediacao.internacional <- tradutor.PT.i[produto.intermediacao.internacional==1, V9001]
lista.produtos.intermediacao.nacional <- tradutor.PT.i[(produto.intermediacao==1 & is.na(produto.intermediacao.internacional)), V9001]

DF.produtos.intermediacao <- C.POF[V9001 %in% lista.produtos.intermediacao, sum(valor.peso)]
DF.produtos.intermediacao.internacional <- C.POF[V9001 %in% lista.produtos.intermediacao.internacional, sum(valor.peso)]
prop.DF.produtos.intermediacao.internacional <- DF.produtos.intermediacao.internacional / DF.produtos.intermediacao
DF.intermediado.agencias.nacional <- DF.intermediado.agencias * (1 - prop.DF.produtos.intermediacao.internacional)

distribuicao.intermediacao <- C.POF[V9001 %in% lista.produtos.intermediacao.nacional, .(valor.peso=sum(valor.peso)), by = .(V9001)]
distribuicao.intermediacao <- left_join(distribuicao.intermediacao, tradutor.PT.i[V9001 %in% lista.produtos.intermediacao.nacional, c(1, 7:12)], by = "V9001")
distribuicao.intermediacao <- melt(distribuicao.intermediacao, id.vars = c("V9001", "valor.peso"))
distribuicao.intermediacao$tru.cod <- as.integer(str_remove(distribuicao.intermediacao$variable, "produto.intermediacao."))
distribuicao.intermediacao <- left_join(distribuicao.intermediacao, CT.i.POF, by = "tru.cod")
distribuicao.intermediacao$CT.i.POF <- ifelse(is.na(distribuicao.intermediacao$value), 0, distribuicao.intermediacao$CT.i.POF)
distribuicao.intermediacao <- dcast(distribuicao.intermediacao, V9001 + valor.peso ~ tru.cod, value.var = "CT.i.POF")
distribuicao.intermediacao$CT.i.POF <- rowSums(distribuicao.intermediacao[, -c(1:2)])
distribuicao.intermediacao <- melt(distribuicao.intermediacao, id.vars = c("V9001", "valor.peso", "CT.i.POF"))
distribuicao.intermediacao$prop <- distribuicao.intermediacao$value / distribuicao.intermediacao$CT.i.POF
distribuicao.intermediacao$valor.peso <- distribuicao.intermediacao$prop * distribuicao.intermediacao$valor.peso
distribuicao.intermediacao <- distribuicao.intermediacao[, .(distribuicao.intermediacao=sum(valor.peso)), by = "variable"]
distribuicao.intermediacao$variable <- as.character(distribuicao.intermediacao$variable)
distribuicao.intermediacao$distribuicao.intermediacao <- distribuicao.intermediacao$distribuicao.intermediacao / distribuicao.intermediacao[, sum(distribuicao.intermediacao)]
distribuicao.intermediacao$distribuicao.intermediacao <- distribuicao.intermediacao$distribuicao.intermediacao * DF.intermediado.agencias.nacional
distribuicao.intermediacao # Valores da despesa com agencias a ser adicionado à despesa nao intermediada de cada produto

# Consumo das familias + Servicos intermediados agencias ------------------
CT.i.POF$tru.cod <- as.character(CT.i.POF$tru.cod)
CT.i.POF <- left_join(CT.i.POF, distribuicao.intermediacao, by = c("tru.cod" = "variable"))
CT.i.POF[is.na(distribuicao.intermediacao)]$distribuicao.intermediacao <- 0
CT.i.POF$CT.i.POF <- CT.i.POF$CT.i.POF + CT.i.POF$distribuicao.intermediacao
CT.i.POF

# QPT
CT.i.POF$QPT.i <- CT.i.POF$CT.i.POF / (CT.i.POF$CR.i.POF + CT.i.POF$CT.i.POF)
CT.i.POF <- left_join(data.table(tru.cod=P.i), CT.i.POF, by="tru.cod")
QPT.i <- as.vector(CT.i.POF$QPT.i)
QPT.i[is.na(QPT.i)] <- 0
QPT.i



Tabela1 <- data.table(P.nomes, percent(QPT.i, accuracy=1))[QPT.i>0]
Tabela1




CT.i.tipoa <- C.i * QPT.i

CT.i.tipob <- as.vector(CT.i.POF$CT.i.POF)
CT.i.tipob[is.na(CT.i.tipob)] <- 0

CT.i.tipob
tipo.estimacao

tipo.estimacao <- left_join(data.table(tru.cod=P.i), tipo.estimacao, by="tru.cod")

CT.i <- ifelse(tipo.estimacao$C=="A", CT.i.tipoa,
               ifelse(tipo.estimacao$C=="B", CT.i.tipob,
                      ifelse(tipo.estimacao$C=="C", DF.agencias, 0)))


CIT.i <- ifelse(tipo.estimacao$CI=="D", CI.i * participacao.passageiros.receita.ciasaereas,
                ifelse(tipo.estimacao$CI=="E", CI.i * participacao.hoteis.alojamento,
                       ifelse(tipo.estimacao$CI=="F", CI.i[101] * participacao.hoteis.alojamento * prop.despesa.alim.aloj,
                              ifelse(tipo.estimacao$CI=="G", CI.agencias, 0))))


XT.i <- ifelse(tipo.estimacao$X=="H", X.i, 0)


MT.i <- ifelse(tipo.estimacao$X=="H", M.i, 0)



## Agregados macroeconomicos -----------------------------------------------

DFT.i <- CT.i + XT.i
DTT.i <- DFT.i + CIT.i
DTT <- sum(DTT.i, na.rm=T)

fator <- DTT.i / DT.i


MGCT.i <- MGC.i * fator
MGCT <- sum(MGCT.i, na.rm=T)
MGTT.i <- MGT.i * fator
MGTT <- sum(MGTT.i, na.rm=T)

IPT.ik <- IP.ik * fator
IPT.i <- rowSums(IPT.ik)
IPT <- sum(IPT.ik, na.rm=T)

VPT.i <- DTT.i - MGCT.i - MGTT.i - IPT.i - MT.i
VPT.j <- data.table(tru.cod=floor(as.numeric(P.i)/10), VPT.i)[, .(VPT=sum(VPT.i, na.rm=T)), by=tru.cod][, VPT]
VPT <- sum(VPT.i, na.rm=T)

MT <- sum(MT.i, na.rm=T)

CIPT.j <- (CI.j / VP.j) * VPT.j
CIPT <- sum(CIPT.j, na.rm=T)



# Valor adicionado bruto --------------------------------------------------
VABT <- VPT - CIPT
VABT.j <- VPT.j - CIPT.j

VAB.prop <- t(t(VAB.jm) / VAB.j)
VABT.jm <- t(VABT.j * t(VAB.prop))
VABT.m <- rowSums(VABT.jm)


PIBDT.Oferta <- DTT - CIPT - MGCT - MGTT - MT 
PIBDT.Oferta

PIBDT.Producao <- VPT - CIPT + IPT
PIBDT.Producao

PIBDT.Oferta == PIBDT.Producao






# Comparacoes -------------------------------------------------------------
PIB.Oferta <- VP - CI + IP
PIB.Oferta

PIB.Demanda <- DF - M
PIB.Demanda

PIB.Demanda==PIB.Oferta

CT <- sum(CT.i, na.rm=T)
CT

PIBDT.Oferta / PIB.Oferta # Participacao do turismo no PIB
CT / C # Participacao do consumo turistico das familias sobre o consumo total das familias




library(janitor)
Tabela2 <- data.table(`Categoria de produtos`=P.nomes, 
                      `Consumo turístico das famílias (CT)`=CT.i, 
                      `Exportação turística (XT)`=XT.i, 
                      `Demanda turística final (DTF)`=DFT.i, 
                      `Consumo intermediário turístico (CIT)`=CIT.i, 
                      `Demanda turística total (DT)`=DTT.i)[!is.na(DTT.i)] %>% 
  adorn_totals("row") %>% format(digits=2, big.mark = ".", decimal.mark = ",")
Tabela2

Tabela3 <- data.table(`Categoria de produtos`=P.nomes,
                      `Oferta total (O)`=DT.i,
                      `Oferta turística total (OT)`=DTT.i,
                      `Participação da oferta turística na oferta total (OT/O)`= DTT.i / DT.i)[`Oferta turística total (OT)`>0] %>% 
  adorn_totals("row") %>% format(digits=2, big.mark = ".", decimal.mark = ",")
Tabela3

Tabela4 <- cbind(data.table(`Categoria de produtos`=P.nomes,
                      `Oferta turística (OT)`=DTT.i,
                      `Margem do comércio sobre o turismo (MGCT)`=MGCT.i,
                      `Margem do transporte sobre o turismo (MGTT)`=MGTT.i,
                      `Imposto de importação sobre o turismo`=IPT.ik[,1],
                      `IPI sobre o turismo`=IPT.ik[,2],
                      `ICMS sobre o turismo`=IPT.ik[,3],
                      `Outros impostos menos subsídios sobre o turismo`=IPT.ik[,4],
                      `Total de impostos líquidos de subsídios sobre o turismo (IPT)`=IPT.i,
                      `Valor total da produção turística (VPT)`=VPT.i,
                      `Importação turística (MT)`=MT.i)[`Oferta turística (OT)`>0],
                 CIPT.j[CIPT.j>0]) %>% 
  adorn_totals("row") %>% format(digits=1, big.mark = ".", decimal.mark = ",")

                 
Tabela5 <- data.table(Operação=VAB.nomes, `Valor (R$ milhões)`=VABT.m) %>% 
  format(digits=1, big.mark = ".", decimal.mark = ",")
Tabela5



Tabela6 <- data.table(`Atividade turística`=A.nomes, 
                      `Valor Adicionado Bruto Direto do Turismo (VABDT / R$ milhões)`=VABT.j,
                      `Ocupações (milhares)`=VABT.jm[14,]) %>% format(digits = 1, big.mark = ".", decimal.mark = ",")

Tabela6

