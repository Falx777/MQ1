setwd("L:/caminho")

#lendo arquivo xlsx
df <- readxl::read_xlsx("GastosPublicos.xlsx")

#vendo elementos
View(head(df))

#nome das colunas
names(df)

#verificando tipo de variável em cada coluna utilizando o comando sapply
sapply(df, class)

############### 1. PADRONIZANDO O TIPO DE GASTOS PARA NUMÉRICO
#1.1 O comando for é para rodar a função interna para cada ano.
#1.2 a variável ano vai ter os valores dos nomes das colunas do df a cada loop.
for (ano in names(df)){
  #1.3 A função grepl serve para verificar se dentro das string existe a palavra "Ano". Retorna verdadeiro se houver, e executa o que tiver dentro do if. 
  if(grepl("Ano",ano)){
    #1.4 Nessa linha, todas as colunas de gastos de ano estão sendo sobre-escritas no formato double
    df[[ano]] <- as.numeric(df[[ano]])
  }
}

#1.4 Verificando novamente tipo de variável em cada coluna utilizando o comando sapply
sapply(df, class)


############### 2. Analisando as estatísticas descritivas de cada ano com a função summary
#Estatísticas: 
# Maximo - Primeiro Quartil - Mediana - Média - Terceiro Quartil - máximo - Quantidade de NA's
for (ano in names(df)){
  if(grepl("Ano",ano)){
    print(summary(df[,ano]))
  }
}

############### 3. Analisando a amplitude de cada ano

#3.1 Criando um data-frame para armazenar os valores mínimos e máximos de cada ano
amplitude <- data.frame(matrix(nrow=0,ncol=2))
#3.2 Dando os nomes das colunas
colnames(amplitude) <- c("Minimo","Maximo")

#3.3 Adicionando a amplitude em cada coluna para cada ano
for (ano in names(df)){
  if(grepl("Ano",ano)){
    #3.4 A função range retorna o valor máximo e mínimo para cada ano correspondente do df
    amplitude[ano,] <-(range(df[[ano]], na.rm = TRUE))
  }
}

View(amplitude)
#salvando arquivo em csv
write.csv2(amplitude, file="amplitude.csv")

############### 4.Analisando os municípios com menor gasto e maior gasto de cada ano
#4.1 Criando data frame para colocar o nome dos municípios
municipios <- data.frame(amplitude)
#4.2 colocando o nome das colunas
colnames(municipios) <- c("Menor Gasto", "Maior Gasto")

#4.3 adicionando o nome dos municípios com menor gasto e maior gasto de cada ano
for (ano in names(df)){
  if(grepl("Ano",ano)){
    #4.4 utilizando o which para procurar as cidades que estão na amplitude de cada ano
    municipios[ano,1] <- (df$municipio[(which(df[[ano]] <= amplitude[ano,"Minimo"] )) == df$numero])
    municipios[ano,2] <- (df$municipio[(which(df[[ano]] >= amplitude[ano,"Maximo"] )) == df$numero])
  }
}

View(municipios)
write.csv2(municipios, file="municipios.csv", fileEncoding = "latin1")

############### 5. Analisando o histograma de gastos de cada ano
for (ano in names(df)){
  if(grepl("Ano",ano)){
    
    hist(df[[ano]]/1000000, 
         main = paste("histograma de gastos – ", ano), 
         col=as.factor(df[[ano]]), 
         xlab="Gastos (Em Milhões)", 
         ylab="Frequência")
  }
}

############### 5. Analisando os quantis de gastos de cada ano

#5.1 Montando os quartis 
# 5.2 Criando um data frame para armazenar o resultado
quartis <- data.frame(matrix(ncol=5, nrow=0))
# 5.3 adicionando o nome das colunas
colnames(quartis) <- names(quantile(quartis, type = 7, na.rm = TRUE))

for (ano in names(df)){
  if(grepl("Ano",ano)){
    #5.4 aplicando a função quantile com atributo padrão de divisão, que gera o intervalo interquartil para cada ano
    quartis[ano,] <- quantile(df[[ano]], type = 7, na.rm = TRUE)
  }
}
View(quartis)
write.csv2(quartis, file="quartis.csv", fileEncoding = "latin1")


# 5.5 Criando um data frame para armazenar o resultado de decis
decis <- data.frame(matrix(ncol=11, nrow=0))
# 5.6 adicionando o nome das colunas
colnames(decis) <- names(quantile(decis, type = 7, na.rm = TRUE,probs = seq(0, 1, 0.1)))
for (ano in names(df)){
  if(grepl("Ano",ano)){
    #5.7 aplicando a função quantile com atributo de divisão de 10 em 10%, que gera o intervalo interquartil para cada ano
    decis[ano,] <- quantile(df[[ano]], type = 7, na.rm = TRUE,probs = seq(0, 1, 0.1))
  }
}

View(decis)
write.csv2(decis, file="decis.csv", fileEncoding = "latin1")

# 6. Analisando o histograma dos 25% que menos gastaram

for (ano in names(df)){
  if(grepl("Ano",ano)){
    # 6.1 Esse intervalo pega todos os gastos abaixo dos 25% (coluna 2 dos quartis)
    intervalo <- df[[ano]][(which(df[[ano]] < quartis[ano,2] ))]/1000000

    boxplot(intervalo,
            main = paste("Boxplot de gastos (25% que mais gastaram) – ", ano),
            col=as.factor(df[[ano]]),
            ylab="Gastos (Em milhoes)")
  }
}

# 7. Analisando o histograma dos 25% que mais gastaram

for (ano in names(df)){
  if(grepl("Ano",ano)){
    # 7.1 Esse intervalo pega todos os gastos acima dos 75% (coluna 4 dos quartis)
    intervalo <- df[[ano]][(which(df[[ano]] > quartis[ano,4] ))]/1000000
    boxplot(intervalo,
            main = paste("Boxplot de gastos (25% que mais gastaram) – ", ano),
                 col=as.factor(df[[ano]]),
                 ylab="Gastos (Em milhoes)")
  }
}

# 8. Analisando medidas de dispersão e tendência

medidas <- data.frame(matrix(ncol = 3, nrow=0))
colnames(medidas) <- c("Media","Mediana","desv.pad")

for (ano in names(df)){
  if(grepl("Ano",ano)){
    medidas[ano,"Media"] <- mean(df[[ano]], na.rm = TRUE)
    medidas[ano,"Mediana"] <- median(df[[ano]], na.rm = TRUE)
    medidas[ano,"desv.pad"] <- sd(df[[ano]], na.rm = TRUE)
  }
}

View(medidas)
write.csv2(medidas, file="medidas.csv", fileEncoding = "latin1")

# Gráfico em coluna da média dos gastos ao longo dos anos (Em milhões)
barplot(
     medidas$Media/1000000, 
     col=factor(medidas$Media), 
     names.arg	= rownames(medidas),
     main = "Média dos gastos ao longo dos anos (Em milhões)")

# Gráfico em coluna da mediana dos gastos ao longo dos anos (Em milhões)
barplot(
  medidas$Mediana/1000000, 
  col=factor(medidas$Mediana), 
  names.arg	= rownames(medidas),
  main = "Mediana dos gastos ao longo dos anos (Em milhões)")


# Gráfico em coluna da mediana dos gastos ao longo dos anos (Em milhões)
barplot(
  medidas$desv.pad/1000000, 
  col=factor(medidas$desv.pad), 
  names.arg	= rownames(medidas),
  main = "Desvio padrão amostral gastos ao longo dos anos (Em milhões)")

# Possíveis fontes de viéses nas estimativas:
nao.declarados <- data.frame(matrix(ncol=1, nrow=0))
colnames(nao.declarados) <- c("qtd NA")

for (ano in names(df)){
  if(grepl("Ano",ano)){
    nao.declarados[ano,1] <- length(df[[ano]][which(is.na(df[[ano]]))])
  }
}
View(nao.declarados)
