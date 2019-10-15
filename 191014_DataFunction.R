#Este Script tem como objetivo testar as principais funções da biblioteca
#lubridate, para a conversão de data.



#Instalando o pacote Tidyverse

install.packages("tidyverse")

#buscando lubridate

library(lubridate)

#buscando a data por interação no console
mydate = readline("Digite a data inicial no formato (yyyymmdd):     ")


#criando a função que traz as datas
myfunction = function(mydate){
  # Converte em data atual string em formato YYYY - MM - DD
  mydate = ymd(mydate)
  # Traz a semana em número
  week_my_date = week(mydate)
  week_today = week(today())
  hoje = ymd(today())
  
  #calcula a diferença entre a data calculada e hoje
  diff = difftime(mydate,hoje)
  
  # Set os parametros iniciais
  sem = 0
  a = c()
  
  #Enquanto a diferença de datas for menor que 0
  while(diff < 0){
    #Para cada semana são acrescentados 7 dias e incrementa
    for (x in 52) {
      sem = sem + 1
      mydate = mydate + 7
      b = week(mydate)
      #coloca todas as semanas em um vetor
      a = c(a,b)
      diff = difftime(mydate,hoje)
    }
  }
  # Retorna o número de semanas
  return(print(paste("O você ja viveu ", sem , "semanas")))
  print(a)
}

myfunction("20190101")
month(mydate,label = TRUE)

#calculando x meses para frente
mydate = "20190101"
meses_function = function(mydate){
  mydate = ymd(mydate)
  month(mydate)
  mydate = mydate + days(x=180)
  print(paste("seis meses apartir da data será : ",mydate))
  print(paste("O dia é ", day(mydate)))
  print(paste("A semana é : ", week(mydate)))
  print(paste("O mês é : ",month(mydate, labe = TRUE)))
  print(paste("O ano é : ", year(mydate)))
}
meses_function("20190510")


## Alterando Fuso horário
timezone = Sys.timezone()
time = Sys.time()
time

#Apresenta a sua hora na Tzone indicada
time = force_tz(time, tzone="America/Chicago")
time
#Apresenta a hora na timezone indicada
chicago = with_tz(time, tzone="America/Detroit")
chicago
