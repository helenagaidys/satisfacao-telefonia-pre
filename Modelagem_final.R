
# Libs --------------------------------------------------------------------

#conjunto de pacotes com propósitos gerais
if(!require(tidyverse)){install.packages("tidyverse")}

#análise exploratória 
#estatísticas de resumo úteis
if(!require(skimr)){install.packages("skimr")}

#curva ROC
if(!require(pROC)){install.packages("pROC")}

#métodos
#seleção de subconjuntos
if(!require(leaps)){install.packages("leaps")}
#regularização
if(!require(glmnet)){install.packages("glmnet")}
#gráficos da regularização
if(!require(plotmo)){install.packages("plotmo")}
#árvores
if(!require(rpart)){install.packages("rpart")}
if(!require(partykit)){install.packages("partykit")}
if(!require(rpart.plot)){install.packages("rpart.plot")}
#bagging
if(!require(ipred)){install.packages("ipred")}
#random forest
if(!require(randomForest)){install.packages("randomForest")}
#Boosting
if(!require(gbm)){install.packages("gbm")}


# Read --------------------------------------------------------------------

#df <- read_csv("data/Dataframe.csv")

df <- read_csv("/Users/nanutida2020/Desktop/Insper/Insper01-2020/Atividade integradora/Dataframe.csv")

# Explore -----------------------------------------------------------------

df %>% glimpse
skimr::skim(df)


# Ajustando dados ---------------------------------------------------------

# Trocando respostas de satisfação geral de character para factor

df$satisfacao <- factor(df$satisfacao, levels = c("Baixo","Alto"))

# Trocando respostas de Satisfação de character para factor

cols_to_change <- c('facil_entend','cumpr_prometido','fzr_rcbr_lig','quali_lig',
                    'cobranca_correta','clareza_conta','val_recarga')

for(i in cols_to_change){
  
  df[[i]] <- factor(df[[i]], levels = c("NS", "Baixo","Alto"))
}

cols_to_change_2 <- c('acesso_internet','manter_conex','veloc_internet','espera_atend',
                      'repetir_demanda','clareza_atend','quali_atend_tel','quali_atend_internet',
                      'quali_atend_loja','res_prob_cobranca','res_prob_lig','res_prob_internet')

for(i in cols_to_change_2){
  
  df[[i]] <- factor(df[[i]], levels = c("NS","Não", "Baixo","Alto"))
}

# Trocando respostas de Sim e Não de character para factor

cols_to_change_3 <- c('responsavel_recarga','servico_conc')

for(i in cols_to_change_3){
  
  df[[i]] <- factor(df[[i]], levels = c("NS","Não","Sim"))
}

# Trocando respostas de salário de character para factor

df$salario <- factor(df$salario, levels = c("SR/Rec","até 4 SM","de 4 a 6 SM","de 6 a 10 SM","acima de 10 SM"))

# Trocando respostas de faixa de idade de character para factor

df$faixa_idade <- factor(df$faixa_idade, levels = c("Recusa","Menor de 16 anos","De 16 a 17 anos","De 18 a 24 anos",
                                                    "De 25 a 30 anos","De 31 a 35 anos","De 36 a 40 anos","De 41 a 50 anos",
                                                    "De 51 a 70 anos", "Mais de 70 anos"))

# Trocando respostas de faixa de idade de character para factor

df$sexo <- factor(df$sexo, levels = c("Feminino", "Masculino"))

# Trocando respostas de operadora de character para factor

df$OPERADORA <- factor(df$OPERADORA)

# Trocando respostas de Estado de character para factor

df$ESTADO <- factor(df$ESTADO)

# Verificando se está tudo ok

df %>% glimpse
skimr::skim(df)
---------
 # Gráfico da satisfação geral
library(scales)
  
ggplot(df,aes(x=as.factor(satisfacao),y=percent())) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "blue") +
  geom_text(aes(y = ((..count..)/sum(..count..)),
                label=scales::percent((..count..)/sum(..count..))),stat="count", vjust = -0.25)+
  scale_y_continuous(labels = percent) + labs(y = "Percentual",x="Satisfação")+
  ggtitle("Satisfação Geral")+
  theme(aspect.ratio = 0.8)
  
 # theme_bw()

# Modelagem ---------------------------------------------------------------



  # Base de treino e teste --------------------------------------------------
  
  
  set.seed(123)
  treino <- sample(nrow(df), .80*nrow(df), replace = FALSE)
  
  df_train <- df[treino,]
  df_test <- df[-treino,]
    
  

  # Boosting ----------------------------------------------------------------
  
  dados_tr <- df_train
  dados_test <- df_test
  
  dados_tr$satisfacao <- ifelse(dados_tr$satisfacao == "Alto", 1, 0)
  dados_test$satisfacao <- ifelse(dados_test$satisfacao == "Alto", 1, 0)
  
  (fit_bst <- gbm(satisfacao ~ ., distribution = "bernoulli", 
                  n.trees = 1000, interaction.depth = 4, 
                  shrinkage = 0.05, data = dados_tr))
  
  summary(fit_bst)
  
  # iterações
  
  set.seed(123)
  
  fit_cv <- gbm(satisfacao ~ ., data = dados_tr, cv.folds = 5,
                n.trees = 2000, interaction.depth = 2, 
                shrinkage = 0.1)
  
  gbm.perf(fit_cv, method = "cv")
  
  # partial dependence plot
  
  plot(fit_bst, i = "cumpr_prometido", lwd = 3)
  
  # Avaliação preditiva
  
  prob_bst <- predict(fit_cv, dados_test, n.trees = 399,
                  type = "response")
  
  pred_bst <- ifelse(prob_bst >= .75, 
                     "Alto", "Baixo")
  
  # --------
  
  resultados_bst <- tibble(observado = df_test$satisfacao, 
                           predito = pred_bst,
                           probabilidade = prob_bst)
  
  # --------
    
  erro_bst <- mean(df_test$satisfacao != pred_bst)
  
  # curva ROC
  
  roc_bst <- resultados_bst %>% 
            roc(observado, probabilidade)
  
  plot(roc_bst, print.auc = TRUE, col = "blue")
   
  # Regressão logística -----------------------------------------------------
  
  # Ajustando o modelo
  fit_log <- glm(satisfacao ~ ., data = df_train, family = "binomial")
  summary(fit_log)
  
  #resultados
    #valores preditos
    prob_log <- predict(fit_log, newdata = df_test)
    
    pred_log <- ifelse(prob_log >= .75, "Alto", "Baixo")
    
    # --------
    
    resultados_log <- tibble(observado = df_test$satisfacao, 
                             predito = pred_log,
                             probabilidade = prob_log)  
    
    # --------

    erro_log <- mean(df_test$satisfacao != pred_log)
    

    
  # Regularização -----------------------------------------------------------

  x <- model.matrix(satisfacao ~ ., data = df)[,-which(names(df) %in% c("satisfacao"))]
  y <- factor(df$satisfacao) 
    
    # Ridge -------------------------------------------------------------------

    met_ridge <- glmnet::glmnet(x[treino,], y[treino], alpha = 0, nlambda = 500, family = "binomial")
    
    #resumo
    plotmo::plot_glmnet(met_ridge)
    
    #ajustando novamento o modelo com o lambda obtido a partir da validação cruzada
    set.seed(123)
    met_ridge <- glmnet::cv.glmnet(x[treino,], y[treino], alpha = 0, family = "binomial")
    
    #resumo
    plot(met_ridge)
    
    #obtendo o menor lambda
    lambda_ridge <- met_ridge$lambda.min
    
    #resultados
    
      #coeficientes
      coef_ridge <- predict(met_ridge, s = lambda_ridge, type = "coef")
      
      #predição
      prob_ridge <- predict(met_ridge, newx = x[-treino,], s = lambda_ridge, type="response")
    
      pred_ridge <- ifelse(prob_ridge >= .75, "Alto", "Baixo")
      
      # --------
      
      resultados_ridge <- tibble(observado = dados_test$satisfacao, 
                               predito = pred_ridge,
                               probabilidade = prob_ridge)
      
      # --------
      
      erro_ridge <- mean(df_test$satisfacao != pred_ridge) 


      
    # Lasso -------------------------------------------------------------------
      
    met_lasso <- glmnet::glmnet(x[treino,], y[treino], alpha = 1, nlambda = 500, family = "binomial")
      
    #resumo
    plotmo::plot_glmnet(met_lasso)
    
    #ajustando novamento o modelo com o lambda obtido a partir da validação cruzada
    set.seed(123)
    met_lasso <- glmnet::cv.glmnet(x[treino,], y[treino], alpha = 1, family = "binomial")
    
    #resumo
    plot(met_lasso)
    
    #best lambda
    lambda_lasso <- met_lasso$lambda.min
    
    #resultados
      #coeficientes
      coef_lasso <- predict(met_lasso, s = lambda_lasso, type="coef")
      
      #predição
      prob_lasso <- predict(met_lasso, newx = x[-treino,], s = lambda_lasso)
      
      pred_lasso <- ifelse(prob_lasso >= .75, "Alto", "Baixo")
      
      # --------
      
      resultados_lasso <- tibble(observado = df_test$satisfacao, 
                                 predito = pred_lasso,
                                 probabilidade = prob_lasso)
      
      # --------
      
  


# Bagging -----------------------------------------------------------------

met_bagging <- ipred::bagging(satisfacao ~ ., data = df_train, coob = TRUE)
met_bagging


#resultados
  #predição
  pred_bag <- predict(met_bagging, newdata = df_test)
  
  resultados_bag <- tibble(observado = df_test$satisfacao,
                           predito = pred_bag)
  
  # --------
  
  erro_bag <- mean(df_test$satisfacao != pred_bag)
  

# Random Forest -----------------------------------------------------------

set.seed(123)
met_rf <- randomForest::randomForest(satisfacao ~ ., data = df_train)
      
#resumo
met_rf

#importancia
randomForest::varImpPlot(met_rf, pch = 19)

#resultados
  #predição
  pred_rf <- predict(met_rf , newdata = df_test)

  resultados_rf <- tibble(observado = df_test$satisfacao, 
                           predito = pred_rf)
  # --------
  
  erro_rf <- mean(df_test$satisfacao != pred_rf)  



  erros <- c("Boosting" = 0.225057, "Regressão Logística" = 0.209392,"Ridge"=  0.229015, "Lasso"= 0.209561, "Random Forest" = 0.197043 , "Bagging" = 0.208309 , "Árvore" = 0.500880 , "Árvore + Poda" = 0.507697)
  
  erros_mod <- erros %>% 
    enframe(name= "Método", value= "erro")
  
  erros_mod  %>% arrange((erro))  
  
  #### _______
#  '''Melhores modelos (menor erro) 
#  1) Random Forest
#  2) Bagging
#  3) Regressão Logística
#  4) Lasso
#  5) Boosting  
#  6) Ridge

  
  # load Caret package for computing Confusion matrix
  library(caret) 
  confusionMatrix(xtab)
  confusionMatrix(pred_rf, df_test$satisfacao)
  CM_RF <- confusionMatrix(pred_rf, df_test$satisfacao)
  CM_RF
  print(CM_RF)
  df_test$satisfacao <- factor(df_test$satisfacao, levels = c("Baixo","Alto"))
  confusionMatrix(resultados_bst1$predito, resultados_bst1$observado)
  CM_Boost <- confusionMatrix(resultados_bst1$predito, resultados_bst1$observado)
  resultados_bag
  
  confusionMatrix(resultados_bag$predito, resultados_bag$observado)
  CM_Bag <- confusionMatrix(resultados_bag$predito, resultados_bag$observado)
  
  resultados_log
  resultados_log$predito <- factor(resultados_log$predito, levels = c("Baixo","Alto"))
  resultados_log
  
  confusionMatrix(resultados_log$predito, resultados_log$observado)
  CM_Log <- confusionMatrix(resultados_log$predito, resultados_log$observado)
  
  resultados_lasso
  resultados_lasso$predito <- factor(resultados_lasso$predito, levels = c("Baixo","Alto"))
  resultados_lasso
  
  CM_Lasso <- confusionMatrix(resultados_lasso$predito, resultados_lasso$observado)
  CM_Ridge <- confusionMatrix(resultados_ridge$predito, resultados_ridge$observado)
  resultados_ridge$predito <- factor(resultados_ridge$predito, levels = c("Baixo","Alto"))
  
  