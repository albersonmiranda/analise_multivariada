### code to prepare `rais` dataset ##

# set Google BigQuery project
basedosdados::set_billing_id("monografia-359922")

# importando dados da Rais
rais = basedosdados::bdplyr("br_me_rais.microdados_vinculos") |>
  # filtrando dados
  dplyr::filter(
    # Espírito Santo
    sigla_uf == "ES" &
      # anos de 2006 e 2022
      ano %in% c(2006, 2014, 2022) &
      # sem vazios em remuneração
      !is.na(valor_remuneracao_media) &
      # sem vazios em raça/cor
      !is.na(raca_cor) & raca_cor != "9" &
      # sem vazios em sexo
      !is.na(sexo) &
      # sem vazios em ocupação
      !is.na(cbo_2002) &
      # valores positivos para remuneração
      valor_remuneracao_media > 0
  ) |>
  # definindo grau de instrução
  dplyr::mutate(
    grau_instrucao = ifelse(ano <= 2005, grau_instrucao_1985_2005, grau_instrucao_apos_2005)
  ) |>
  # escolhendo colunas
  dplyr::select(
    ano,
    id_municipio,
    sexo,
    raca_cor,
    valor_remuneracao_media,
    idade,
    grau_instrucao,
    cbo_2002
  ) |>
  # grau de instrução preenchido
  dplyr::filter(!is.na(grau_instrucao)) |>
  # coletando dados
  basedosdados::bd_collect()

# dicionário Rais
rais = within(rais, {
  grau_instrucao = ifelse(grau_instrucao == "1", "analfabeto", grau_instrucao)
  grau_instrucao = ifelse(grau_instrucao == "2", "fund_I_incompleto", grau_instrucao)
  grau_instrucao = ifelse(grau_instrucao == "3", "fund_I_completo", grau_instrucao)
  grau_instrucao = ifelse(grau_instrucao == "4", "fund_II_incompleto", grau_instrucao)
  grau_instrucao = ifelse(grau_instrucao == "5", "fund_II_completo", grau_instrucao)
  grau_instrucao = ifelse(grau_instrucao == "6", "medio_incompleto", grau_instrucao)
  grau_instrucao = ifelse(grau_instrucao == "7", "medio_completo", grau_instrucao)
  grau_instrucao = ifelse(grau_instrucao == "8", "superior_incompleto", grau_instrucao)
  grau_instrucao = ifelse(grau_instrucao == "9", "superior_completo", grau_instrucao)
  grau_instrucao = ifelse(grau_instrucao == "10", "mestrado", grau_instrucao)
  grau_instrucao = ifelse(grau_instrucao == "11", "doutorado", grau_instrucao)
  sexo = ifelse(sexo == "1", "masculino", sexo)
  sexo = ifelse(sexo == "2", "feminino", sexo)
  raca_cor = ifelse(raca_cor == "1", "indigena", raca_cor)
  raca_cor = ifelse(raca_cor == "2", "branca", raca_cor)
  raca_cor = ifelse(raca_cor == "4", "preta", raca_cor)
  raca_cor = ifelse(raca_cor == "6", "amarela", raca_cor)
  raca_cor = ifelse(raca_cor == "8", "parda", raca_cor)
  raca_cor = ifelse(raca_cor == "9", "nao_informado", raca_cor)
  sexo = relevel(as.factor(sexo), "feminino")
  raca_cor = relevel(as.factor(raca_cor), "preta")
  cbo = relevel(as.factor(substr(cbo_2002, 1, 2)), "76")
  cbo = cbo_2002
  cbo_2002 = NULL
  vlr_rem = valor_remuneracao_media
  valor_remuneracao_media = NULL
  regiao = ifelse(
    id_municipio %in% c("3205309", "3205200", "3205002", "3201308"),
    "gv",
    "interior"
  )
  regiao = relevel(as.factor(regiao), "interior")
})

# agregando em ciclos completos
rais = within(rais, {
  grau_instrucao = ifelse(grau_instrucao %in% c("analfabeto", "fund_I_incompleto"), "nenhum", grau_instrucao)
  grau_instrucao = ifelse(grau_instrucao %in% c("fund_II_incompleto", "fund_I_completo"), "fund_I", grau_instrucao)
  grau_instrucao = ifelse(grau_instrucao %in% c("fund_II_completo", "medio_incompleto"), "fund_II", grau_instrucao)
  grau_instrucao = ifelse(grau_instrucao %in% c("medio_completo", "superior_incompleto"), "medio", grau_instrucao)
  grau_instrucao = ifelse(grau_instrucao %in% c("superior_completo"), "superior", grau_instrucao)
  grau = relevel(as.factor(grau_instrucao), "nenhum")
  grau_instrucao = NULL
})

# adicionando experiência
rais = within(rais, {
  exp = ifelse(grau == "nenhum", idade - 6, NA)
  exp = ifelse(grau == "fund_I", idade - 10, exp)
  exp = ifelse(grau == "fund_II", idade - 14, exp)
  exp = ifelse(grau == "medio", idade - 17, exp)
  exp = ifelse(grau == "superior", idade - 21, exp)
  exp = ifelse(grau == "mestrado", idade - 23, exp)
  exp = ifelse(grau == "doutorado", idade - 27, exp)
})

# deflacionando salários pelo IPCA
deflator_2022 <- 2615.05 / 6474.09
deflator_2014 <- 2615.05 / 4059.86

rais$vlr_rem = ifelse(rais$ano == 2014, rais$vlr_rem * deflator_2014, rais$vlr_rem)
rais$vlr_rem = ifelse(rais$ano == 2022, rais$vlr_rem * deflator_2022, rais$vlr_rem)

# salvando dataframe
if (!dir.exists("data")) {
  dir.create("data")
}

saveRDS(rais, "data/rais.rds", compress = "xz")
