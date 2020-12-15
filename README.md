## MODELO DE GESTÃO DOS CONTRATOS DE ENERGIA DE CONSUMIDORES – PREVISÃO E OTIMIZAÇÃO DA CONTRATAÇÃO DE DEMANDA DE ENERGIA ELÉTRICA

Para os consumidores do Grupo A, no que tange o uso do sistema de distribuição, é necessário contratar da distribuidora de energia um valor de demanda de potência para
atender sua carga. Por sua vez, a distribuidora deve garantir que sua rede de distribuição tenha capacidade de fornecer o valor de demanda contratado.
A definição do valor da demanda a ser contratada é uma tarefa complexa, sendo que a demanda medida mensalmente pela distribuidora não será necessariamente o
valor contratado. Em alguns meses, a demanda medida será maior do que a contratada, podendo implicar no pagamento de multas de ultrapassagem. Em outros meses, a demanda será menor, gerando custos desnecessários já que o consumidor pagará pela demanda
contratada, ou seja, pagar por uma demanda não utilizada. O objetivo da contratação de demanda é definir um valor que ao longo do período de contrato gere o menor custo para
o consumidor. Muitas vezes as empresas não possuem pessoal especializado para definir o melhor valor da demanda a ser contratada, o que pode implicar em custos desnecessários.
No âmbito da Administração Pública Federal, BRASIL (2020) realizou a avaliação de 664 contratos de energia e constatou que em aproximadamente 75% destes contratos não houve
alteração no valor da demanda contratada no período de janeiro de 2015 a março de 2019.

Para facilitar o processo de definição do valor da demanda a ser contratada, em 2019, foi oficialmente lançado o projeto de Modelo de Avaliação dos Contratos de 
Demanda de Energia (MACDE) com aporte de recursos da Secretaria de Educação Profissional e Tecnológica (Setec), dentro do Programa Energif, ligado à rede de Instituições
Federais de Educação Profissional e Tecnológica. O principal objetivo do projeto foi criar uma ferramenta web, de fácil utilização, para auxiliar no planejamento da contratação
de demanda de energia. Diante da importância da ferramenta MACDE, o Secretário da Secretaria de Educação Profissional e Tecnológica (SETEC) publicou o OFÍCIOCIRCULAR Nº 82/2019/GAB/SETEC/SETEC-MEC 2 solicitando o apoio para a ampla
divulgação do MACDE na rede federal de ensino.

A ferramenta MACDE está disponível no seguinte endereço:
https://gese.florianopolis.ifsc.edu.br/macde/

### 1. Justificativa
A justificativa deste trabalho é a necessidade de uma metodologia que auxilie os responsáveis pelos contratos de energia a contratarem um valor mais eficiente de demanda,
fornecendo uma revisão sobre tema, norteando sobre a legislação atual e com a aplicação de uma ferramenta de uso fácil. 

### 2. Objetivo Geral
Reduzir os gastos dos consumidores do Grupo A, especialmente da Administração pública Federal, auxiliando-os na definição do valor de demanda a ser contratado, tornando o gerenciamento de contratos mais eficiente.

###  3. Objetivos Específicos
- Revisar a regulação do Sistema Elétrico Brasileiro com ênfase na contratação de
demanda de potência ativa dos consumidores do Grupo A;
- Apresentar a metodologia utilizada na ferramenta MACDE;
- Apresentar metodologias para a previsão de demanda;
- Apresentar metodologias para a otimização do valor de demanda a ser contratado;

### 4. Informações
A aplicação foi desenvolvida no Rstudio. Para a aplicação funcionar corretamente é necessário definir o diretório de trabalho como sendo a pasta raiz desta aplicação.

Este repositório é resultado da Dissertação de Mestrado submetida ao Instituto Federal de Educação, Ciência e Tecnologia de Santa Catarina como parte dos requisitos para
obtenção do título de Mestre em Engenharia Elétrica.

**Mestrando**: Dyego de Campos\
**Orientador**: Rubipiara Cavalcante Fernandes, D. Eng.\
**Coorientador**: Daniel Tenfen, D. Eng

###  5. Banco de dados
Os bancos de dados utilizados estão armazenados na pasta dataframes e serão explicados a seguir. Os dados são referentes ao dados de demanda de potência e energia ativa de 6 câmpus do Instituto Federal de Educação, Ciência e Tecnologia de Santa Catarina. Os câmpus são identificados por códigos: c1, c2, c2a, c3, c3a, c4, c5, c5a, c6. O código com a letra a no final indica que os valores da geração fotovoltaica foi considerada.

A pasta generation, contém os arquivos .csv com informações sobre os valores gerados nos câmpus que possuem sistema fotovoltaico instalado. Como os sistemas fotovoltaicos instalados nos câmpus analisados são iguais, as simulações consideram apenas os valores reais do câmpus c3 para energia, sendo aplicado aos demais. Para demanda o valor considerado foi a metade da capacidade instalada. O arquivo data_ifsc, dentro da pasta generation, contém os dados referentes aos câmpus do IFSC utilizados, onde NA representa os dados faltantes.

- Descrição do dataframe energies_c3, colunas 1 a 4:
1. month - mês de referência;
2. 2018_MW - energia gerada em 2018;
3. 2019_MW - energia gerada em 2019;
4. mean - média das energias geradas.

- Descrição do dataframe generics_demands, colunas 1 a 2:
1. month - mês de referência;
2. d_kw - demanda considerada;

- Descrição do dataframe data_ifsc, colunas 1 a 8:
1. ref - referência ao câmpus;
2. year - ano de referência;
3. month - mês de referência;
4. pkd - demanda de ponta;
5. opd - demanda fora de ponta;
6. pke - energia de ponta;
7. ope - energia fora de ponta;
8. ctrd - demanda contratada.

A pasta macde, contém os arquivos .csv com o padrão utilizado na ferramenta MACDE. Estes arquivos podem ser carregados diretamente na aplicação https://gese.florianopolis.ifsc.edu.br/macde_simulador/. Os dados estão separados por câmpus e histórico. Por exemplo, o arquivo mcd_c2_2016_2018 é referente aos valores câmpus c2, com histórico de três anos (2016-2018) para previsão do ano de 2019.

A pasta nnq_forecast, contém os arquvios .csv no padrão utilizado na aplicação NNQ Forecast https://estatistica.inf.ufsc.br/nnq/forecast/. Os arquivos estão separados por câmpus, por posto tarifário e grandeza. Por exemplo, o arquivo nnq_c1_opd, possui informações da demanda fora de ponta do câmpus c1.

A legenda dos gráficos é gerada automaticamente com base nas informações do arquivo graphicsmessages, que contém as mensagens em dois idiomas: português (pt-br) e inglês (en).

