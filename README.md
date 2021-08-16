## Suicídios no Brasil entre 2010 e 2019

<a href="https://rpubs.com/reisrgabriel/761964" target="_blank">**RMarkdown**</a> |
<a href="https://github.com/GabrielReisR/suicide_in_brazil/blob/main/analyses/analyses.R" target="_blank">**Código em R**</a>

![](https://github.com/GabrielReisR/suicide_in_brazil/blob/main/figures/suicides_across_years_img.png?raw=true)

#### Sobre
Esse trabalho é um projeto pessoal realizado a fim de exercitar habilidades em
visualização de dados e jornalismo de dados usando R. Todas as figuras formadas
utilizaram o pacote *ggplot2* e *plotly*. Os gifs foram feitos através do pacote
*gganimate*. Além de utilizar o *ggplot2*, os mapas foram criados a partir dos 
pacotes *geobr*, *maps* e *sf* (um agradecimento especial ao código fornecido
por Gerson Júnior e Henrique Martins na iniciativa 
<a href= "https://opencodecom.net/post/2021-04-20-criando-mapas-no-r-mundo-e-brasil/" target="_blank">Open Code Community</a>).

O objetivo principal para realização do trabalho não interferiu na qualidade de
tratamento dos dados. Os dados foram extraídos utilizando-se a ferramenta 
<a href="https://github.com/AlertaDengue/PySUS" target= "_blank">PySUS</a>,
seguindo-se o tutorial descrito <a href= "https://medium.com/psicodata/baixando-e-processando-dados-do-datasus-sobre-suic%C3%ADdio-com-python-656afa17f6ad?source=friends_link&sk=4e94866d21707aefec13aafe5923d6f1" target="_blank">aqui</a>, como 
reiterados nesse vídeo
<a href="https://www.youtube.com/watch?v=7TxlU5mgABk" target="_blank">aqui</a>.
Essa mesma metodologia foi a empregada por
<a href="https://www.scielo.br/pdf/rbp/v31s2/v31s2a07.pdf" target="_blank">Lovisi et al. (2009)</a>.

Foram baixados os dados de 2010 a 2019, contemplando 10 anos de uma série 
histórica.
Os dados foram pré-processados em Python, podendo todos os procedimentos 
iniciais de pré-processamento serem verificados <a href="https://github.com/GabrielReisR/suicide_in_brazil/blob/main/getting_pysus_data_2010_2019.ipynb" target= "_blank">neste link</a>.
A manipulação de dados para realização das análises e criação das visualizações 
foram realizadas em R, conforme descrito
<a href="https://github.com/GabrielReisR/suicide_in_brazil/blob/main/analyses/analyses.R" target="_blank">nesse documento aqui</a>.

O repositório no GitHub para esse trabalho está
<a href="https://github.com/GabrielReisR/suicide_in_brazil/" target="_blank">aqui</a>.
Todas as análises aqui descritas devem ser passíveis de replicação direta ao se
executar o código presente no repositório.

Qualquer dúvida, comentário ou retificação podem ser enviadas para 
reisrgabriel@gmail.com ou em algum link
<a href="https://linktr.ee/gabrielrr" target="_blank">aqui</a>.

#### Resumo
De forma geral, observou-se que:

* A média de suicídios ao longo de 2010 e 2019, por ano, é de 11249. De modo
geral, o número bruto de suicídios vem aumentando ao longo dos anos.
* Os meses de Dezembro, Outubro e Março, respectivamente, contaram com o maior
número de casos de suicídio.
* Ano após ano, homens (78,58%) cometeram mais suicídios do que as mulheres.
  * Não se tem dados estruturados sobre a população transgênero pelo DATASUS.
* A população que comete suicídio é majoritariamente branca (51,29%). Além
disso, somando-se os percentuais da população preta (5,42%) e parda (41,80%),
tem-se que 47,22% dos casos atinge a população negra do Brasil.
* Pessoas com ensino fundamental I completo ou ensino fundamental II incompleto
somaram 32,35% dos casos de suicídio. Após isso, 30,77% dos casos de suicídio
foram registrados em pessoas com ensino médio completo ou incompleto.
* A maioria dos registros de suicídio no Brasil entre 2010 e 2019 foram de
pessoas solteiras (54,46%).
* Aposentados/as e pensionistas, estudantes, donas de casa, trabalhadores/as
agropecuários em geral, e empregados/as doméstico nos serviços gerais foram as
profissões que mais registraram suicídios no Brasil entre 2010 e 2019.
  * Estudantes têm liderado o número de suicídios no Brasil desde 2017, seguidos
  por aposentados/as e trabalhadores/as agropecuários em geral. 
* O local mais comum para ocorrência do suicídio é no domicílio (60,89% dos
casos).
* Roraima registrou o menor número de suicídios ao longo dos últimos
10 anos (n = 402), seguido por Amapá (n = 429) e Acre (n = 508).
* São Paulo é o estado com maior número de registros de suicídio (n = 21970),
seguido por Minas Gerais (n = 13541) e Rio Grande do Sul (n = 11860).
* A maioria dos casos de suicídio não receberam nem assistência médica nem
cirurgia.

## Contato
Esse trabalho buscou dar um panorama geral sobre os casos de suicídio no Brasil
entre 2010 e 2019. Além disso, o
<a href= "https://github.com/GabrielReisR/suicide_in_brazil" target="_blank">código aberto do trabalho</a>
permite que qualquer pessoa com habilidades no R possa tentar replicar as análises
aqui descritas. De toda a forma, ressalta-se que a utilização de figuras ou 
análises aqui descritas devem ser acompanhadas da referência a esse trabalho.

Caso tenha errado em algum ponto ou me equivocado na breve explicação de algo,
adoraria receber sugestões de melhoria desse documento. Você pode falar comigo
através dos links presentes em <a href= "https://reisrgabriel.com" target= "_blank"></a>

Gostou muito do trabalho e quer apoiar o tempo dedicado a ele?
Se quiser, <a href="https://ko-fi.com/gabrielrr" target="_blank">você pode me pagar um café</a>.
