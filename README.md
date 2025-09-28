1. **Identificação**\
Nome: Artur Fardin Corrêa\
Curso: Ciência da Computação\

2. **Tema:** sistema de cadastro de servicos\
**Objetivo:** Usar o framework scotty para construir um serviço web que faça o registro de servicos e clientes com banco de dados relacionais usando SQLLITE e Haskell\

3. **Desenvolvimento:**\
    Usei o framework scotty para construir o back-end e criar as rotas que serão utilizadas no front-end para fazer as requisições, decidi criar algumas funções extras por receio do trabalho ficar muito focado em banco de dados. Algumas das bibliotecas usadas são:
  - aeson: manipulação de JSON.
  - text: manipulação de Text.
  - wai-cors: middleware cors, usado para conectar frontend com backend.
  - SQLLITE.SIMPLE: banco de dados.
  - Data.List (maximumBy) : usado para pegar o maior elemento de uma lista, usei em algumas funções.
  - Data.Ord (comparing) : Usado para comparar campos de tuplas, usei em funções.
  - Data.Text.Lazy : usei nos testes para manipular strings e converter para Text.

**Experiencia pessoal e fase de desenvolvimento**
- A primeira coisa que fiz foi criar as tabelas, precisei criar as estruturas (Cliente e Servico) para poder usar o banco de dados. Criei as rotas que seriam necessárias para o sistema funcionar e tive alguns problemas para converter os dados em JSON e deixá-los formatados corretamente.
- Criei um teste manual sem usar HUnit, é o arquivo testes.hs; criei alguns clientes e serviços como exemplo e printei os resultados das funções.
- Usei um template gerado por prompt para o front-end, só precisei ajustar algumas coisas para a conexão com o back-end, mas achei que o front-end seria realmente muito útil para facilitar os cadastros e a visualização de possíveis erros na manipulação do banco de dados.
- Achei a criação das rotas bem tranquila, ainda mais analisando os exemplos passados pela professora.
- Tive dificuldades para manipular as tuplas que as funções criavam; também tive um erro com o banco de dados que não estava permitindo inserir os dados, mas foi só apagá-lo e deixar o back-end criar outro que funcionou.
- Outro problema foram as dependências; algumas delas não estavam funcionando mesmo eu baixando, então tive que removê-las.
- Como nunca tinha feito um banco de dados relacional, achei que seria muito difícil, mas até que foi tranquilo.
- Fiquei com receio de o trabalho ficar muito focado no front-end e no banco de dados, então, para mostrar um pouco mais da linguagem funcional e Haskell, criei algumas funções extras e coloquei em uma rota separada do front-end: a rota "/estatisticas", que retorna algumas informações legais em formato JSON. Fiz isso por conta do front-end já fazer alguns cálculos e para demonstrar que seria possível fazer em Haskell também.

 4. **Orientações para execução:**
    - Instalação das dependencias: Utilize o código
    ```
     cabal install scotty text aeson sqlite-simple wai-cors wai-extra containers mtl bytestring
    ```
    
    - Use GHCI main.hs para executar o código
    - Após isso, no GHCI digite main para abrir o servidor e inicializar o banco de dados
    - Abra o navegador e vá nesse link: http://localhost:3000/
    - Para as outras rotas http://localhost:3000/servicos, http://localhost:3000/clientes, http://localhost:3000/estatisticas
    - Ctrl + C para desativar o servidor e Ctrl + D para sair do GHCI
    - Para executar a testes.hs é só dar ghci testes.hs e digitar main
    - Recomendo excluir o banco de dados, para ele ser criado quando o programa for iniciado e evitar erros

 6. **Resultado Final:**
    

https://github.com/user-attachments/assets/8c780ecf-8895-4c22-9c77-3e995fa1a891

6. **Referencias**


   [1]. https://hackage.haskell.org/package/aeson

   [2]. https://hackage.haskell.org/package/HUnit (mesmo não usando, tentei ver como fazia)

   [3]. https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:Just

   [4]. https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Maybe.html

   [5]. https://hackage.haskell.org/package/scotty

   [6]. https://claude.ai/

   [7]. https://liascript.github.io/course/?https://raw.githubusercontent.com/elc117/demo-scotty-codespace-2025b/main/README.md#1

   [8]. https://hackage.haskell.org/package/sqlite-simple

   [9]. https://danidiaz.medium.com/using-the-latest-version-of-sqlite-with-haskell-on-windows-1d6d4df2e683

   [10]. https://stackoverflow.com/questions/18808258/what-does-the-just-syntax-mean-in-haskell

   [11]. https://hoogle.haskell.org/?hoogle=maximum

   [12]. https://www.stackbuilders.com/insights/getting-started-with-haskell-projects-using-scotty/

   [13]. prompt usado para criação do front-end ("Me faça um front-end simples e funcional para cadastro de servicos e clientes")

   [14]. https://github.com/scotty-web/scotty

   [15]. https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Ord.html

   [16]. https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html

   [17]. https://hackage-content.haskell.org/package/text-2.1.3/docs/Data-Text-Lazy.html










   
   
