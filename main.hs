{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, object, (.=))
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import GHC.Generics (Generic)
import Network.Wai.Middleware.Cors (cors, simpleCors, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty
import System.Console.GetOpt (ArgDescr(NoArg))
-- Acabei não usando todas,  mas como pretendo fazer upgrades mesmo após o envio, vou deixar pra caso precise 

data Cliente = Cliente
  { clienteid :: Maybe Int,
    nome :: Text,
    telefone :: Text
  }
  deriving (Show, Generic)

data Servico = Servico
  { servicoid :: Maybe Int,
    cliente_id :: Int,
    descricao :: Text,
    valor :: Double,
    data_servico :: Text
  }
  deriving (Show, Generic)

instance ToJSON Cliente
instance FromJSON Cliente
instance ToJSON Servico
instance FromJSON Servico

instance FromRow Servico where
  fromRow = Servico <$> field <*> field <*> field <*> field <*> field
instance ToRow Servico where
  toRow (Servico id clienteId descricao valor data_servico) = toRow (id, clienteId, descricao, valor, data_servico)
instance FromRow Cliente where
  fromRow = Cliente <$> field <*> field <*> field
instance ToRow Cliente where
  toRow (Cliente id nome telefone) = toRow (id, nome, telefone)

-- Função para calcular o valor total de um servico dado o id do cliente, funciona ja que a tabela de servicos e relacionada pelo id do cliente com a tabela de clientes
calculovalortotal :: Int -> [Servico] -> Double
calculovalortotal id servicos = sum [valor s | s <- servicos, cliente_id s == id] -- Calcular o valor total dos servicos de um cliente

-- Função para calcular a quantidade de servicos de um determinado cliente
contarservicosporcliente :: Int -> [Servico] -> Int
contarservicosporcliente id servicos = length [s | s <- servicos, cliente_id s == id]

-- Função para calcular a quantidade de servicos por cliente, aproveita e calcula o valor total também
quantidadeporcliente :: [Cliente] -> [Servico] -> [(Cliente, Int, Double)]
quantidadeporcliente clientes servicos = [(c, contarservicosporcliente (fromMaybe 0 (clienteid c)) servicos, calculovalortotal (fromMaybe 0 (clienteid c)) servicos) | c <- clientes]

-- Função para calcular o cliente com mais servicos
clientecommaisservicos :: [(Cliente, Int, Double)] -> Maybe (Cliente, Int)
clientecommaisservicos [] = Nothing
clientecommaisservicos lista = Just $ let (c, n, _) = maximumBy (comparing (\(_, n, _) -> n)) lista in (c, n)

-- Função para ver o total faturado
calculafaturamentototal :: [Servico] -> Double
calculafaturamentototal servico = sum [valor s | s <- servico]

-- Cliente que mais gastou
clientequemaisgastou :: [(Cliente, Int, Double)] -> Maybe (Cliente, Double)
clientequemaisgastou [] = Nothing
clientequemaisgastou lista = let (c, _, total) = maximumBy (comparing (\(_, _, t)-> t)) lista in Just (c, total)

-- Serviço mais caro
servicomaiscaro :: [Servico] -> Maybe Servico
servicomaiscaro [] = Nothing
servicomaiscaro servico = Just (maximumBy (comparing valor) servico)

-- inicialização do banco de dados
initDB :: Connection -> IO ()
initDB conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS clientes (clienteid INTEGER PRIMARY KEY AUTOINCREMENT, nome TEXT, telefone TEXT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS servicos (servicoid INTEGER PRIMARY KEY AUTOINCREMENT, cliente_id INTEGER, descricao TEXT, valor REAL, data_servico TEXT, FOREIGN KEY(cliente_id) REFERENCES clientes(clienteid))"



main :: IO ()
main = do
  conn <- open "bdados.db"
  initDB conn
  scotty 3000 $ do
    middleware $ cors $ const $ Just simpleCorsResourcePolicy
    middleware logStdoutDev
    get "/" $ do
      file "cadastro.html"

-- funções extras, algumas estatisticas divertidas
    get "/estatisticas" $ do
      servicos <- liftIO $ query_ conn "SELECT * FROM servicos" :: ActionM [Servico]
      clientes <- liftIO $ query_ conn "SELECT * FROM clientes" :: ActionM [Cliente]
      let clientetop = clientecommaisservicos (quantidadeporcliente clientes servicos)
      let clientegastador = clientequemaisgastou (quantidadeporcliente clientes servicos)
      let servicocaro = servicomaiscaro servicos
      let faturamentototal = calculafaturamentototal servicos
     
     -- Só pra deixar o objeto mais "bonito"
      json $ object
        [ "faturamento_total" .= faturamentototal
        , "cliente_top_servicos" .= case clientetop of
            Just (c, qtd) -> object -- Separa o retorno da função clientecommaisservicos em variaveis separadas, c = [Cliente] e qtd = Int (quantidade de servicos)
              [ "clienteid"   .= clienteid c
              , "nome"        .= nome c
              , "telefone"    .= telefone c
              , "servicos"   .= qtd
              ]
            Nothing -> object [] -- caso não tenha cliente retorna um json vazio
        , "cliente_gastador" .= case clientegastador of
            Just (c, total) -> object -- Separa o retorno da função clientequemais gastou nas informações do cliente [Cliente] e o tanto que o cliente gastou
              [ "clienteid"   .= clienteid c
              , "nome"        .= nome c
              , "telefone"    .= telefone c
              , "valor_gasto" .= total
              ]
            Nothing -> object []
        , "servico_mais_caro" .= servicocaro -- retorna o servico mais caro e suas informações em formato json
        ]

    -- Rota para mostrar todos clientes cadastrados
    get "/clientes" $ do
      clientes <- liftIO $ query_ conn "SELECT * FROM clientes" :: ActionM [Cliente]
      json clientes
    -- Rota de cadastro de clientes
    post "/cadastro/:nome/:telefone" $ do
      nome <- pathParam "nome" :: ActionM Text
      telefone <- pathParam "telefone" :: ActionM Text
      liftIO $ execute conn "INSERT INTO clientes (nome, telefone) VALUES (?, ?)" (nome, telefone)
    -- Rota para mostrar todos servicos cadastrados
    get "/servicos" $ do
      servicos <- liftIO $ query_ conn "SELECT * FROM servicos" :: ActionM [Servico]
      json servicos
    post "/servicos" $ do
      -- Inserir no banco de dados
      servico <- jsonData :: ActionM Servico
      liftIO $ execute conn "INSERT INTO servicos (cliente_id, descricao, valor, data_servico) VALUES (?, ?, ?, ?)" (cliente_id servico, descricao servico, valor servico, data_servico servico)

    delete "/servicos/delete/:id" $ do
      -- Deletar servico do banco de dados
      idparam <- pathParam "id" :: ActionM Int
      liftIO $ execute conn "DELETE FROM servicos WHERE servicoid = ?" (Only idparam)
      

    delete "/clientes/delete/:id" $ do
      -- Deletar cliente do banco de dados, se deletar o cliente, deleta todos servicos linkados com o id do cliente
      idparam <- pathParam "id" :: ActionM Int
      liftIO $ execute conn "DELETE FROM clientes WHERE clienteid = ?" (Only idparam)
      liftIO $ execute conn "DELETE FROM servicos WHERE cliente_id = ?" (Only idparam)
      


