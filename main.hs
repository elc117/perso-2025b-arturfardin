{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import GHC.Generics (Generic)
-- import Network.Wai.Handler.Warp (HostPreference, defaultSettings, setHost, setPort)

import Network.Wai.Middleware.Cors (cors, simpleCors, simpleCorsResourcePolicy)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Scotty

-- Ainda não sei se vou usar todos esses imports, mas é bom ter eles prontos caso eu precise
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
    dataServico :: Text
  }
  deriving (Show, Generic)

instance ToJSON Cliente

instance FromJSON Cliente

instance ToJSON Servico

instance FromJSON Servico

instance FromRow Servico where
  fromRow = Servico <$> field <*> field <*> field <*> field <*> field

instance ToRow Servico where
  toRow (Servico id clienteId descricao valor dataServico) = toRow (id, clienteId, descricao, valor, dataServico)

instance FromRow Cliente where
  fromRow = Cliente <$> field <*> field <*> field

instance ToRow Cliente where
  toRow (Cliente id nome telefone) = toRow (id, nome, telefone)

calculovalortotal :: [Servico] -> Double
calculovalortotal servicos = sum (map valor servicos) -- Calcular o valor total dos serviços

initDB :: Connection -> IO ()
initDB conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS clientes (clienteid INTEGER PRIMARY KEY AUTOINCREMENT, nome TEXT, telefone TEXT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS servicos (servicoid INTEGER PRIMARY KEY AUTOINCREMENT, cliente_id INTEGER, descricao TEXT, valor REAL, dataServico TEXT, FOREIGN KEY(cliente_id) REFERENCES clientes(clienteid))"

main :: IO ()
main = do
  conn <- open "bdados.db"
  initDB conn
  scotty 3000 $ do
    middleware $ cors $ const $ Just simpleCorsResourcePolicy
    get "/" $ do
      file "cadastro.html"

    get "/servicos/:id" $ do
      -- Buscar no banco de dados
      text "Buscar serviço por ID"

    get "/clientes" $ do
      clientes <- liftIO $ query_ conn "SELECT * FROM clientes" :: ActionM [Cliente]
      json clientes

    post "/cadastro/:nome/:telefone" $ do
      nome <- pathParam "nome" :: ActionM Text
      telefone <- pathParam "telefone" :: ActionM Text
      liftIO $ execute conn "INSERT INTO clientes (nome, telefone) VALUES (?, ?)" (nome, telefone)

    get "/servicos" $ do
      servicos <- liftIO $ query_ conn "SELECT * FROM servicos" :: ActionM [Servico]
      json servicos

    post "/servicos" $ do
      -- Inserir no banco de dados
      servico <- jsonData :: ActionM Servico
      liftIO $ execute conn "INSERT INTO servicos (cliente_id, descricao, valor, data_servico) VALUES (?, ?, ?, ?)" (cliente_id servico, descricao servico, valor servico, dataServico servico)

    delete "/servicos/:id" $ do
      text "Em fase de construção"

-- Deletar do banco de dados
