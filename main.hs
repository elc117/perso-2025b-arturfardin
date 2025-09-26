{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import GHC.Generics (Generic)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
-- import Network.Wai.Handler.Warp (HostPreference, defaultSettings, setHost, setPort)
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

instance FromRow Cliente where
  fromRow = Cliente <$> field <*> field <*> field

instance ToRow Cliente where
  toRow (Cliente id nome telefone) = toRow (id, nome, telefone)

initDB :: Connection -> IO ()
initDB conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS clientes (clienteid INTEGER PRIMARY KEY AUTOINCREMENT, nome TEXT, telefone TEXT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS servicos (servicoid INTEGER PRIMARY KEY AUTOINCREMENT, cliente_id INTEGER, descricao TEXT, valor REAL, data_servico TEXT, FOREIGN KEY(cliente_id) REFERENCES clientes(clienteid))"

main :: IO ()
main = do
  conn <- open "bdados.db"
  initDB conn
  scotty 3000 $ do
    middleware logStdoutDev

    get "/servicos/:id" $ do
      -- Buscar no banco de dados
      text "Buscar serviço por ID"

    post "/servicos/cadastro" $ do
      -- Rota de listar serviços
      text "Cadastro de serviços"

    get "/servicos" $ do
      -- Colocar no banco de dados
      text "Listar serviços"

    delete "/servicos/:id" $ do
      -- Deletar do banco de dados
      text "Deletar serviço"
