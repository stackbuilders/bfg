{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
  ( main
  )
  where

-- base
import Data.Int (Int64)

-- aeson
import qualified Data.Aeson.TH as AesonTH

-- blaze-html
import Text.Blaze.Html5 (Html)

-- either
import Control.Monad.Trans.Either (EitherT)
import qualified Control.Monad.Trans.Either as EitherT

-- monad-logger
import qualified Control.Monad.Logger as MonadLogger

-- mtl
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as ReaderT

-- pandoc
import qualified Text.Pandoc as Pandoc

-- persistent
import Database.Persist (Key)
import qualified Database.Persist as Persist
import Database.Persist.Sql
  ( ConnectionPool
#ifdef BFG_INIT
  , SqlBackend
#endif
  , SqlPersistT
  )
import qualified Database.Persist.Sql as PersistSql

-- persistent-sqlite
import qualified Database.Persist.Sqlite as PersistSqlite

-- persistent-template
import qualified Database.Persist.TH as PersistTH

-- servant
import Servant

-- servant-blaze
import Servant.HTML.Blaze (HTML)

-- servant-docs
import Servant.Docs
  ( API
  , ToCapture(..)
  , ToParam(..)
  , ToSample(..)
  )
import qualified Servant.Docs as ServantDocs

-- servant-pandoc
import qualified Servant.Docs.Pandoc as ServantPandoc

-- text
import Data.Text (Text)
import qualified Data.Text as Text

-- wai
import Network.Wai (Application)

-- wai-extra
import qualified Network.Wai.Middleware.RequestLogger as WaiExtra

-- warp
import qualified Network.Wai.Handler.Warp as Warp


----------------------------------------------------------------------
--
----------------------------------------------------------------------

PersistTH.share
  [ PersistTH.mkPersist PersistTH.sqlSettings
  , PersistTH.mkMigrate "migrateAll"
  ]
  [PersistTH.persistLowerCase|

Book
  authors [Text]
  originalTitle Text Maybe
  publisher Text
  title Text
  translator Text Maybe
  year Int64

  |]


$(AesonTH.deriveJSON AesonTH.defaultOptions ''Book)


-- |
--
--

bookKey
  :: Int64
  -> Key Book
bookKey =
  BookKey . PersistSql.SqlBackendKey


----------------------------------------------------------------------
--
----------------------------------------------------------------------

-- |
--
--

type BfgAPI =
       "api" :> Get '[HTML] Html
  :<|> "books" :> BooksAPI


-- |
--
--

type BooksAPI =
       QueryParam "author" Text :> Get '[JSON] [Book]
  :<|> ReqBody '[JSON] Book :> Post '[JSON] (Key Book)
  :<|> Capture "bookId" Int64 :>
         ( Get '[JSON] Book
      :<|> ReqBody '[JSON] Book :> Put '[JSON] ()
      :<|> Delete '[JSON] ()
         )

-- |
--
--

bfgAPIServer :: ServerT BfgAPI Bfg
bfgAPIServer =
       docsServer
  :<|> booksServer


-- |
--
--

docsServer :: Bfg Html
docsServer =
  return (Pandoc.writeHtml Pandoc.def (ServantPandoc.pandoc apiDocs))
  where
    apiDocs :: API
    apiDocs =
      ServantDocs.docs bfgAPI


-- |
--
--

booksServer :: ServerT BooksAPI Bfg
booksServer = getBooks :<|> addBook :<|> getUpdateDeleteBook
  where
    getBooks :: Maybe Text -> Bfg [Book]
    getBooks Nothing = do
      books <- runDB (Persist.selectList [] [])
      return (fmap Persist.entityVal books)

    getBooks (Just author) = do
      books <- fmap Persist.entityVal <$> runDB (Persist.selectList [] [])
      return (filter (any (Text.isInfixOf author) . bookAuthors) books)

    addBook :: Book -> Bfg (Key Book)
    addBook book =
      runDB (Persist.insert book)

    getUpdateDeleteBook key =
      getBook :<|> updateBook :<|> deleteBook

      where
        getBook :: Bfg Book
        getBook = do
          maybeBook <- runDB (Persist.get (bookKey key))
          case maybeBook of
            Nothing ->
              ReaderT.lift (EitherT.left err404)
            Just book ->
              return book

        updateBook :: Book -> Bfg ()
        updateBook book =
          runDB (Persist.replace (bookKey key) book)

        deleteBook :: Bfg ()
        deleteBook =
          runDB (Persist.delete (bookKey key))


----------------------------------------------------------------------
--
----------------------------------------------------------------------

instance ToSample Book Book where
  toSample _ =
    Just theBFG


instance ToSample [Book] [Book] where
  toSamples _ =
    [ ( "If you use ?author=Dahl"
      , [ theBFG
        ]
      )
    , ( "If you use ?author=Saramago"
      , [ alabardas
        , levantadoDelSuelo
        ]
      )
    , ( "If you don't specify anything"
      , [ alabardas
        , levantadoDelSuelo
        , theBFG
        ]
      )
    ]


instance ToSample Html Html where
  toSample _ =
    Just mempty


instance ToCapture (Capture "bookId" Int64) where
  toCapture _ =
    ServantDocs.DocCapture
      "bookId"
      "(integer) a book id"


instance ToSample () () where
  toSample _ =
    Just ()


instance ToSample (Key Book) (Key Book) where
  toSample _ =
    Just (bookKey 2)


instance ToParam (QueryParam "author" Text) where
  toParam _ =
    ServantDocs.DocQueryParam
      "author"
      ["Dahl", "Saramago", "..."]
      "Search a book given a text."
      ServantDocs.Normal


----------------------------------------------------------------------
--
----------------------------------------------------------------------

type Bfg =
  ReaderT BfgConfig (EitherT ServantErr IO)


data BfgConfig =
  BfgConfig
    { bfgConnectionPool :: ConnectionPool
    }


getBfgConnectionPool :: IO ConnectionPool
getBfgConnectionPool =
  MonadLogger.runStdoutLoggingT
    (PersistSqlite.createSqlitePool "bfg.db" 1)


runDB :: SqlPersistT IO b -> Bfg b
runDB query = do
  pool <- ReaderT.asks bfgConnectionPool
  ReaderT.liftIO (PersistSql.runSqlPool query pool)


bfgAPI :: Proxy BfgAPI
bfgAPI =
  Proxy


bfgAPIApp
  :: BfgConfig
  -> Application
bfgAPIApp config =
  serve bfgAPI (readerServer config)


readerServer :: BfgConfig -> Server BfgAPI
readerServer config =
  enter (runReaderTNat config) bfgAPIServer


main :: IO ()
main = do
  bfgConfig@BfgConfig{..} <- fmap BfgConfig getBfgConnectionPool
  PersistSql.runSqlPool
    (PersistSql.runMigration migrateAll) bfgConnectionPool
#ifdef BFG_INIT
  PersistSql.runSqlPool bfgInit bfgConnectionPool
#endif
  Warp.run 8081 (WaiExtra.logStdoutDev (bfgAPIApp bfgConfig))


#ifdef BFG_INIT

----------------------------------------------------------------------
-- Init
----------------------------------------------------------------------

bfgInit :: ReaderT SqlBackend IO ()
bfgInit =
  mapM_ (uncurry Persist.repsert) (zip keys someBooks)
  where
    keys =
      fmap bookKey [1..]

    someBooks =
      [ alabardas
      , levantadoDelSuelo
      , michelStrogoff
      , theAdventuresOfHuckleberryFinn
      , theBFG
      , todoQuedaEnCasa
      ]

#endif


----------------------------------------------------------------------
-- Some books
----------------------------------------------------------------------

alabardas :: Book
alabardas =
  Book
    { bookAuthors = ["José Saramago"]
    , bookOriginalTitle = Nothing
    , bookPublisher = "Alfaguara"
    , bookTitle = "Alabardas"
    , bookTranslator = Nothing
    , bookYear = 2014
    }


levantadoDelSuelo :: Book
levantadoDelSuelo =
  Book
    { bookAuthors = ["José Saramago"]
    , bookOriginalTitle = Just "Levantado do Chāo"
    , bookPublisher = "Punto de lectura"
    , bookTitle = "Levantado del suelo"
    , bookTranslator = Just "Basilio Losada"
    , bookYear = 1980
    }


theBFG :: Book
theBFG =
  Book
    { bookAuthors = ["Roald Dahl"]
    , bookOriginalTitle = Nothing
    , bookPublisher = "Scholastic"
    , bookTitle = "The BFG"
    , bookTranslator = Nothing
    , bookYear = 1982
    }


#ifdef BFG_INIT

michelStrogoff :: Book
michelStrogoff =
  Book
    { bookAuthors = ["Jules Verne"]
    , bookOriginalTitle = Nothing
    , bookPublisher = "Hachette"
    , bookTitle = "Michel Strogoff"
    , bookTranslator = Nothing
    , bookYear = 1876
    }


theAdventuresOfHuckleberryFinn :: Book
theAdventuresOfHuckleberryFinn =
  Book
    { bookAuthors = ["Mark Twain"]
    , bookOriginalTitle = Nothing
    , bookPublisher = "Puffin Books"
    , bookTitle = "The Adventures of Huckleberry Finn"
    , bookTranslator = Nothing
    , bookYear = 1884
    }


todoQuedaEnCasa :: Book
todoQuedaEnCasa =
  Book
    { bookAuthors = ["Alice Munro"]
    , bookOriginalTitle = Nothing
    , bookPublisher = "Lumen"
    , bookTitle = "Todo queda en casa"
    , bookTranslator = Nothing
    , bookYear = 2014
    }

#endif
