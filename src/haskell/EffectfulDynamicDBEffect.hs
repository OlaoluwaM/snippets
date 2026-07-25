import Blammo.Logging.Setup (LoggingT, runSimpleLoggingT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Database.Esqueleto.Experimental (ConnectionPool, SqlPersistT, runSqlPool)
import Effectful (Eff, Effect, IOE, MonadIO (liftIO), (:>))
import Effectful qualified as Eff
import Effectful.Dispatch.Static qualified as Eff
import Effectful.TH qualified as Eff
import Effectful.Writer.Static.Local qualified as Eff

data DB :: Effect where
    -- LoggingT enables persistent to emit executed SQL statements to our logger automatically.
    -- ResourceT ensures the connection is checked back into the pool when the query finishes, even on exceptions.
    QueryDB :: SqlPersistT (LoggingT (ResourceT IO)) a -> DB m a

type instance Eff.DispatchOf DB = Eff.Static Eff.WithSideEffects

newtype instance Eff.StaticRep DB = QueryDB (SqlPersistT (LoggingT (ResourceT IO)) a -> m a)

runDB :: (IOE :> es, DB :> es) => ConnectionPool -> Eff es a
runDB connPool = do
    QueryDB query <- Eff.getStaticRep
    liftIO . runResourceT . runSimpleLoggingT $ runSqlPool query connPool
