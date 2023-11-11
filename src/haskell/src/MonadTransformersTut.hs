-- {-# LANGUAGE InstanceSigs #-}

-- module MonadTransformersTut where

-- import Data.Text (Text)
-- import Data.Text qualified as Txt

-- import Control.Applicative
-- import Control.Arrow (ArrowChoice (left))
-- import Control.Monad ( guard, ap, liftM, MonadPlus(..), msum )
-- import Control.Monad.Except qualified as Ex
-- import Control.Monad.Identity (Identity)
-- import Control.Monad.State (get)
-- import Control.Monad.Trans (MonadTrans (lift))
-- import Data.Char (isAlpha, isNumber, isPunctuation)
-- import Data.Functor ((<&>))
-- import Data.Map as Map
-- import Data.Maybe (fromMaybe)
-- import Data.Text.IO qualified as TIO
-- import GHC.Conc (retry)
-- import Text.Read (readMaybe)

-- -- From: https://two-wrongs.com/a-gentle-introduction-to-monad-transformers
-- data LoginError = InvalidEmail | NoSuchUser | WrongPassword deriving (Show)

-- getDomain :: Text -> Either LoginError Text
-- getDomain email = case Txt.splitOn "@" email of
--   [_, domain] -> return domain
--   _ -> Left InvalidEmail

-- printResult :: Either LoginError Text -> IO ()
-- printResult eitherVal = let output = either (const toErrorOutput) (toSuccessOutput . tshow) eitherVal in TIO.putStrLn output
--  where
--   toErrorOutput = "ERROR: Invalid Domain"
--   toSuccessOutput = Txt.append "Domain: "

-- tshow :: (Show a) => a -> Text
-- tshow = Txt.pack . show

-- getUserToken :: IO (Either LoginError Text)
-- getUserToken = TIO.putStrLn "Enter your email address: " >> getDomain <$> TIO.getLine

-- users :: Map Text Text
-- users = Map.fromList [("example.com", "qwerty123"), ("localhost", "password")]

-- userLogin :: IO (Either LoginError Text)
-- userLogin = do
--   token <- getUserToken

--   case token of
--     Right domain ->
--       case lookupUserByDomain domain of
--         Just userpw -> do
--           TIO.putStrLn "Enter password:"
--           password <- TIO.getLine

--           if userpw == password
--             then return token
--             else return (Left WrongPassword)
--         Nothing ->
--           return (Left NoSuchUser)
--     left ->
--       return left

-- lookupUserByDomain :: Text -> Maybe Text
-- lookupUserByDomain domain = Map.lookup domain users

-- -- -------------------------------- IO Either ------------------------------- --
-- newtype IOEither e a = IOEither {runIOEither :: IO (Either e a)}

-- instance Functor (IOEither e) where
--   fmap f = IOEither . fmap (fmap f) . runIOEither

-- instance Applicative (IOEither e) where
--   pure = IOEither . return . Right

--   (<*>) :: IOEither e (a -> b) -> IOEither e a -> IOEither e b
--   (IOEither ioEFX) <*> (IOEither ioEX) = IOEither $ ioEFX `ioBind` (\ethrFx -> ioEX `ioBind` \ethrX -> return $ ethrFx `apEither` ethrX)

-- instance Monad (IOEither e) where
--   (IOEither ioE) >>= fIOE = IOEither $ ioE `ioBind` either (return . Left) (runIOEither . fIOE)

-- apEither :: Either e (a -> b) -> Either e a -> Either e b
-- apEither = (<*>)

-- ioBind :: IO a -> (a -> IO b) -> IO b
-- ioBind = (>>=)

-- getToken :: IOEither LoginError Text
-- getToken = IOEither $ TIO.putStrLn "Enter email address: " >> getDomain <$> TIO.getLine

-- liftEither :: Either e a -> IOEither e a
-- liftEither x =
--   IOEither (return x)

-- liftIO :: IO a -> IOEither e a
-- liftIO x =
--   IOEither (fmap Right x)

-- userLogin' :: IOEither LoginError Text
-- userLogin' = do
--   token <- getToken
--   userPw <- maybe (throwE' NoSuchUser) return (Map.lookup token users)
--   password <-
--     liftIO
--       ( TIO.putStrLn "Enter your password:"
--           >> TIO.getLine
--       )

--   if userPw == password then return token else throwE' WrongPassword

-- printResult' :: Either LoginError Text -> IO ()
-- printResult' res =
--   TIO.putStrLn $ case res of
--     Right token ->
--       Txt.append "Logged in with token: " token
--     Left InvalidEmail ->
--       "Invalid email address entered."
--     Left NoSuchUser ->
--       "No user with that email exists."
--     Left WrongPassword ->
--       "Wrong password."

-- throwE' :: e -> IOEither e a
-- throwE' = liftEither . Left

-- -- --------------------------- Exceptions with IO --------------------------- --
-- newtype ExceptIO e a = ExceptIO {runExceptIO :: IO (Either e a)}

-- catchE ::
--   IOEither e a ->
--   (e -> IOEither e a) ->
--   IOEither e a
-- catchE throwing handler =
--   IOEither $ do
--     result <- runIOEither throwing
--     case result of
--       Left failure ->
--         runIOEither (handler failure)
--       success ->
--         return success

-- wrongPasswordHandler :: LoginError -> IOEither LoginError Text
-- wrongPasswordHandler WrongPassword = liftIO (TIO.putStrLn "Wrong password, one more chance") >> userLogin'
-- wrongPasswordHandler err = throwE' err

-- printError :: LoginError -> IOEither LoginError a
-- printError err = do
--   liftIO . TIO.putStrLn $ case err of
--     WrongPassword ->
--       "Wrong password. No more chances."
--     NoSuchUser ->
--       "No user with that email exists."
--     InvalidEmail ->
--       "Invalid email address entered."

--   throwE' err

-- loginDialogue :: IOEither LoginError ()
-- loginDialogue = do
--   let retry' = catchE userLogin' wrongPasswordHandler
--   token <- catchE retry' printError
--   liftIO $ TIO.putStrLn $ Txt.append "Logged in with token: " token

-- -- From: https://en.wikibooks.org/wiki/Haskell/Monad_transformers

-- newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

-- instance (Monad m) => Functor (MaybeT m) where
--   fmap = liftM

-- instance (Monad m) => Applicative (MaybeT m) where
--   pure = MaybeT . return . Just
--   (<*>) = ap

-- instance (Monad m) => Monad (MaybeT m) where
--   x >>= f = MaybeT $ do
--     maybe_value <- runMaybeT x
--     case maybe_value of
--       Nothing -> return Nothing
--       Just value -> runMaybeT $ f value

-- instance (Monad m) => Alternative (MaybeT m) where
--   empty = MaybeT $ return Nothing
--   x <|> y = MaybeT $ do
--     maybe_value <- runMaybeT x
--     case maybe_value of
--       Nothing -> runMaybeT y
--       Just _ -> return maybe_value

-- instance (Monad m) => MonadPlus (MaybeT m) where
--   mzero = Control.Applicative.empty
--   mplus = (<|>)

-- instance MonadTrans MaybeT where
--   lift = MaybeT . fmap Just

-- getPassphrase :: MaybeT IO String
-- getPassphrase = do
--   s <- lift getLine
--   guard (isValid s) -- Alternative provides guard.
--   return s

-- isValid :: String -> Bool
-- isValid s =
--   length s >= 8
--     && any isAlpha s
--     && any isNumber s
--     && any isPunctuation s

-- askPassphrase :: MaybeT IO ()
-- askPassphrase = do
--   lift $ putStrLn "Insert your new passphrase:"
--   value <- getPassphrase
--   lift $ putStrLn "Storing in database..."

-- askPassphrase' :: MaybeT IO ()
-- askPassphrase' = do
--   lift $ putStrLn "Insert your new passphrase:"
--   value <- msum $ repeat getPassphrase
--   lift $ putStrLn $ "Storing in database... " ++ value

-- -- -------------------------------- Exercises ------------------------------- --
-- -- newtype Identity a = Identity {runIdentity :: a}

-- -- instance Monad Identity where
-- --   return a = Identity a
-- --   m >>= k = k (runIdentity m)

-- newtype IdentityT m a = IdentityT {runIdentity :: m a}

-- instance (Monad m) => Functor (IdentityT m) where
--   fmap = liftM

-- instance (Monad m) => Applicative (IdentityT m) where
--   pure = IdentityT . return

--   (<*>) = ap

-- instance (Monad m) => Monad (IdentityT m) where
--   (IdentityT identityMA) >>= fToIMA = IdentityT $ identityMA >>= (runIdentity . fToIMA)

-- instance MonadTrans IdentityT where
--   lift = IdentityT

-- newtype StateT s m a = StateT {runStateT :: (s -> m (a, s))}

-- -- instance (Monad m) => Monad (StateT s m) where
-- --   return a = StateT $ \s -> return (a, s)
-- --   (StateT x) >>= f = StateT $ \s -> do
-- --     (v, s') <- x s -- get new value and state
-- --     runStateT (f v) s' -- pass them to f

-- -- ---------- From https://www.youtube.com/watch?v=w9ExsWcoXPs&t=6s --------- --

-- readInt :: IO (Maybe Int)
-- readInt = getLine <&> readMaybe

-- readThreeInts :: IO (Maybe (Int, Int, Int))
-- readThreeInts = do
--   maybeInt0 <- readInt
--   case maybeInt0 of
--     Nothing -> return Nothing
--     Just int0 -> do
--       maybeInt1 <- readInt
--       case maybeInt1 of
--         Nothing -> return Nothing
--         Just int1 -> do
--           maybeInt2 <- readInt
--           case maybeInt2 of
--             Nothing -> return Nothing
--             Just int2 -> do
--               return $ Just (int0, int1, int2)

-- readThreeInts' :: IO (Maybe (Int, Int, Int))
-- readThreeInts' = runMaybeT $ do
--   int0 <- MaybeT readInt
--   int1 <- MaybeT readInt
--   int2 <- MaybeT readInt
--   return (int0, int1, int2)

-- example :: StateT Int (Ex.ExceptT String IO) ()
-- example = do
--   n <- get
--   if n < 0
--     then lift (Ex.throwError "Oh no!")
--     else lift (lift (print "Hooray!"))
