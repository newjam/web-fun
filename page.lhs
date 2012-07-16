> {-# LANGUAGE OverloadedStrings #-}


> import Data.Typeable

 import Network.HTTP.Conduit

 import Network.URI

> import Control.Monad
> import Control.Applicative

> import Control.Monad.Trans


> import           Web.Scotty             as S
> import           Data.Aeson             as A
> import qualified Data.ByteString        as B
> import qualified Data.ByteString.Char8  as C
> import qualified Database.Redis         as R
> import qualified Data.Text.Lazy         as T
> import qualified Data.ByteString.Lazy   as LBS

import qualified Text.Blaze             as HHHH

> import qualified Text.Blaze.Html5       as H
> import           Text.Blaze.Html5 ((!))
> import qualified Text.Blaze.Html5.Attributes as A
> import           Text.Blaze.Renderer.Text (renderHtml)
> import           Data.Maybe

import           Data.Encoding

> import Data.Monoid (mconcat)

it's a little annoying that foo :: (Either (Maybe ByteString))
I wish I were in an error monad or something

 main = do
   conn <- R.connect R.defaultConnectInfo
   foo <- liftIO $ R.runRedis conn (R.get "bar")
   print foo

cool, this could easily use the Aeson package to retrieve structured information from the returned values.

> data Post = Post {pName::T.Text, pBody::T.Text, pImg::Maybe T.Text}
>  deriving (Eq, Show)

> instance FromJSON Post where
>   parseJSON (Object o) = Post <$> o .: "name" <*> o .: "body" <*> o .:? "img"
>   parseJSON _          = mzero

> instance ToJSON Post where
>   toJSON (Post name body (Just img)) = object ["name" .= name, "body" .= body, "img" .= img]
>   toJSON (Post name body Nothing) = object ["name" .= name, "body" .= body]

> instance H.ToHtml Post where
>   toHtml (Post name body _) = H.div $ do
>     H.toHtml (T.append "name: " name) 
>     H.br
>     H.toHtml body

I could use MVar, but then I wouldn't get persistence

simple get/set REST api for redis 

> foo :: R.Connection -> R.Redis (Either R.Reply b) -> ActionM b
> foo conn redis = do
>   result <- liftIO $ R.runRedis conn redis
>   case result of 
>     Left err -> raise . T.pack . show $ err
>     Right x -> return x

> main = scotty 3000 $ do
>   get "/posts" $ do
>     conn <- liftIO $ R.connect R.defaultConnectInfo
>     mPostCount <- foo conn $ R.get "posts:count"
>     let postCount = maybe 0 (read . C.unpack)  
>     let postIds x = x : postIds (x-1)
>     let postKeys = map (\id -> mkKey "posts" id) . postIds
>     mposts <- foo conn . R.mget . take 10 . postKeys . postCount $ mPostCount 
>     let posts = catMaybes . map (>>= decode) . map (fmap (\x->LBS.fromChunks [x])) $ mposts :: [Post]
>     html . renderHtml . topLevel . sequence_ . map H.toHtml $ posts
>   post "/posts/new" $ do
>     name <- param "name"
>     body <- param "body"
>     let post = Post name body Nothing
>     conn <- liftIO $ R.connect R.defaultConnectInfo
>     id <- foo conn $ R.incr "posts:count"
>     foo conn $ R.set (mkKey "posts" id) (B.concat . LBS.toChunks . A.encode $ post)
>     text . T.pack . show $ id
>   get "/posts/:id" $ do
>     id <- param "id" 
>     --liftIO . putStrLn $ "/posts/" ++ show id
>     conn <- liftIO $ R.connect R.defaultConnectInfo
>     post <- foo conn $ R.get (mkKey "posts" id)
>     S.json post
>   get "/posts/new" $ do
>     --liftIO . putStrLn $ "/posts/new"
>     --text "bleh"
>     html . renderHtml . topLevel $ do
>       H.form ! A.id "login" ! A.method "post" ! A.action "/posts/new" $ do
>         H.text "name: "
>         H.input ! A.type_ "text" ! A.name "name"
>         H.br
>         H.text "post: "
>         H.textarea ! A.name "body" ! A.rows "4" $ H.text ""
>         H.br
>         H.input ! A.type_ "submit"
 


   get "/get/:key" $ do
     key <- param "key"
     conn <- liftIO $ R.connect R.defaultConnectInfo
     mval <- foo conn (R.get key)
     case mval of
       Just val  -> html . T.pack . C.unpack $ val
       Nothing -> html "error"
   get "/set/:key/:val" $ do
     key <- param "key"
     val <- param "val"
     conn <- liftIO $ R.connect R.defaultConnectInfo
     r <- liftIO $ R.runRedis conn (R.set key val)
     case r of 
       Right R.Ok -> html "ok"
       _        -> html "fail"


 Integer -> [Integer]

 foo x = x : foo (x-1)


     r <- liftIO $ R.runRedis conn (R.set key va)





>   post "/test" $ do
>     t <- param "txt"
>     text t


images seem to be tricky....

>   post "/images" $ do
>     conn <- liftIO $ R.connect R.defaultConnectInfo
>     img <- body
>     liftIO . putStrLn . show $ img 
>     --img <- param "img"
>     id <- foo conn $ R.incr "imgs:count"      
>     liftIO $ LBS.writeFile (show id) img
>     --foo conn $ R.set (imageKey id) (B.concat . LBS.toChunks $ img)
>     text . T.pack . show $ id
>   get "/images/:id" $ do
>     id <- param "id"    
>     conn <- liftIO $ R.connect R.defaultConnectInfo
>     mimg <- foo conn $ R.get (mkKey "imgs" id)
>     case mimg of
>       Just img -> text . T.pack . C.unpack $ img
>       Nothing  -> text "error"
>     header "content-type" "image"
   
> mkKey :: B.ByteString -> Integer -> B.ByteString
> mkKey x id = B.concat [x, ":", C.pack . show $ id]

 topLevel :: H.Html -> H.Html

> topLevel content = do
>   H.html $ do
>     H.header $ return ()
>     H.body content





how should posts be stored in the database?

GET posts:4

creating a post:

id = INCR posts:count
set ("posts:" ++ id) "{"name":"james","body":"wat"}"



post "/posts" $ do
    post <- jsonData

   name <- param "name"
   body <- param "body"
   img  <- param "img"
   


 foo = do
   man <- liftIO $ newManager def
   req <- liftIO $ parseUrl "https://accounts.google.com/o/oauth2/auth" 
   res <- http req man
   let x = responseBody res
   liftIO $ putStr x


   html $ t 

    beam <- param "word"

    wat <- liftIO $ simpleHttp "http://jsonip.com/"


mconcat ["<h1>Scotty, ", decodeUtf8 wat, " me up!</h1>"]

oauthEndpoint = maybe (error "wat") id $ parseURI "https://accounts.google.com/o/oauth2/auth" 






