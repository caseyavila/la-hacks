{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import System.IO
import Yesod
import Text.Lucius

data LaHacks = LaHacks

-- TODO: Figure out how to handle post requests on server-side, return the user
-- a web page containing the newly-created forum.

mkYesod "LaHacks" [parseRoutes|
/style.css StyleR GET
/ HomeR GET
/draft DraftR GET POST
|]

instance Yesod LaHacks

getStyleR :: Handler Css
getStyleR = do
    sendFile "text/css" "site/style.css"

getHomeR :: Handler Html
getHomeR = do 
    sendFile "text/html" "site/index.html"

getDraftR :: Handler Html
getDraftR = do
    sendFile "text/html" "site/draft.html"

postDraftR :: Handler Html
postDraftR = do
    number <- liftIO $ logNumber
    hello <- lookupPostParam "question-textarea"
    case question of
        Nothing -> redirect HomeR
        Just question -> do
            liftIO $ print number
            liftIO $ writeFile ("questions/" ++ (show $ number)) (show $ hello)
            liftIO $ writeFile "number" (show $ number + 1)
            redirect DraftR

logNumber :: IO Int
logNumber = do
    handle <- openFile "number" ReadWriteMode
    contents <- hGetContents handle
    return (read contents :: Int)

main :: IO ()
main = warp 3000 LaHacks
