{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import Yesod
import Text.Lucius

data LaHacks = LaHacks

-- TODO: Figure out how to handle post requests on server-side, return the user
-- a web page containing the newly-created forum.

mkYesod "LaHacks" [parseRoutes|
/style.css StyleR GET
/ HomeR GET
/draft DraftR GET --POST
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

{-  Garbage code btw
postDraftR :: Handler Html
postDraftR = do
    value <- lookupPostParam "question-textarea"
    case value of
        Just value -> return ()
        Nothing -> putStrLn "No value." 
    sendFile "text/html" "site/draft.html" -}

main :: IO ()
main = warp 3000 LaHacks
