{-# LANGUAGE RecordWildCards #-}
module Lib where

import Data.Maybe
import System.Directory
import System.FilePath.Posix
import Text.XML.Light

data Icon
  = Icon
  { fileName :: FilePath
  , directory :: FilePath
  , content :: Element
  } deriving (Show)

main :: IO ()
main = do
  iconFileNames <- filter (\f -> takeExtension f == ".svg") <$> listDirectory "icon"
  let iconPaths = fmap ("icon/" ++) iconFileNames
  icons <- catMaybes <$> mapM readIcon iconPaths
  render . concatIcons $ icons

readIcon :: FilePath -> IO (Maybe Icon)
readIcon fp = do
  absolute <- makeAbsolute fp
  content <- parseXMLDoc <$> readFile absolute
  case content of
    Nothing -> pure Nothing
    Just c ->
      pure $ Just $ Icon
        { content = change (takeBaseName absolute) c
        , fileName = takeFileName absolute
        , directory = takeDirectory absolute
        }

change :: String -> Element -> Element
change idName e =
  let
    newName = mkQName "g"
    idAttr = mkAttr "id" idName
    attrAllowed a = not $ (qName . attrKey $ a) `elem` ["xmlns", "id"]
    filteredAttribs = filter attrAllowed (elAttribs e)
  in
    e { elName = newName, elAttribs = idAttr : filteredAttribs }

concatIcons :: [Icon] -> Icon
concatIcons icons =
  let
    fileName = "out.svg"
    directory = "."
    styleText = "g { display: none; } g:target { display: inline };"
    styleNode :: Element
    styleNode
      = node (mkQName "defs")
      $ node (mkQName "style") styleText
    svgQName
      = add_attr (mkAttr "xmlns" "http://www.w3.org/2000/svg")
      . add_attr (mkAttr "viewBox" "0 0 512 512")
      . node (mkQName "svg")
      { qName = "svg"
      }
    contents = fmap content icons
  in
    Icon
      { fileName = fileName
      , directory = directory
      , content = (svgQName (styleNode:contents))
      }

render :: Icon -> IO ()
render Icon{..} = do
  path <- makeAbsolute $ directory </> fileName
  writeFile path (showElement content)

--
-- XML utils
--

mkAttr :: String -> String -> Attr
mkAttr name val = Attr { attrKey = mkQName name, attrVal = val}

mkQName :: String -> QName
mkQName name = blank_name { qName = name }
