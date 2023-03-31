{-# LANGUAGE OverloadedStrings #-}
import Data.Text (unpack)
import Network.HTTP.Simple (httpLbs, parseRequest)
import Text.XML (def, parseLBS, renderText)
import Text.XML.Cursor (Cursor, content, element, fromDocument, ($//), (&|))

fetch :: IO ByteString
fetch = do
    request <- parseRequest "https://www.w3schools.com/xml/cd_catalog.xml"
    response <- httpLbs request
    return $ getResponseBody response

parse :: ByteString -> Cursor
parse xml = fromDocument $ parseLBS def xml

getTableData :: Cursor -> [(String, String, String, String)]
getTableData cursor =
    cursor $// element "CD" >=> \cd ->
        let title = cd $// element "TITLE" &| unpack &| head
            artist = cd $// element "ARTIST" &| unpack &| head
            country = cd $// element "COUNTRY" &| unpack &| head
            price = cd $// element "PRICE" &| unpack &| head
        in [(title, artist, country, price)]

main :: IO ()
main = do
    xml <- fetch
    let cursor = parse xml
        tableData = getTableData cursor
        header = "Title\tArtist\tCountry\tPrice"
        rows = map (\(t, a, c, p) -> printf "%s\t%s\t%s\t%s" t a c p) tableData
        table = unlines $ header : rows
    putStrLn table