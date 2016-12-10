-- This file is part of bsfscraper
-- Copyright (C) 2016  Fraser Tweedale
--
-- hs-notmuch is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Char (chr, ord)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Monoid ((<>))
import System.IO (hPutStrLn, hPutChar, hPutStr, hSetEcho, stderr, stdin)

import Control.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Csv (encode)
import Network.Wreq
import qualified Network.Wreq.Session as S
import Text.HTML.Scalpel
import Text.HTML.TagSoup

import Csv
import Member
import Scrapers

topUrl = "https://class.bsfinternational.org/"
landingUrl = "https://class.bsfinternational.org/Default.aspx"
activeMembersUrl = "https://class.bsfinternational.org/SecMembersList.aspx"
inactiveMembersUrl = "https://class.bsfinternational.org/SecMembersListInactive.aspx"
classDataUrl = "https://class.bsfinternational.org/Orders_ClassWelcome_3.aspx"

main :: IO ()
main = S.withSession $ \sess -> do
  (classNo, password) <- getCreds
  hPutStrLn stderr "Logging in..."
  r <- S.get sess landingUrl
  r <- S.post sess landingUrl $
    [ "txtClassNo" := classNo, "txtPassword" := password
    , "cmdSubmit" := ("Submit" :: String), "txtIncorrect" := ("" :: String)
    ] ++ postParams r

  hPutStrLn stderr "Entering class data website..."
  let
    params = postParamsExtra
      [ "tbx_Att_Date", "tbx_rd_date", "tbx_invupd_date"
      , "btn_Class_Data_Website", "Lessons", "HDP", "HTL", "TVH", "txbx_TC_1"
      , "txbx_TC_qty", "txbx_TC_2", "txbx_SC_1", "txbx_SC_qty", "txbx_SC_2"
      ] r
  r <- S.post sess classDataUrl params

  hPutStrLn stderr "Retrieving member list..."
  r <- S.get sess activeMembersUrl
  rs <- mapM
    (\c ->
      S.post sess activeMembersUrl (("__EVENTTARGET" := ("lb_" <> [c])) : postParams r)
      <* hPutChar stderr c)
    (['A'..'Z'] :: [Char])
  hPutChar stderr '\n'
  let urls = rs >>= (\r -> fromJust $ scrapeFrom r scrapeMemberEditUrls)

  hPutStrLn stderr $ "Found " ++ show (length urls) ++ " members."
  hPutStrLn stderr "Retrieving member details..."
  let unBS = map (chr . fromIntegral) . L.unpack
  members <- catMaybes <$> mapM
    ((\url -> scrapeMemberInfo sess url <* hPutChar stderr '.') . unBS)
    urls
  hPutChar stderr '\n'
  hPutStrLn stderr $ "Parsed " <> show (length members) <> " members."
  L.putStr $ encode $ fmap memberSummaryRecord members
  hPutStrLn stderr "Finished!"

scrapeMemberInfo :: S.Session -> String -> IO (Maybe Member)
scrapeMemberInfo sess relUrl =
  let
    opts = defaults & header "Referer" .~ [activeMembersUrl]
  in do
    r <- S.getWith opts sess $ topUrl ++ relUrl
    return $ scrapeFrom r scrapeMember


-- | Extract POST args from form in response
--
postParamsExtra :: [String] -> Response L.ByteString -> [FormParam]
postParamsExtra extraIds r =
  let
    tags = parseTags $ r ^. responseBody
    ids =
      ["__LASTFOCUS", "__EVENTTARGET", "__EVENTARGUMENT"
      , "__VIEWSTATE", "__PREVIOUSPAGE", "__EVENTVALIDATION"]
    bs = B.pack . map (fromIntegral . ord)
  in
    mapMaybe
      (\tagId -> (bs tagId :=) <$> scrapeFrom r (valueById tagId))
      (ids ++ extraIds)

postParams :: Response L.ByteString -> [FormParam]
postParams = postParamsExtra []

scrapeFrom r s = scrape s $ parseTags $ r ^. responseBody

getCreds :: IO (String, String)
getCreds = do
  hPutStr stderr "Enter class no: "
  classNo <- getLine
  hSetEcho stdin False
  hPutStr stderr "Enter password: "
  password <- getLine
  hPutChar stderr '\n'
  hSetEcho stdin True
  return (classNo, password)
