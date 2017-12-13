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
{-# LANGUAGE TupleSections #-}

module Scrapers where

import Control.Applicative
import Data.Bool (bool)
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))

import Control.Lens
import Data.ByteString.Lens
import Text.HTML.Scalpel
import Text.Regex.Base (makeRegex)
import Text.Regex.TDFA.String (Regex)
import Text.StringLike (StringLike)

import Member

valueById = attrById "value"

checkedById s = (== "checked") <$> attrById "checked" s <|> pure False

attrById k s = unBS
  <$> attr k (TagString "input" @: [AttributeString "id" @= s])

scrapeMemberEditUrls :: (Ord str, Show str, StringLike str) => Scraper str [str]
scrapeMemberEditUrls = attrs "href" $
  TagString "a" @: [AttributeString "href" @=~ (makeRegex "^MemberEdit.aspx" :: Regex)]

scrapeAddress = Address
  <$> valueById "Mail_Address_1"
  <*> optional (valueById "Mail_Address_2")
  <*> valueById "Mail_City"
  <*> valueById "Mail_State"
  <*> valueById "Mail_Zip"

scrapeChurch = Church
  <$> (unBS <$> text
    (  TagString "select" @: [AttributeString "name" @= "ddl_Church_Name"]
    // TagString "option" @: [AttributeString "selected" @= "selected"] ))
  <*> valueById "Church_City"
  <*> valueById "Church_Denomination"

scrapeMember = Member
 <$> valueById "Last_Name"
 <*> valueById "First_Name"
 <*> scrapeAddress
 <*> optional (valueById "Cell_Phone")
 <*> optional (valueById "Email_Address")
 <*> scrapeChurch
 <*> valueById "Date_Registered"
 <*> optional (valueById "Date_Enrolled")
 <*> optional (valueById "Date_Inactivated")
 <*> optional (valueById "Date_Reinstated")
 <*> scrapePastStudies
 <*> checkedById "ckb_Register_For_Next_Study"

studies :: [String]
studies =
  [ "Genesis", "Moses", "PPL1", "Isaiah", "PPL2"
  , "Matthew", "Romans", "Acts", "John", "Revelation" ]

scrapePastStudies =
  catMaybes <$> traverse scrapePastStudy studies

scrapePastStudy study =
  optional $
    checkedById idCheckbox >>= bool empty ((study,) <$> valueById idGL)
  where
    idCheckbox = "ckbx_" <> study <> "_Completed"
    idGL = "txbx_" <> study <> "_GL"

ensureNonEmpty = (>>= f) where
  f s | s == "" = empty | otherwise = pure s

unBS = (^. unpackedChars)
