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

{-# LANGUAGE GADTs #-}

module Csv where

import Control.Lens
import Data.Csv

import Member

data CanField where
  CanField :: ToField a => a -> CanField

instance ToField CanField where
  toField (CanField a) = toField a

memberSummaryRecord :: Member -> [CanField]
memberSummaryRecord v =
  [ CanField $ v ^. familyName
  , CanField $ v ^. givenName
  , CanField $ v ^. address1
  , CanField $ v ^. address2
  , CanField $ v ^. city
  , CanField $ v ^. postcode
  , CanField $ v ^. churchName
  , CanField $ v ^. phone
  , CanField $ v ^. email
  ]
