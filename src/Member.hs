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

{-# LANGUAGE TemplateHaskell #-}

module Member where

import Control.Lens

data Address = Address
  { _address1 :: String
  , _address2 :: Maybe String
  , _city :: String
  , _state :: String
  , _postcode :: String
  } deriving (Show, Eq)
makeClassy ''Address

data Church = Church
  { _churchName :: String
  , _churchSuburb :: String
  , _churchDenomination :: String
  } deriving (Show, Eq)
makeClassy ''Church

type Study = String
type Leader = String
type PastStudies = [(Study, Leader)]

data Member = Member
  { _familyName :: String
  , _givenName :: String
  , _memberAddress :: Address
  , _mobilePhone :: Maybe String
  , _homePhone :: Maybe String
  , _businessPhone :: Maybe String
  , _email :: Maybe String
  , _memberChurch :: Church
  , _invitedBy :: Maybe String
  -- TODO make these actual dates
  , _dateRegistered :: String
  , _dateInactivated :: Maybe String
  , _dateReactivated :: Maybe String
  , _pastStudies :: PastStudies
  } deriving (Show, Eq)
makeClassy ''Member

instance HasAddress Member where
  address = memberAddress

instance HasChurch Member where
  church = memberChurch
