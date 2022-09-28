{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

module Permissions where
import Discord.Internal.Rest ( Role(rolePerms) )
import Data.Text as T ( unpack )
import Data.Bits ( Bits((.&.)) )
import Data.Map (fromList)

data TPerms = MANAGE_MESSAGES | ADMIN | NF deriving Eq

instance Enum TPerms where
    fromEnum MANAGE_MESSAGES = 0x0000000000002000
    fromEnum ADMIN = 0x0000000000000008
    fromEnum NF = 0

    toEnum 0x0000000000002000 = MANAGE_MESSAGES
    toEnum 0x0000000000000008 = ADMIN
    toEnum _ = NF

hasUserPermissions::  [Role] -> TPerms -> Bool
hasUserPermissions l p = any(> 0) $ foldr(\x acc-> [(.&.) (read (T.unpack $ rolePerms x) :: Int) $ fromEnum p] <> acc) [] l