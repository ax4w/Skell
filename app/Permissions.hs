{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

{-
    Handle Permissions with the discord api hex values to check if needed permissions are set
-}

module Permissions where
import Discord.Internal.Rest ( Role(rolePerms) )
import Data.Text as T ( unpack )
import Data.Bits ( Bits((.&.)) )

data TPerms = MANAGE_MESSAGES | ADMIN | KICK | NF deriving Eq

instance Enum TPerms where
    fromEnum MANAGE_MESSAGES = 0x0000000000002000
    fromEnum ADMIN = 0x0000000000000008
    fromEnum KICK = 0x0000000000000002
    fromEnum _ = 0
    

    toEnum 0x0000000000002000 = MANAGE_MESSAGES
    toEnum 0x0000000000000008 = ADMIN
    toEnum 0x0000000000000002 = KICK
    toEnum _ = NF

hasUserPermissions::  [Role] -> TPerms -> Bool
hasUserPermissions l p = any(> 0) $ foldr(\x acc-> [(.&.) (read (T.unpack $ rolePerms x) :: Int) $ fromEnum p] <> acc) [] l