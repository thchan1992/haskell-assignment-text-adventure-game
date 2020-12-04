module DeclaredData(
Room(..)
,Pokemon(..)
,Move(..)
,Direction(..)
,Item(..)
,GameMap(..)
,Key
,Name
,Type
,Power
,EN
,HP
,Position
,X_up
,X_bottom
,Y_up
,Y_bottom
, User(..)
) where

{-| User data type
Name: The name of the user
Position:   The user position on the map
[Pokemon]:  The list of the pokemon that user own
[Item]:     The list of the item that user own
-}
data User = User Name Position [Pokemon] [Item]

{-| Room data type
Name: The name of room (String)
Pokemon: The Pokemon users need to use to escape the room (Pokemon)
Position: The position where users can find those pokemon (Position)
Position:  The exit position (Position)
Position:       The pokemon centre position where users can heal their pokemon (Position)
Key:  Whether the user has obtained the key pokemon to exit the room (Bool)
Int:    The number are used for the pattern matching purpose in "nextStage" method on Game.hs.
        If this number is 0, it means the game has finished
-}
data Room = Room Name Pokemon Pokemon Position Position Position Key Int deriving Show
type Name = String
type Position = (Int, Int) 
type Key = Bool



{-| Pokemon data type
Name: The name of the pokemon (String)
Type: The type of the pokemon (String)
HP:  The Health Point of the pokemon (Int)
[Move]: The list of move of pokemon
Bool:   For indicating whether this pokemon is a wlid pokemon
-}
data Pokemon = Pokemon Name Type HP [Move] Bool deriving Show
type Type = String
type HP = Int



{-| Moves data type
Name:  The name of the move (String)
Power:  How powerful is the move (Int)
EN:     How many times that user can use this move (Int)
-}
data Move =  Move Name Power EN deriving Show
type Power = Int
type EN = Int


-- The direction in the map
data Direction = UP | RIGHT | DOWN | LEFT deriving Show


{-| Item data type
Name:   The name of the item
HP:     How much Health Point will increase the HP of a pokemon after it is used. 
Position:   The location where users can find it on the map
String:     The string showing the effect of the item
-}
data Item = Item Name HP Position String deriving Show


-- a map of the game
-- X_up and X_bottom represent how wide is the map
-- Y_ip and Y_bottom represent how high is the map
data GameMap = GameMap X_up X_bottom Y_up Y_bottom Bool
type X_up = Int
type X_bottom = Int
type Y_up = Int
type Y_bottom = Int