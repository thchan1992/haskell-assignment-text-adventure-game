module StageFunctions (
isPokeKey,
pokeOption,
addHPToPoke,
addPokeToUser,
obtainKeyPoke,
removePokeFromMap,
pokeCentre,
addItemToUser,
removeItemFromMap,
showUserPocket,
encountedAPoke,
getWildPokeSize
) where 

import DeclaredData
import SupportFunctions

{-|
Syntax:     showUserPocket User
Purpose:    It returns a string containing what items and pokemon that the user owns
Example:    Assume we have a pokemon called "Hello", and its HP is 100.  Also user has a "APPLE" as an item
            The output will be:
            "Pokemon list:"
            "Name: Hello HP: 100"
            "---------"
            "Item list:"
            "APPLE"
            This will be the output     
Process:    This function take a parameter: (User name pos pokes items) - the user data
            Then it passes 'pokes'(user's pokemon list) to " recursiveUserPokeList" method to get the list of pokemon
            Next it passes 'items'(user's item list) to "recuriveUserItemList" method to get the list of item
-}
showUserPocket :: User -> IO()
showUserPocket (User name pos pokes items) = do 
    putStrLn "\n\nPokemon list:"
    recursiveUserPokeList pokes 1
    putStrLn ("Wild Pokemon Size: " ++ show(getWildPokeSize (User name pos pokes items)))
    putStrLn "\n\nYour Items:"
    recuriveUserItemList items


{-|
Syntax:     recursiveUserPokeList Pokemon
Purpose:    It returns a string containing all the pokemon that the user got to "showUserPocket" method
Example:    Assume we have two pokemons "Hello, HP is 100" and "APPLE, HP is 20". 
            The output will be:
            "Pokemon list:"
            "Name: Hello HP: 100"
            "Name: Apple HP: 20"
            "---------"
            This will be the output     
Process:    This function take a parameter: (x:xs) (the list of pokemons)
            This will first pass 'x' (the first element of the list) to "get1PokeName" and "get1PokeHP" to get the name of the pokemon and its HP.
            Then it show them inside a string.
            Then call the same method with 'xs' (the rest of the list except the first element)
            It will stop until (x:xs) becomes an empty list.
-}
recursiveUserPokeList :: [Pokemon] ->  Int -> IO()
recursiveUserPokeList [] n = do 
    putStrLn ("--------\n\n")

recursiveUserPokeList (x:xs) n = do 
        putStrLn ("Option (" ++ show(n) ++ ") Name: " ++ (get1PokeName x) ++ " HP: " ++ (show(get1PokeHP x)) ++ " Wild Pokemon: " ++ isWildPoke x)
        recursiveUserPokeList xs (n+1)


{-|
Syntax:     recursiveUserItemList Pokemon
Purpose:    It returns a string containing all the items that the user got to "showUserPocket" method
Example:    Assume we have two items "APPLE" and "BANANA". 
            The output will be:
            "Item list:"
            "APPLE"
            "BANANA"
            "---------"   
Process:    This function take a parameter: (x:xs) (the list of items)
            This will first pass 'x' (the first element of the list) to "get1ItemName" to get the name of the Item.
            Then it show it inside a string.
            Then call the same method with 'xs' (the rest of the list except the first element)
            It will stop until (x:xs) becomes an empty list.
-}
recuriveUserItemList :: [Item] -> IO()
recuriveUserItemList [] = do
    putStrLn ("--------\n\n")

recuriveUserItemList (x:xs) = do
    putStrLn ("Item list: " ++ get1ItemName x)
    recuriveUserItemList xs


{-|
Syntax:     isWildPoke Pokemon
Purpose:    It returns a string showing whether a pokemon is a wild pokemon or not to showUserItem method
Example:    Assume we have isWildPoke (Pokemon name type' hp moves True) = "Yes"

Process:    This method take a Pokemon data, and check if the last parameter is a True value or not, if it is true return String "Yes".
-}
isWildPoke ::Pokemon -> String
isWildPoke (Pokemon name type' hp moves tf) = if tf == True then "Yes" else  "No"




{-|
Syntax:     isPokeKey Pokemon Room
Purpose:    Check whether the Pokemon that chosen by the user is the right pokemon to escape the current room.  If this is the case, it returns the True value
Example:    Assume the second parameter of a Room which is the right pokemon, and the pokemon we are checking got the same Name (Apple).
            isPokeKey (poke Apple) (room Apple) = Apple == Apple then True.
            This function will return true value.
Process:    This method takes two parameters which are 'poke'(The pokemon we are checking) and 'room'(The current).  
            It first passes poke to "get1PokeName" to get the pokemon name.
            It then passes room to "getRoomPokeName" to get the key pokemon name of the room
            It then compares these two value.  If they are the same, then it will return True value.
            In the getRoomPokeName method, it will take the first parameter of a Room which is 'poke'.
            Then it passes it to get1PokeName to get the name of the pokemon.
-}
isPokeKey :: Pokemon -> Room -> Bool
isPokeKey poke room = if get1PokeName poke == getRoomPokeName room then True else False
    where getRoomPokeName (Room name poke poke' pos1 pos2 pos3 tf n) = get1PokeName poke




{-|
Syntax:     pokeOption [Pokemon]
Purpose:    Show all the pokemon that users can choose to help them to escape the room.
            It returns a Pokemon data.
Example:    Assume we have list of pokemon [poke1, poke2].  
            poke1 has a name of "Hello" and type "Fire"
            poke2 has a name of "World" and type "Fire"

            the output will be:
            Option: Hello Type: Fire
            Option: World Type: Fire

Process:    This method takes a parameter: (x:xs) (a list of all pokemons).  
            It first passes 'x'(the first element of the list) to get1PokeName to get the name of the pokemon.
            and then passes 'x' to getType below to get the type of the pokemon.
            Then, it returns a string with all these values and calls itself again with 'xs' (the rest of the list without the first item)
            It will stop calling itself if (x:xs) becomes empty.
-}
pokeOption :: [Pokemon] -> IO ()
pokeOption [] = do 
    putStrLn ("---")
pokeOption (x:xs) = do 
    putStrLn ("Option: " ++ get1PokeName x  ++ " Type: " ++ getType x) 
    pokeOption xs
   where getType (Pokemon name type' hp moves tf) = type'




{-|
Syntax:     addHPToPoke Pokemon
Purpose:    If users have chosen a right pokemon to unlock the door, the pokemon will get extra HP.
            This method is to add extra 10HP to that chosen pokemon
Example:    Assume we have addHPToPoke (Pokemon name type' 100 moves) = (Pokemon name type' 100+10 moves)
Process:    This method takes a parameter: (Pokemon name type' hp moves) (the chosen pokemon)
            It then add 10 to hp attribute and returns a Pokemon Data.
-}
addHPToPoke :: Pokemon -> Pokemon
addHPToPoke (Pokemon name type' hp moves tf) = (Pokemon name type' (hp+10) moves tf)


{-|
Syntax:     addPokeToUser User Pokemon
Purpose:    After user chose a pokemon, this method will add that pokemon to users' pocket.
            This method will then return a updated user data.

Example:    Assume we have addPokeToUser (User name pos xs items) x = (User name pos (x:xs) items)
        
Process:    This method takes a parameter: (User name pos pokes items)(User data.)
            Then this method will use ':' operator to put newPoke('the chosen pokemon') in front of 'pokes'(User's pokemons)
            Then it will return an updated user data.
-}
addPokeToUser :: User -> Pokemon -> User
addPokeToUser (User name pos pokes items) newPoke = (User name pos (newPoke:pokes) items)


{-|
Syntax:     obtainKeyPoke Room
Purpose:    After user chose a pokemon, this method will change the boolean value which is the sixth parameter of the room.
            With this, the user can unlock the room.  
Example:    Assume we have obtainKeyPoke (Room name poke pos1 pos2 pos3 tf n) = (Room name poke pos1 pos2 pos3 True n)
Process:    This method takes a parameter: (Room name poke pos1 pos2 pos3 tf n)(The current room)
            Then this method will change the sixth parameter which is 'tf' to True.
-}
obtainKeyPoke :: Room -> Room
obtainKeyPoke (Room name poke poke' pos1 pos2 pos3 tf n) = (Room name poke poke' pos1 pos2 pos3 True n)



{-|
Syntax:     removePokeFromMap [Pokemon] Pokemon
Purpose:    After user chose a pokemon, this method will remove the pokemon from the pokemons(The list of all pokemon in the game).
            Then, return a updated pokemon list.
Example:    Assume we have pokemon list [poke1, poke2, poke3], and the second parameter is poke3.
            The output will be [poke1, poke2]
        
Process:    This method takes two parameters (x:xs)(A list of all pokemons in the game), y(The chosen pokemon by users).
            It first pass both parameters to get1PokeName to get the name of the pokemon.
            Then compares them if they got the same value.  If this is the case, just return 'xs'(the rest of the list without the first element).
            Otherwise, place the first item 'x' in front of the list and then call the method again with xs (the rest of the list without the first element) and y.
            It will stop until (x:Xs) becomes empty.
-}
removePokeFromMap :: [Pokemon] -> Pokemon -> [Pokemon]
removePokeFromMap [] _ = []
removePokeFromMap (x:xs) y = 
    if get1PokeName x == get1PokeName y then 
        xs 
    else 
        x:removePokeFromMap xs y


{-|
Syntax:     pokeCentre User
Purpose:    After users go to the pokemon centre, all users' will return to 100HP if their pokemon's HP is lower than 100.
Example:    Assume we have pokemon list [(poke1 HP = 1000), (poke2 HP = 40), (poke3 HP = 110)] in a user data
            The output will be a user data with a list of pokemon - [(poke1 HP = 1000), (poke2 HP = 100), (poke3 HP = 110)]
        
Process:    This method takes a parameter of (User name pos pokes moves).  It will pass pokes(the list of pokemons) to checkPokeHP.
            Then, use this returned value to replace the current pokes.
            Next, it will return the updated user data.
-}
pokeCentre :: User -> User
pokeCentre (User name pos pokes moves) = (User name pos (checkPokeHP pokes) moves)


{-|
Syntax:     checkPokeHP [Pokemon]
Purpose:    After users go to the pokemon centre, all users' will return to 100HP if their pokemon's HP is lower than 100.
            This method is to check whether each pokemons' HP in user's list is lower 100.  If any pokemon is lower than 100, their HP will become 100.
            Then it will return a updated pokemons list to pokeCentre method.

Example:    Assume we have pokemon list [(poke1 HP = 1000), (poke2 HP = 40), (poke3 HP = 110)] 
            The output will be a list of pokemon - [(poke1 HP = 1000), (poke2 HP = 100), (poke3 HP = 110)]
        
Process:    This method takes a parameter of (x:xs)(all users' pokemons).
            It will first passes x(the first element of the list) to get1PokeHP to get its HP.  Then, it checks if its HP is lower than 100.
            If it is lower, it will passes it to healPoke method to restore its HP to 100.  Then place x in front of the list by using ':'.
            And then it calls itself again with 'xs' (the rest of the pokemons without the first one).  If it is higher than 100, then, it will place x in front of the list by using ':'.
            And then it calls itself again with 'xs' (the rest of the pokemons without the first one). It will stop until the (x:xs) becomes empty.
-}
checkPokeHP :: [Pokemon] -> [Pokemon]
checkPokeHP [] = []
checkPokeHP (x:xs) = 
    if get1PokeHP x < 100 then 
        (healPoke x):checkPokeHP xs
    else
        x:checkPokeHP xs
    where healPoke (Pokemon name type' hp moves tf) = (Pokemon name type' 100 moves tf)




{-|
Syntax:     addItemToUser User Item
Purpose:    After users encountered an item and decided to pick it up.  This method will add this item to users' pocket.
            It is to return an updated user's data.
Example:    Assume we have a user data with a list of item [item1, item2], and the users have picked up an item3.
            The output will be a user data with a list of item [item3, item1, item2].
Process:    This method takes two parameters of (User name pos pokes items)(users' data) and item(the chosen item)
            It will place item in front of items by using the ':' operator.
            Then it will return the updated user data.
-}
addItemToUser :: User -> Item -> User
addItemToUser (User name pos pokes items) item = (User name pos pokes (item:items))



{-|
Syntax:     removeItemFromMap [Item] Item
Purpose:    After users encountered an item and decided to pick it up.  This method will remove the item from the game world.
            Then it will return updated items list.

Example:    Assume we have  a list of item [item1, item2], and the users have picked up an item1.
            The output will be a list of item  [item2].
        
Process:    This method takes two parameters of (x:xs)(list of all item in the game) and y(the item chosen by the user)
            It will first pass x(the first item of the list) and y(the item chosen by the user) to get1ItemName to get their name
            so it can compare the value.  IF they are the same, then it will return xs(The list without the first element)
            Otherwise, it places x in front of the list and then call the same method with xs(the list without the first element) and y.
            It will stop calling itself once (x:xs) becomes empty.
            
-}
removeItemFromMap :: [Item] -> Item -> [Item]
removeItemFromMap [] _ = []
removeItemFromMap (x:xs) y 
    | get1ItemName x == get1ItemName y = xs
    | otherwise = x:removeItemFromMap xs y 






{-|
Syntax:     encountedAPoke Pokemon User
Purpose:    This method shows what pokemon user has encounted on the map.

Example:    You encountered a PSYDUCK Do you want to capture it?
            You can only get 7 wild pokemons at most
            And you now have 0
            Press 'YES' to capture, press 'NO' to move on.  

Process:    This method a parameter of a pokemon data and return the last parameter
-}
encountedAPoke :: Pokemon -> User -> [String] -> IO()
encountedAPoke poke user commands = do
    putStrLn ("You encountered a " ++ get1PokeName poke ++ " \nHP: " ++ show(get1PokeHP poke) ++ "\n Type: " ++ getType poke ++"\nDo you want to capture it?\n You can only get 7 wild pokemons at most\n And you now have " ++ (show(getWildPokeSize user)))
    putStrLn ("Press '"++ chooseCommand commands 7 ++"' to capture, press '" ++ chooseCommand commands 8 ++ "' to move on." )
    where getType (Pokemon name type' hp moves tf) = type'
    


{-|
Syntax:     getWildPokeList [Pokemon]
Purpose:    This method gets all the user's wild Pokemon in the list and return it to getWildPokeSize
Example:    Assume we have two wild pokemons which are poke1 and poke2.  Then the output will be [poke1, poke2]
Process:    This method is recursive method, it will take a parameter: (x:xs) the list of all pokemon of a user.
            It will stop calling itself when the (x:xs) becomes empty
            In each recursive call, it will pass the first item (x) to get1PokeBool to obtain the boolean to see if they are wlid pokemon
            If it is true, then, it will place this item in front of the list and then call itself again with the rest of the item (xs)
            otherwise, it will just call itself with xs without the first item.
-}
getWildPokeList :: [Pokemon] -> [Pokemon]
getWildPokeList [] = []
getWildPokeList (x:xs) = if get1PokeBool x == True then x:getWildPokeList xs else getWildPokeList xs


{-|
Syntax:     getWildPokeSize User
Purpose:    This method return the size of user's wlid pokemons

Example:     Assume we have two wild pokemons which are poke1 and poke2.  Then the output will be 2

Process:    This method take a parameter: user data.  It passes pokes which is the list of user's pokemon to getWildPokeList to get the list with only wlid pokemon
            Then use length to return the size of it.
-}
getWildPokeSize :: User -> Int
getWildPokeSize (User name pos pokes items) = length (getWildPokeList pokes)




