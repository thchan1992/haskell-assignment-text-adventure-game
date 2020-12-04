module GameData (
resetPos,
createUser,
createMap,
createItem,
createRoom,
createRoomPosition,
createItemPosition,
createAllPoke,
getWildPoke,
createAllCommands,
createMapSize
)where

import DeclaredData
import StageFunctions
import SupportFunctions
import Data.Char(toUpper, isDigit, isSpace)


{-|
Syntax:     This is called the method 'start' of Game.hs
Purpose:    This method returns a user data after the program has taken a name from the user
Example:    User "Hello" (0,0) [] []
Process:    It asks the user to input a name and then place this parameter on a user data.  Then it will return it to the start method
-}
createUser ::  IO User
createUser = do
    putStrLn "            Hello! Happy to see you here, what is your name?"
    option' <- getLine
    empty
    let option = map toUpper option'
    putStrLn ("Welcome! " ++ option)
    let user = User option (0, 0) [] []
    return user



{-|
Syntax:     This is called the method 'start' of Game.hs
Purpose:    This method returns a number for the size of the map after asking users the size of the map
Example:    return 5
Process:    It asks the user to input a number.  If the number is not bigger or equal to 5, then it will ask the user to input again.  Then it will return the number.
-}
createMapSize :: IO Int
createMapSize = do
    putStrLn "\nFirst thing first, what size of the game world would you like? It has to be at least 5 - (5X5). \n Bigger the world, it take longer to go to the item and exit. Choose a number!\n"
    option <- getLine
    empty
    -- checkInput method of 'SupportFunctions.hs' checks if the user's input is purely a number or not.  
    -- if the input is a number..
    if checkInput option == True then do
        -- convert this input from string to a number and check if it is 5 or bigger
        let x = (read option)
        if x >= 5 then do
            --return this number
            return x
        -- number is smaller than 5
        else do 
            wrongInput
            createMapSize
    -- if the input is not a pure natural number
    else do
        wrongInput
        createMapSize



{-|
Syntax:     createMap Int
Purpose:    This method returns a GameMap data to the game
Example:    gameMap = (GameMap 5 0 5 0 False)
Process:    This method take a parameter: n (size of the game world).  
            First It asks the user if they want to turn on the torch.  (If the torch is on the radar, all the special location will be shown on the radar for the user.)
            If they do want to turn it on, the last parameter of the map will be False otherwise it will be True.  
            Then, it will place n on the first and thrid of parameter of the GameMap and return the gameMap data.
-}
createMap :: Int -> [String] -> IO GameMap
createMap n commands = do
    putStrLn ("Are you testing this game and need a TORCH?\n\n If you need it, we will uncover all the item and special location for you!\n Press " ++ (chooseCommand commands 7) ++ " or " ++ (chooseCommand commands 8))
    option' <- getLine
    empty
    let option'' = map toUpper option'
    -- chooseCommand method is to get the command list from the game, and 7 represents the number position of the list,
    -- so we are checking if user's input is same as the 8th command in the list.
    if chooseCommand commands 7  == option'' then do
            -- set up the gameMap with by passing n on the parameter and set the last parameter as False
            let gameMap = (GameMap n 0 n 0 False)
            putStrLn "\nWe turned on the TORCH for you now. \nAnd the speical location will be indicated for you on the radar.\n"
            -- a method that pause of the game so user can read the text
            pause
            --return the GameMap
            return gameMap
    -- if user chose not to turn on the torch, this will be called
    else if chooseCommand commands 8 == option'' then do
            -- set up the gameMap with by passing n on the parameter and set the last parameter as True
            let gameMap = (GameMap n 0 n 0 True)
            -- return the GameMap
            return gameMap
    else do
        -- a indicator shows that user has put a wrong input
        wrongInput
        createMap n commands




{-|
Syntax:     This is called by start method of Game.hs
Purpose:    This method return all the text commands to the main game, so users can use these commands
Process:    It created commands by declaring them as string, and then place them into a list.  Then, it returns this list.
-}
createAllCommands :: IO [String]
createAllCommands = do
    let command0 = "W"
    let command1 = "S"
    let command2 = "A"
    let command3 = "D"
    let command4 = "PICK UP "
    let command5 = "USE "
    let command6 = "LEAVE IT"
    let command7 = "YES"
    let command8 = "NO"
    let command9 = "POCKET"
    let command10 = "DELETE"
    let command11 = "BACK"
    let commands = [command0, command1, command2, command3, command4, command5, command6, command7, command8, command9, command10, command11] :: [String]
    return commands


{-|
Syntax:     resetPos User
Purpose:    It replace the second parameter of a User.  This is to reset the position to the original position after a user exited from a room.
Example:    Assume resetPos (User name (1,3) pokes items) = (User name (0,0) pokes items)
Process:    It replaces the second parameters of a User data by (0,0) and then return the User data.
-}
resetPos :: User -> User
resetPos (User name pos pokes items) = (User name (0,0) pokes items)

{-|
Syntax:     This is called by a nextStage method of Game.hs
Purpose:    This method return all the domestic pokemon as a list.
Process:    It first creates moves and pokemon.  Then, place these pokemons into a list and return this list.
-}
createAllPoke :: IO [Pokemon]
createAllPoke = do
    let tackle = Move "TACKLE" 20 20 :: Move
    let razorLeaf = Move "RAZOR LEAF" 40 2 :: Move
    let fireBeam = Move "FIRE BEAM" 40 2 :: Move
    let waterCannon = Move "WATER CANNON" 40 2 :: Move
    let useless = Move "USELESS MOVE" 5 100 :: Move
    let bulbasaur = Pokemon "BULBASAUR" "GRASS" 100 [tackle, razorLeaf] False 
    let charmander = Pokemon "CHARMANDER" "FIRE" 100 [tackle, fireBeam] False 
    let squirtle = Pokemon "SQUIRTLE" "WATER" 100 [tackle, waterCannon] False 
    let magikarp = Pokemon "MAGIKARP" "USELESS" 100 [useless] False
    let pokemons = [bulbasaur, charmander, squirtle, magikarp] :: [Pokemon]
    return pokemons 


{-|
Syntax:     createRoom Int
Purpose:    This method return a room data.
Process:    It is defined by pattern matching.  The Int parameter is to decide which pattern to be called.
            It first creates moves for the boss and key pokemon of the room.  Then it creates the boss and key pokemon.  Next, it creates the room and return it.
-}
createRoom :: Int -> IO Room
createRoom 1 = do 
    let tackle = Move "TACKLE" 20 20
    let thunderBolt = Move "THUNDER BOLT" 40 1
    let boss = Pokemon "PIKACHU" "THUNDER" 100 [thunderBolt, tackle] True
    let charmander = Pokemon "CHARMANDER" "FIRE" 100 [] False
    let room = Room "GRASS" charmander boss (0, 0) (0, 0) (0, 0) False 2
    return room

createRoom 2 = do
    let tackle = Move "TACKLE" 20 100
    let beam = Move "BEAM" 50 2
    let boss = Pokemon "MEW" "DARK" 120 [beam, tackle] True
    let bulbasaur = Pokemon "BULBASAUR" "GRASS" 100 [] False
    let room = Room "WATER" bulbasaur boss (0, 0) (0, 0) (0, 0) False 3
    return room

createRoom 3 = do
    let tackle = Move "TACKLE" 20 100
    let beam = Move "BEAM" 60 3
    let boss = Pokemon "MEWTWO" "DARK" 120 [beam, tackle] True
    let bulbasaur = Pokemon "BULBASAUR" "GRASS" 100 [] False
    let room = Room "WATER" bulbasaur boss (0, 0) (0, 0) (0, 0) False 0
    return room


{-|
Syntax:     createItem Int
Purpose:    This method return a list of item data.
Process:    It is defined by pattern matching.  The Int parameter is to decide which pattern to be called.
            It first creates items and then place them into the list
-}
createItem :: Int -> IO [Item]
createItem 1 = do
    let item0 = Item "SUSHI" 20 (0, 0) "Your pokemon is enjoying it! HP got increased by 20"
    let item1 = Item "BAT MEAT" (-30) (0, 0) "Your pokemon hates it and got a coronavirus, HP got decreased by 30"
    let items = [item0, item1]
    return items

createItem 2 = do
    let item0 = Item "BAT MEAT FRIED RICE" (-40) (0, 0) "Your pokemon hates it and got a coronavirus, HP got decreased by 40"
    let item1 = Item "CHICKEN SOUP" (50) (0, 0) "Your pokemon is enjoying it. HP got increased by 50"
    let items = [item0, item1]
    return items

createItem 3 = do 
    let item0 = Item "WEISSWURST" 40 (0, 0) "Your pokemon is enjoying it! HP got increased by 40"
    let item1 = Item "BAT SOUP" (-20) (0, 0) "Your pokemon hates it and got a coronavirus, HP got decreased by 20"
    let items = [item0, item1]
    return items

    
    
{-|
Syntax:     createRoomPosition User GameMap Room
Purpose:    It is to create a random position for all position parameter in a room data type after user has finished one room or just started the game.
Process:    It will first generate 6 different random numbers which have 3 pairs of numbers.  It then passes it to the room data type.
            Then, we use the if statement to check if any one of these positions are the same, then also checks with the current position of the user.  
            If one of them are the same, it will call the function again to generate the number.  If they are not the same, it will return the Room data.
-}
createRoomPosition :: User ->  GameMap -> Room -> IO Room
createRoomPosition user (GameMap xup xbot yup ybot tf) (Room nam poke poke' pos1 pos2 pos3 tf' n) = do
    -- get the random number seed from randomNum method
    getseed <- randomNum
    -- use the mod function to make sure the number is within the range of xup (size of the map)
    let a = mod getseed xup
    getseed <- randomNum
    let a' = mod (getseed) xup
    let pos1 = (a, a')
    getseed <- randomNum
    let b = mod getseed xup
    getseed <- randomNum
    let b' = mod (getseed) xup
    let pos2 = (b, b')
    getseed <- randomNum
    let c = mod getseed xup
    getseed <- randomNum
    let c' = mod (getseed) xup
    let pos3 = (c, c')
    -- put these position into the parameter of this room
    let room = (Room nam poke poke' pos1 pos2 pos3  tf' n)
    -- check if any one of these position are the same
    if (getRoomPos room 1 /= getRoomPos room 2 && getRoomPos room 2 /= getRoomPos room 3 &&  getRoomPos room 1 /= getRoomPos room 3) then
        -- check if any one of these position are the same as user's position
        if (getRoomPos room 1 /= getUserPos user && getRoomPos room 2 /= getUserPos user && getRoomPos room 3 /= getUserPos user) then 
            return room
        else 
            -- if any one of these position are the same, it will call the method again to generate a new positions
            createRoomPosition user (GameMap xup xbot yup ybot tf) room
    else 
        createRoomPosition user (GameMap xup xbot yup ybot tf) room




{-|
Syntax:     createItemPosition User GameMap [Item] Room [Item] 
Purpose:    It is to create a random position for items  
Process:    It is a recursive method.  It will stop calling itself until (x:xs)the item list become empty.
            In each recursive call:
                It will generate a random position for (x) the first item on the list, then calls this position with all the room, items and user position.
                If it is true, it will put the item into the empty list which is the last parameter of this functions(items)
            Once the (xs) becomes empty, it will return
-}

-- this is the base case, it will be called when xs become empty
createItemPosition :: User -> GameMap -> [Item] ->  Room -> [Item] -> IO [Item]
createItemPosition user gameMap [] room items = do
    -- return the list of item 
    return items 
-- (x:xs) represents the current list of item
createItemPosition user (GameMap xup xbot yup ybot tf) (x:xs) room items = do
    -- get random number
    getseed <- randomNum
    let a = mod getseed xup
    getseed <- randomNum
    let a' = mod getseed xup
    let pos1 = (a, a')
    -- place it in the item by using addItemPosition
    let x' = addItemPosition x pos1
    -- use pass xs and room checkItemPosWithAllPos to gather all position in a list then use checkItemPosWithAllPos to compare the new item position to the result
    -- if the new item does not match any position of a room or any item...
    if (checkItemPosWithAllPos (gatherPosition room items) (get1ItemPos x') == True) && (get1ItemPos x' /= getUserPos user) then do
        -- it will add this item into items'
        let items' = (x':items)
        -- call the function again with new items' and (xs) the rest of the list without the list
        createItemPosition user (GameMap xup xbot yup ybot tf) xs room items'
    else do
        -- if the new item position matches one of these position, it will the function with the full list (x:Xs) to generate another pair of position
        createItemPosition user  (GameMap xup xbot yup ybot tf) (x:xs) room items


{-|
Syntax:     getWildPoke Int
Purpose:    This method return the wlid pokemon that user has met on the map.
Example:    Assume we have n = 1, then the output will be pokemon data - "lugia"
Process:    This method take a parameter n which is a random number generated in the Game.hs.  It passes this number to check which condition is met.
            After that, it will return the pokemon data associated with this number.
-}
getWildPoke :: Int -> Pokemon
getWildPoke n 
    | n == 1 = lugia
    | n > 1 && n < 10 = snorlax
    | n >= 10 && n < 50 = zubat
    | otherwise = psyduck
    where superTackle = Move "SUPER TACKLE" 50 20
          areoForce = Move "AREO FORCE" 100 2
          tackle = Move "TACKLE" 20 10
          fallingDown = Move "FALLING DOWN" 70 2
          zubat = Pokemon "ZUBAT" "NORMAL" 100 [tackle] True
          lugia = Pokemon "LUGIA" "GOD" 300 [superTackle, areoForce] True
          psyduck = Pokemon "PSYDUCK" "PSY" 100 [tackle] True
          snorlax = Pokemon "SNORLAX" "NORMAL" 150 [tackle, fallingDown] True