module Game(
start,

) where

import Data.Char(toUpper, isDigit, isSpace)
import BattleFunctions
import StageFunctions
import DeclaredData
import SupportFunctions
import GameData



{-|
Syntax:     It is called by pokemon method from 'StartHere.hs'
Purpose:    It is to take the user's name, ask for the size of the map, and create all the domestic pokemon and commands in the game world, then pass it to nextStage method to get room(level) data.
Process:    It first get all the commands fro createAllCommands, get the user data from creteUser, get the map from createMapSize and createMap and then create all the domestic pokemon data from createAllPoke method.
-}
start ::  IO()
start = do
    -- get all the commands for the game by using createAllCommands of GameData.hs
    getCommands <- createAllCommands
    let commands = getCommands
    -- create a user data by using createUser of GameData.hs
    getUser <- createUser 
    let user = getUser
    -- get a world size from the user by using createMapSize of GameData.hs
    mapSize <- createMapSize
    let n = mapSize 
    -- create the world by using createMap of GameData.hs
    getMap <- createMap n commands
    let gameMap = getMap
    -- get all the domestic pokemon by using createAllPoke of GameData.hs
    getAllPoke <- createAllPoke 
    let pokemons = getAllPoke
    putStrLn ("\nThere are three rooms to escape! In each room, you need to pick a pokemon from Professor OAK to unlock a room!\n")
    -- show instruction how to move in the game by using 'movingInstruction' of SupportFunctions.hs
    movingInstruction commands
    -- create a dummy Room data so it could be passed to nextStage for putting room data in it.
    let foo = Room "FOO" (pokemons!!0) (pokemons!!0)  (0, 0) (0, 0) (0, 0) False 1
    -- go to nextStage function
    nextStage user pokemons gameMap [] commands foo


{-|
Syntax:     nextStage [Pokemon] GameMap [Item][String] Room
            This method is being called either by lifeChecker or start.  If it is called by start, it means the user has just start the game.
            If it is called by lifeChecker, it means the user has escaped from one room and going to the next room.
Purpose:    This method either prepares the data for the next room or finish the game depending on the last parameter of the room data they received.
Process:    This method is defined in a pattern matching.  The diffrent pattern will be called depends on the last parameter of a Room data.
            If this parameter is 0, then it means that users have finished the game.
            If this parameter is n, it means the user is going from the nth room to second room.
            It also passes "n" to createItem and createRoom methods to get all the data that for the room to carry on the game.
-} 
-- The first pattern to be called to terminate the game
nextStage :: User -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room -> IO()
nextStage user pokemons gameMap items commands (Room name poke poke' pos1 pos2 pos3 tf 0) = do
     putStrLn "\nYou have escaped all the room! Thank you for your playing \n CREATED BY TSZ HANG CHAN FOR ASSIGNMENT2 OF FUNCTIONAL PROGRAMMING\n"
     
-- the second pattern to be called to get the room and item data for the next stage
nextStage user pokemons gameMap items commands (Room name poke poke' pos1 pos2 pos3 tf n) = do
    putStr "\nYou are now going to the next room!\n"
    -- reset users' position to the original position (0, 0)
    let user' = resetPos user
    -- getting room data from createRoom method from 'GameData.hs'
    newRoom <- createRoom n
    let room' = newRoom
    -- getting three random positions for createRoomPosition method from 'GameData.hs'
    newRoomWithRandom <- createRoomPosition user' gameMap room'
    -- declare the new room
    let room'' = newRoomWithRandom
    -- get the item list data by using createItem from RoomData.hs
    itemList <-createItem n
    let items' = itemList
    -- getting random positions for all items in the room by using createItemPosition from 'GameData.hs'
    newItemWithRandom <- createItemPosition user' gameMap items' room'' []
    -- declare the new items list
    let items'' = newItemWithRandom 
    stage user' pokemons gameMap items'' commands room''


{-|
Syntax:     Stage User [Pokemon] GameMap [Item] [String] Room
Purpose:    This is the game stage where the user can move around, each move will be checked whether users position is within the range of the map.  
            If it is valid it will pass this new position to eventTrigger to see if this move matches any position.  
            It also checks whether the command from users is to view the pocket.  If this is the case, it will call pocketMode.
Process:    It will first show the radar and then take the command from the user.  If the command is for showing pocket, it will call pocketMode method
            If the command is for moving, it will check if the move is out of the range of a map.  Then it will update and pass users' position to eventTrigger method.
-}
stage :: User -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room -> IO()
stage user pokemons gameMap items commands room = do
    putStrLn ("\nINSTRUCTION: type: " ++ chooseCommand commands 9 ++ " to view your pocket.\n") 
    -- convert a position that can be displayed on the rader (more detail showRadar on SupportFunctions.hs)
    let pos = gatherPositionWithChar room items
    -- show the radar by using showRadar
    showRadar gameMap user pos
    -- tell user what room are they in
    putStrLn ( "\n\nYou are in the room of " ++ getRoomName room++ "\nLet's MOVE!! " ++ getUserName user ++ "!\n")
    -- taking command from user
    option' <- getLine
    let option = map toUpper option'
    empty 
    -- check whether the command is equal to "POCKET", if it is true, then it will show user's pocket
    if option == chooseCommand commands 9 then do
        pocketMode user pokemons gameMap items commands room
    -- if the option does not match "POCKET", then it will do the following
    else do
        -- check whether option is the same as "W", "S", "A" or "D" the moving commands
        if option == chooseCommand commands 0 || 
            option == chooseCommand commands 1 || 
            option == chooseCommand commands 2 || 
            option == chooseCommand commands 3 then do
        -- If the commands matches one of the commands list, pass the option to moveChecker method to check whether user's position is out of range after the move.
            if moveChecker gameMap user option commands == True then do     
                --if user's move is within the range of map, it updates the current position of user by replacing the current one.
                let user' = (updateUserPos user option commands)
                
                -- pass the user position to eventTrigger to see if user's positon mataches any position of special event such as picking up an item.
                eventTrigger user' pokemons gameMap items commands room
            -- if user's move is out of the range of map, it will ask user to come back to the map.
            else do    
                putStrLn "You going too far, please come back!"
                stage user pokemons gameMap items commands room
        -- if the moving command does not match any commands in the list.
        else do
            wrongInput
            -- show the user one more time the moving instruction
            movingInstruction commands
            stage user pokemons gameMap items commands room


{-|
Syntax:     eventTrigger User [Pokemon] GameMap [Item] [String] Room
Purpose:    This is the method to check whether user position from the stage method matches any special location.
Process:    It first checks if the user's position matches any positions of:
            The first position of the room - Professor Oak(the guy who gives the pokemone to user), -> it will go to pickPokeMenu
            The second position of the room - the exit of the room -> go to the exit menu
            The third position of the room - pokemon centre(it heal all the pokemon) -> it will use pokeCentre method to heal the pokemon, and return to stage method
            any item position -> it will go to pickItemMenu

            If new position dos not match any one of the position above -> will generate a random number to see if the user would meet any wlid pokemon.
            If the random number is 5, then the user will go to capturePokeMenu where they can choose if they want to capture a pokemon.
            If the random number is not 5, it will go back to stage method to take another move from the users.
-}
eventTrigger :: User -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room -> IO()       
eventTrigger user pokemons gameMap items commands room = do
    -- checkEventPosition of 'StageFunctions.hs' will return different parameter of position in a room data depends on the int parameter.
    -- 1 is for professor Oak's position (the first position in the room data)
    -- 2 is for the exit position (the second position in the room data)
    -- 3 is for the pokemon centre. (the third position in the room data)

        -- if the user's postion is matching professor oak position and checkKey method returns false,
        -- 'checkKey' of 'StageFunctions.hs' is a method returns whether the user has already pick up a pokemon which they use for the key of the room
        if checkEventPosition user room 1 && checkKey room == False then do   
            -- call pickPokeMenu
            pickPokeMenu user pokemons gameMap items commands room
        -- check if the position is matching the exit
        else if checkEventPosition user room 2 == True then do 
            -- call exitMethod
            exitMethod user pokemons gameMap items commands room
        -- check if the position is matching the pokemon centre position
        else if checkEventPosition user room 3 == True then do    
            putStrLn "\n\n Nurse JOY: This is the [[pokemon centre]], we will heal all your pokemon!\n\n"
            -- heal all the pokemon by replacing the user data by the method of 'pokeCentre' of 'StageFunctions' 
            let user' = pokeCentre user
            stage user' pokemons gameMap items commands room 
        -- 'atPickItem' of 'StageFunctions.hs' method is to check all the position of the item in the game, and check whether user position is matching one of them.
        else if atPickItem user items == True then do   
            -- call the pickItemMenu for user to pick up the item by passing (passItem method which passes the item that the user has encountered) as parameter.
            pickItemMenu user pokemons gameMap items commands room (passItem user items)
        -- it indicate that the user's move has not matched anything in the map.
        else do
            -- generate a number 
            getseed <- randomNum
            let z = mod getseed 10
            -- it will be called if the number is 5 and the wildpokeSize is lower than or equal to 7.
            -- (the method will check if the wlid pokemon in users' list is more than 7)
            if z == 5 && getWildPokeSize user < 7 then do
                getseed <- randomNum
                let z = mod getseed 100
                -- generate another number between 1 to 100, it will be  used for the function to pick up a random wlid pokemon later on.
                -- go to capturePokeMenu
                capturePokeMenu user pokemons gameMap items commands room z
            else do      
                -- go back to stage method.
                putStrLn "\nNothing happen! Keep clam and carry on!\n"
                stage user pokemons gameMap items commands room 

            
   
--FUNCTION FOR SHOWING POCKET AND DELETING WILD POKEMON
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-|
Sytanx:     pocketMode User [Pokemon] GameMap [Item] [String] Room
Purpose:    This is to show the user's pocket containing all the pokemons and items. 
Process:    It will first show user all the item they have.  
            Then it takes the input from user to check if they want to either go back to the stage method or go to deleteMode to delete any wild Pokemon

-}
pocketMode :: User -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room -> IO()
pocketMode user pokemons gameMap items commands room = do
        putStrLn ("\n\nThis is "++getUserName user ++ "'s POCKET:\n\n")
        -- show users' all items and pokemons that user got
        showUserPocket user
        putStrLn ("INSTRUCTION: Type '" ++ chooseCommand commands 10 ++ "' To delete your wlid pokemon OR type '" ++ chooseCommand commands 11 ++"' to go back to main meun.\n\n")
        option' <- getLine
        let option = map toUpper option'
        -- if user press delete, it will go to the delete move
        -- it will only allow user to get in if their getWildPokeSize is bigger 0. (see more detail on getWildPokeSize of StageFunction)
        if chooseCommand commands 10 == option && getWildPokeSize user > 0 then do
            -- go to deleteMode
            deleteMode user pokemons gameMap items commands room 
        -- it will be called if user's pocket do not have any wlid pokemon
        else if chooseCommand commands 10 == option && getWildPokeSize user == 0 then do
            putStrLn " You do not have any wlid pokemon to delete."
            -- go back to stage method
            stage user pokemons gameMap items commands room
        -- user decided to go back to the main menu
        else if chooseCommand commands 11 == option then do    
            empty
            stage user pokemons gameMap items commands room
        else do
            wrongInput 
            pocketMode user pokemons gameMap items commands room

{-|
Syntax:     deleteMode User [Pokemon] GameMap [Item] [String] Room
Purpose:    This will be called after user decided to delete a pocket in a menu from pocketMode.  It will update user's pokemon list.
Process:    It will take an input from users to see if they want to go back to the main stage or delete a pokemon.
            If it is a number from users, the number will be used to identify the number position on the users' pokemon list and delete this pokemon.
            If the input is 'BACK', then the user will go back to the stage.
-}
deleteMode :: User -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room -> IO()
deleteMode (User name pos xs items') pokemons gameMap items commands room  = do
    putStrLn ("Type the number of the pokemon you want to remove or Press '" ++ chooseCommand commands 11 ++"' to go back to the main page.")
    -- take the input
    option <- getLine
    let option' = map toUpper option
    -- if user input BACK, it will go back to the stage method
    if option' == chooseCommand commands 11 then do
        stage (User name pos xs items') pokemons gameMap items commands room
    else do
        -- if user's input is a number, it will...
        if checkInput option' == True then do
            -- read the user input
            let x = (read option')
            -- check if the number is smaller than 0 or bigger than the size of the list
            if x <= 0 || x > checkPokeLeft (User name pos xs items') then do
                -- show wrong input
                wrongInput
                deleteMode (User name pos xs items') pokemons gameMap items commands room 
            else do
                -- if it is not smaller or equal to zero,
                -- it will first minus users' input by 1 so we can check the number position of a pokemon list by using !!.
                -- then use 'get1PokeBool' of 'StageFunction.hs' to check if it is a wlid pokemon 
                -- it also checks if user's has any pokemon in their list by using checkPokeLeft of 'StageFunction.hs'
                -- all of these boolean value have to be true.
                if get1PokeBool (xs!!(x-1)) == True && checkPokeLeft (User name pos xs items') >= x then do
                        putStrLn ("You have removed " ++ get1PokeName (xs!!(x-1)))
                        -- remove a pokemon from user by takePokeFromUser of 'BattleFunction.hs'
                        let user' = takePokeFromUser (User name pos xs items') (x-1)
                        stage user' pokemons gameMap items commands room
                -- show warning that user can only delete wild pokemon
                else if get1PokeBool (xs!!(x-1)) == False && checkPokeLeft (User name pos xs items') >= x  then do
                        putStrLn ("You can only remove wild pokemon!")
                        deleteMode (User name pos xs items') pokemons gameMap items commands room
                else do
                        wrongInput
                        deleteMode (User name pos xs items') pokemons gameMap items commands room 
        else do
            wrongInput
            deleteMode (User name pos xs items') pokemons gameMap items commands room 
                


        
--FUNCTION FOR PICKING UP A POKEMON FOR ESCAPING ROOM
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Syntax:  User [Pokemon] GameMap [Item] [String] Room
-- Purpose: This is called by the eventTrigger method for user to pick up the pokemon as a key to escape the room.
-- Process: It first show what pokemon options do the user have.  Then take the input from user and pass it to choosePoke method
pickPokeMenu :: User -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room -> IO()
pickPokeMenu user pokemons gameMap items commands room = do
    putStrLn ("\n Professor OAK:  Hey " ++ getUserName user ++ ", this is Professor Oak, please pick a pokemon to help you to escape this room!\n\n")
    putStrLn (" INSTRUCTION: Type the name of the pokemon to make a choice. (Think about the type of the room, you are now in a room of " ++ getRoomName room++")\n\n")
    -- show all the available pokemon option
    pokeOption pokemons
    --get the option from the user
    option'<- getLine
    empty
    let option = map toUpper option'
    -- make a backup data for this pokemon list
    let pokemons' = pokemons
    -- call choosePoke method
    choosePoke user pokemons gameMap items commands room option pokemons' 


{-|
Syntax:     choosePoke User [Pokemon] GameMap [Item] [String] [Room] String [Pokemon]
Purpose:    It is to choose a pokemon based on the input from pickPokeMenu method and then return a updated user data with a updated list of pokemons.
Process:    This is a recursive method:
            It has two base cases:
            1. The pokemon list in the game becomes empty
                It will stop the recursive call if the list has nothing left (In order words, the user's option is not matching any pokemon's name in the list.)
                then it will return the backup pokemons data back to pickPokeItem to ask user to re enter the option.

            2. User's option matches one of the pokemon's name in the list of the game.
                it will match the user's option with each pokemon's name in the game, 
                if it matches one of the pokemon in the game:
                    It will then check if this option matches the key of room.  
                    If this is the case, this pokemon will increase HP by 10, 
                    Otherwise, there is no bonus HP for such pokemon
            In each recursive call, it checks the first pokemon in the list with the user's option, 
            After each recursive call, it will take off the first pokemon. and then move on to the next pokemon in the list.
-}    
choosePoke :: User -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room -> String -> [Pokemon] -> IO()
-- this is called when the pokemon list in the game becomes empty.
choosePoke user [] gameMap items commands room option pokemons' = do
    -- show user that the input is incorrect and call the pickPokeMenu again.
    wrongInput
    -- it calls pickPokeMenu method witht the backup Pokemon list pokemons'.
    pickPokeMenu user pokemons' gameMap items commands room 

-- (x:xs) represents the list of the pokemon in the game
choosePoke user (x:xs) gameMap items commands room option pokemons'   = do
    -- it matches the user's input with the name of the first pokemon in the list(x) by using 'get1PokeName' of 'SupportFunction.hs' which returns a name of a pokemon
    -- if it matches the name, it will do the following.
    if option == get1PokeName x then

        -- 'isPokeKey' of 'StageFunction.hs' checks whether the pokemon is the current key of the room
        -- if user's option is the correct key
        if isPokeKey x room == True then do
            -- add bonus HP to user's option by using addHPToPoke of 'StageFunctions.hs'
            let userPoke = addHPToPoke x
            -- updata user's pokemon list by using addPokeToUser of 'StageFunctions.hs'
            let user' = addPokeToUser user userPoke
            -- change the room boolean value true by using obtainKeyPoke of 'StageFunctions.hs'
            let room' = obtainKeyPoke room
            -- remove the chosen pokemon from the pokemon list of the game by using removePokeFromMap of 'StageFunctions.hs'
            let pokemons = removePokeFromMap pokemons' x
            empty
            putStrLn ("\nProfessor OAK: "++get1PokeName x ++ " is an excellent choice! Your chosen pokemon HP will increase by 10!\n")
            stage user' pokemons gameMap items commands room'
        -- if user's option is not the correct key, it will do exactly same thing like above but without the 'addHPToPoke', 
        -- because it is without the bonus HP.
        else do
            let user' = addPokeToUser user x
            let room' = obtainKeyPoke room
            let pokemons = removePokeFromMap pokemons' x
            empty
            putStrLn ("\nProfessor OAK: "++get1PokeName x ++"????? Come on, this is such a bad choice! This is not what I expected! But I respect your choice!\n ")
            stage user' pokemons gameMap items commands room'   
            
    -- if the user's option does not match the current pokemon we are checking, 
    -- we will call the same method with the pokemon without the first pokemon(xs)
    else 
        choosePoke user xs gameMap items commands room option pokemons'




--FUNCTION FOR PICKING UP AN ITEM
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-|
Syntax:     pickItemMenu User [Pokemon] GameMap [Item] [String] Room Item
Purpose:    It is to let the user to choose to pick up the an item or not.  If they decide to take the item, it will update users' item list.
Process:    It first take the input from users then check if this option matches a string that is 'PICK UP' and the name of the item.  
            If it is, it will add 'item' to the item list of the user data.
            If user's input is 'LEAVE IT', it will go back to the stage function.
-}    
pickItemMenu :: User -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room -> Item-> IO()
pickItemMenu user pokemons gameMap items commands room item = do
        -- show what item they have encountered
        putStrLn ("\nThere is a " ++ get1ItemName item ++ ". Want to pick it up? It might come handy in a rainy day!")
        putStrLn ("\n\n INSTRUSTION: Type '" ++ chooseCommand commands 4 ++ get1ItemName item ++ "' to grab it, \n  or just type: '" ++ chooseCommand commands 6 ++ "' to leave it.\n\n")
        option' <- getLine
        empty
        let option = map toUpper option'
        -- check if the user's command is matching the fifth command in the command list which is "PICK UP "
        -- and also check whether the name of item is matching user's option as well
        -- if it matches...
        if option == chooseCommand commands 4 ++ get1ItemName item then do
            -- addItemToUser of 'StageFunctions.hs' is to update the user's item list
            let user' = addItemToUser user item
            -- removeItemFromMap of 'StageFunctions.hs' is to remove item from the game
            let items' = removeItemFromMap items item
            putStrLn ("\nYou got a " ++ get1ItemName item ++ "!\n You can use it in a battle!\n\n")
            -- return to the stage method 
            stage user' pokemons gameMap items' commands room
        -- if the user input matches the seventh command in the command list "LEAVE IT", then it will return back to stage method
        else if option == chooseCommand commands 6 then do
            putStrLn "\nOkay, it is your choice. But it looks so tasty tho.\n"
            stage user pokemons gameMap items commands room
        -- wrong input
        else do
            wrongInput
            pickItemMenu user pokemons gameMap items commands room item







--FUNCTION FOR CAPTURING POKEMON
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-|
Syntax:     capturePokeMenu :: User -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room -> Int 
Purpose:    Once user has encountered a wlid pokemon on eventTriiger method above, 
            this is to show the user that what pokemon have they met and they can select to capture it.
Process:    This method passed from eventTrigger method with the random number 'z', the method passes 'z' to getWildPoke to obtain the randomly chosen pokemon
            then take input from users.  If the input is YES, the method will updata user's pokemon list.  If the input is NO, it will go back to main menu.
-}    
capturePokeMenu :: User -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room -> Int -> IO()
capturePokeMenu user pokemons gameMap items commands room z = do
        -- encountedAPoke of 'StageFunctions.hs' shows what pokemon the users met and getWildPoke of 'GameData.hs' is to pass the wlid pokemon
        encountedAPoke (getWildPoke z) user commands
        option' <- getLine
        let option = map toUpper option'
        -- check if the input is "YES" or "NO"
        -- in case of "YES"
        if option == chooseCommand commands 7 then do
            -- add the pokemon to user data by using addPokeToUser of 'StageFunctions.hs'
            let user' = addPokeToUser user (getWildPoke z)
            putStrLn ("\n You got the wild " ++ get1PokeName (getWildPoke z))
            stage user' pokemons gameMap items commands room
        -- in case of "NO"
        else if option == chooseCommand commands 8 then do
            -- go back to the stage
            stage user pokemons gameMap items commands room
        else do
            wrongInput
            capturePokeMenu user  pokemons gameMap items commands room z








--FUNCTION FOR EXITING ROOM
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-|
Syntax:     exitMethod User -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room
Purpose:    It is the exitMethod called by the eventTrigger since user is in the position of the exit.
            It can lead user to the battle mode.

Process:    It first check if the user has got the pokemon ("checkKey" method), it will ask if the user want to exit the room.
            If they want to exit the room, then the battlePrep function will be called.
            It the checkKey method returns False, it will then ask the user to come back after they got the pokemon as a key.
-}    
exitMethod :: User -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room -> IO()
exitMethod user pokemons gameMap items commands room = do
    -- 'checkKey' of SupportFunctions.hs is to check if the bool value of the room is True(it indicates if the user has got a pokemon as a key for exiting the room)
    -- if it is true..
    if checkKey room == True then do
        -- ask if the user want to exit the room
        putStrLn ("\nThis is the exit!\n\nDo you want to escape from this room now? \n Type " ++ chooseCommand commands 7 ++ " to escape or type " ++ chooseCommand commands 8)
        option' <- getLine
        empty
        let option = map toUpper option'
        -- if it matches the 8th command in the commands "YES", it will call battlePrep method to prepare the method
        if (option == chooseCommand commands 7) then do
            putStrLn "\nYou are now using your pokemon to escape this room! \n"
            battlePrep user pokemons gameMap items commands room
        -- if it matches the 9th command in the commands "NO", it will go back to stage method.
        else if option == chooseCommand commands 8 then do
            putStrLn "\nCome back when you are ready!\n"
            stage user pokemons gameMap items commands room
        -- if it does not matches any command, call the method again with the warning.
        else do
            wrongInput
            exitMethod user pokemons gameMap items commands room
    -- if the checkKey returns a False value. it will call the stage method again.
    else do
            putStrLn "\nYou don't have a pokemon for escaping this room, go look for Professor Oak!\n"
            stage user pokemons gameMap items commands room






--FUNCTION FOR BATTLEMODE
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-|
Syntax:     battlePrep User -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room
Purpose:    This method is to seperate the boss from the user's data and then pass all the parameter to choosePokeToBattle.  
            It also prepare the backup data for user in case they lose the fight.
Process:    It first create a variable boss and then put the third parameter of the room data in it.
            It then shows the name of the boss to the user.  Next, it back up user data in 'userBackup'.  Then it calls 'choosePokeToBattle'
-}  
battlePrep :: User -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room -> IO()
battlePrep user pokemons gameMap items commands (Room name poke poke' pos1 pos2 pos3 tf n) = do
    -- create a boss by using the third parameter of the room
    let boss = poke'
    putStrLn ("\n\nHOWEVER, there is a " ++ get1PokeName boss ++ " standing in your way.  You need to battle it in order to escape!\n\n")
    -- create a backup data for user in case user lose the fight
    let userBackup = user
    pause
    -- call choosePokeToBattle
    choosePokeToBattle user boss pokemons gameMap items commands (Room name poke poke' pos1 pos2 pos3 tf n) userBackup


{-|
Syntax:     choosePokeToBattle User -> Pokemon -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room -> User
Purpose:    This method is to let user to choose the pokemon they have in their list to battle the boss before going to battleField
Process:    It first shows what option the user have and then, take a number as a option,  
            then use this number as a number position to get the pokemon from the user data pokemon list 'xs'. 
             Then put this chosen pokemon in a varaible and call 'battleField' method
-}  
choosePokeToBattle:: User -> Pokemon -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room -> User-> IO()
choosePokeToBattle (User name pos xs items') boss pokemons gameMap items commands room userBackup = do
    -- showing what pokemon they have 
    pokeBattleOption (User name pos xs items') 1
    -- take the input
    option' <- getLine
    empty
    -- if the input is a number then, it will return a true value 
    if checkInput option' == True then do
            -- convert the string into number
            let option = (read option') ::Int
            let x = option-1
            -- check if the input is bigger than 0 or bigger than the size of the user's pokemon list..
            -- If it is the case...
            if x >= 0 && x <= ((checkPokeLeft (User name pos xs items'))-1) then do    
                -- pick the pokemon from the user list (xs) by using !! operator, put it into poke
                let poke = (xs!!x)
                -- take the pokemon from the user by using takePokeFromUser of BattleFunctions.hs
                let user' = takePokeFromUser (User name pos xs items') x
                -- call battleField
                battleField user' poke boss pokemons gameMap items commands room userBackup
            else do
                wrongInput 
                choosePokeToBattle (User name pos xs items') boss pokemons gameMap items commands room userBackup
    else do
        wrongInput 
        choosePokeToBattle (User name pos xs items') boss pokemons gameMap items commands room userBackup


{-|
Syntax:     battleField  User -> Pokemon -> Pokemon -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room -> User 
Purpose:    This method is being called by battlePrep.  It show all the option that the user can use namely, the move and item.
            Then take the input from the user and pass it to either the attackMode or useItemMode
Process:    This method first check if the boss and user have got any EN(how many times that user can use the move) in all their move.
            This is to prevent that either one of them has ran out of the move in a battle.
            If it happens, the program created a new move to add it to their pokemon to carry on the battle.
            Then, it takes the input from the user to check if they want to use item or move to attack.
            After that, depends on the input, it will either call attackMode or useItemMode
            
-} 
battleField :: User -> Pokemon -> Pokemon -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room -> User -> IO()
battleField user poke boss pokemons gameMap items commands room userBackup  = do 
    --checkAllMovesEN is to check all the move of a pokemon's EN.  If all of them are lower than 0, then it will return false value.
    -- this is to check user's pokemon...
    if checkAllMovesEN poke == False then do
        --create a new move
        let basic = (Move "BASIC MOVE" 10 1)
        -- add this move in pokemon by using addBasicMove method of 'BattleFunctions.hs'
        let poke' = addBasicMove poke [basic]
        putStrLn ("You have ran out of all the moves, but you can still use " ++ get1MoveName basic) 
        battleField user poke' boss pokemons gameMap items commands room userBackup
    -- this is to check the boss's EN
    else if checkAllMovesEN boss == False then do
        let basic = (Move "BASIC MOVE" 10 1)
        let boss' = addBasicMove boss [basic, basic]
        putStrLn (get1PokeName boss' ++" ran out of all the moves, and starts using " ++ get1MoveName basic)
        battleField user poke boss' pokemons gameMap items commands room userBackup
    -- if both of pokemons have EN, then the user can choose the move to attack or choose an item to use.
    else do
        -- showBattleMenu shows what item and move that user can use
        showBattleMenu poke boss user commands
        --take the input
        option' <- getLine
        empty
        let option = map toUpper option'
        
        -- pickMoveInBattle of BattleFunctions.hs checks if the options is matching one of the move from the pokemon.
        if pickMoveInBattle poke commands option == True then do
            -- it is to back up a extra poke data
            let poke' = poke
            -- call attackMode with user's input to start the attack
            attackMode user poke boss pokemons gameMap items commands room option poke' userBackup
        -- pickItemInBattle checks if the options is matching one of the item of users.
        else if pickItemInBattle user commands option == True then do
            -- back up an extra user data
            let user' = user
            -- call useItemMode to pick up an item.
            useItemMode user poke boss pokemons gameMap items commands room option user' userBackup
        -- input does not match any commands. 
        else do
            wrongInput
            battleField user poke boss pokemons gameMap items commands room  userBackup
    

{-|
Syntax:     attackMode User -> Pokemon -> Pokemon -> [Pokemon]-> GameMap-> [Item] ->[String] ->  Room ->String -> Pokemon -> User
Purpose:    This method is being called by battleField.  It updated all users and boss's HP and EN after the one round of battle.
            Afterward, it passes these value to lifeChecker to check if the pokemon and boss are still alive.

Process:    
            It will first generate the a random number for the boss the pick up a move.  
            Then, check if this move still got EN.  If not, the function will call itself again.
            Next, it will check if users' input matches one of the move from the pokemon.  This will be checked by using the recursive method.
            It has two base case:
            1. The list of move in a user's pokemon become empty.  It means the input does not match any move in the list, 
            it will return to the battleField to ask user to input a new move.
            
            2. user's input matches one of the move in the list:
            it will update the EN and HP of both pokemon and then call lifeChecker with these parameters.

            In each recursive call, If users option does not match the current move we are checking, it will take off the first move in the list and call the attackMode again with the list without the first move.
-} 
-- this is the first base case, it happens when the list of move become empty because user's input does not match any move's name in the list
attackMode :: User -> Pokemon -> Pokemon -> [Pokemon]-> GameMap-> [Item] ->[String] ->  Room ->String -> Pokemon -> User -> IO()
attackMode user (Pokemon name type' hp [] tf) boss pokemons gameMap items commands room option poke' userBackup  = do 
    wrongInput
    -- call the battleField method to ask user to input a new input again with poke'(the back up data of pokemon)
    battleField user poke' boss pokemons gameMap items commands room userBackup

-- (x:xs) represents the list of move of the user's pokemon
attackMode user (Pokemon name type' hp (x:xs) tf) boss pokemons gameMap items commands room option poke' userBackup   = do
    -- z is a random number
    -- so the boss can randomly choose a move between two moves
    -- z also represents the number position of the move
    -- if z is 1 then, the number position will be 1 and the first move of the list should be picked.
    -- (more detail: check attackUser method on SupportFunctions.hs)
    getseed <- randomNum
    let z = mod getseed 10
    -- it is to check if boss' chosen move has enough EN to attack by using getBossEN of BattleFunctions.hs
    -- if boss' move EN is bigger than 0
    if getBossEN boss z > 0 then
        --  it checks whether the sixth command in the command list("USE ") together the move name matches the user's input.
        -- also the get1MoveEN of BattleFunctions.hs is to check whether this move chosen by the user has enough EN.
        -- if this is the case
        if (option == chooseCommand commands 5 ++ get1MoveName x) && (get1MoveEN x > 0) then do
            -- attackBoss of 'BattleFunctions' is to update boss' EN and HP after one round of the battle
            let boss' = attackBoss boss x z
            -- attackUser of 'BattleFunctions' is to update user's pokemon's EN and HP after one round of the battle
            let poke = attackUser poke' x boss' z
            -- showUSerMove and showBossMove of 'BattleFunctions.hs' show what move they have used in this round
            showUserMove x
            showBossMove boss' z
            -- pass boss and poke to lifeChecker to check their HP level.
            lifeChecker user poke boss' pokemons gameMap items commands room userBackup
        -- if get1MoveEN is 0, then the move has no EN left and will return to battleField method to get the input from user again
        else if (option == chooseCommand commands 5 ++ get1MoveName x) && (get1MoveEN x == 0) then do
            putStrLn "\nYou don't have EN left. Choose another move\n"
            battleField user poke' boss pokemons gameMap items commands room  userBackup
        -- if the current move we are checking does not match user's input, it will call the same method with 'xs'(the move list without the current move)
        else 
            attackMode user (Pokemon name type' hp xs tf) boss pokemons gameMap items commands room option poke'  userBackup
    -- this happens in case the boss randomly pick a move which has no EN left, then it will call the attackMode function again to generate a new random number
    -- until the boss pick a move which got EN to attack.
    else 
        attackMode user (Pokemon name type' hp (x:xs) tf) boss pokemons gameMap items commands room option poke'  userBackup 



{-|
Syntax:     useItemMode User -> Pokemon -> Pokemon -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room -> String -> User -> User
Purpose:    This method is being called by battleField.  
            It updated all users' HP and item list and boss' EN after the user has used an item and the boss has attacked.
            Afterward, it passes these value to lifeChecker to check if the pokemon and boss are still alive.
Process:    This method is similiar to 'attackMode'.  It first generate a number for the boss to pick move.  Then it checks if users' input
            matches the name of the item from the list.

            This method is also a recursive method.
            It has two base case:
            1. the list of item in a user become empty
            It means the input does not match any item in the list, it will return to the battleField to ask user to input a new input
            2. user's input matches one of the item in the list.
            After this, it will update the EN and HP of both pokemon and then call lifeChecker with these parameters.
            it also updated user's item list.

            In each recursive call, if the item does not match the users' input,
            it will take off the first item in the list and call the attackMode again with this updated list without the first item.

-} 
-- this is the first base case, it happens when the list of item become empty because user's input does not match any item name in the list
useItemMode :: User -> Pokemon -> Pokemon -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room -> String -> User -> User -> IO()    
useItemMode (User name pos pokes []) poke boss pokemons gameMap items commands room option user' userBackup   = do
    wrongInput
    -- call the battleField method to ask user to input a new input again with the backup user data - user'
    battleField user' poke boss pokemons gameMap items commands room userBackup

--(x:xs) represents the list of item
useItemMode (User name pos pokes (x:xs)) poke boss pokemons gameMap items commands room option user' userBackup   = do
        getseed <- randomNum
        let z = mod getseed 10
        if getBossEN boss z > 0 then
        -- if the user's input match the first item of the list...
            if(option == chooseCommand commands 5 ++ get1ItemName x) then do
                -- it will update the pokemon's HP by useItemToPoke of 'BattleFunctions.hs'
                let poke' = useItemToPoke poke x
                -- it will remove an item from user's list by removeItemFromUser of BattleFunctions.hs
                let user'' = removeItemFromUser user' x
                -- attackUserInItemMode of 'BattleFunctions.hs' updated the user's pokemon's HP after the boss has attacked
                let poke'' = attackUserInItemMode poke' boss z
                -- minusBossENInItemMode of 'BattleFunctions.hs' updated the boss' EN after it attaacked
                let boss' = minusBossENInItemMode boss z
                -- show what is the effect of the item
                showItemDesc x
                -- show what move did the boss use
                showBossMove boss' z
                -- call lifeChecker to check if either boss or pokemon are still alive
                lifeChecker user'' poke'' boss' pokemons gameMap items commands room  userBackup
            -- if the current item we are checking does not match user's input, it will call the same method with 'xs'(the item list without the current item) 
            else
                useItemMode (User name pos pokes xs) poke boss pokemons gameMap items commands room option user'   userBackup
            -- if the boss picked a move that ran out of EN
        else    
            useItemMode (User name pos pokes (x:xs)) poke boss pokemons gameMap items commands room option user' userBackup




        
{-|
Syntax:     lifeChecker  User -> Pokemon -> Pokemon -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room -> User 
Purpose:    This method is being called by either attackMove or useItemMode.  It checks both user and boss' HP.
            It is a method to determine if users have won the battle or not.
Process:    It will check user and boss HP.  
            There are 6 different if statement:
                1. if user's pokemon is higher than 0 and the boss is lower or equal to 0, it will call nextStage
                2. if users' pokemon and boss' HP have reached to 0 and users' pokemon list is greater than 0, it will call nextStage
                3.  if user's pokemon and boss HP have reached to 0 and user's pokemon list is  0, it will call deadOption
                4.   if user's pokemon HP is lower or equal to 0 and boss HP is bigger than 0 and user's pokemon list is 0, it will call deadoption
                5.  if user's pokemon HP is lower or equal to 0 and boss HP is bigger than 0 and user's pokemon list is bigger than 0, it will call choosePokeToBattle 
                6. any other option, it will go to battleField
-} 
lifeChecker :: User -> Pokemon -> Pokemon -> [Pokemon] -> GameMap -> [Item] -> [String] -> Room -> User -> IO()
lifeChecker user poke boss pokemons gameMap items commands room userBackup  = do
    -- these if statement is to check user's and boss' HP level and length of user's pokemon list.
    -- get1PokeHP of 'SupportFunctions.hs' is to return the pokemon's HP
    -- checkPokeLeft of 'SupportFunctions.hs' is to return the size of user's pokemon list.

    -- if user's pokemon is higher than 0 and the boss is lower or equal to 0
    if get1PokeHP poke > 0 && get1PokeHP boss <= 0 then do
        putStrLn ("\n"++get1PokeName boss ++ " can't fight anymore! You won the fight!\n")
        -- addPokeBack put the pokemon in a battle back to user's pokemon's list.
        let user' = addPokeBack user poke
        pause
        -- call nextStage to go to the next room
        nextStage user' pokemons gameMap items commands room 
    -- if user's pokemon and boss HP have reached to 0 and user's pokemon list is greater than 0
    else if get1PokeHP poke <= 0 && get1PokeHP boss <= 0 && checkPokeLeft user > 0 then do
        putStrLn ("\nBoth your pokemon and " ++ get1PokeName boss ++ " can't fight anymore.  But you still got Pokemon left. \nYou won the fight!\n")
        pause
        -- call nextStage to go to the next room
        nextStage user pokemons gameMap items commands room 
    -- if user's pokemon and boss HP have reached to 0 and user's pokemon list is  0
    else if get1PokeHP poke <= 0 && get1PokeHP boss <= 0 && checkPokeLeft user == 0 then do
        putStrLn ("\nBoth your pokemon and " ++ get1PokeName boss ++ " can't fight anymore.  But you don't have any pokemon left. \nYou Lost!\n")
        pause
        -- go to deadOption method.
        deadOption userBackup pokemons gameMap items commands room 
    -- if user's pokemon HP is lower or equal to 0 and boss HP is bigger than 0 and user's pokemon list is 0
    else if get1PokeHP poke <= 0 && get1PokeHP boss > 0 && checkPokeLeft user == 0 then do
        putStrLn "\nYour Pokemon can't fight anymore, you lost!\n"
        pause
        -- call the deadOption method
        deadOption userBackup pokemons gameMap items commands room 
    -- if user's pokemon HP is lower or equal to 0 and boss HP is bigger than 0 and user's pokemon list is bigger than 0
    else if get1PokeHP poke <= 0 && get1PokeHP boss > 0 && checkPokeLeft user > 0 then do
        putStrLn ("\n"++get1PokeName poke ++ " can't fight anymore, choose another poke from your pocket to flight!\n")
        pause
        -- call choosePokeBattle again to start the fight with different pokemon
        choosePokeToBattle user boss pokemons gameMap items commands room userBackup
    -- it happens when both pokemon HP are higher than 0, it call battleField to start the next round of battle
    else do
        battleField user poke boss pokemons gameMap items commands room userBackup

        
{-|
Syntax:     deadOption User -> Pokemon ->  GameMap -> [Item] -> [String] -> Room 
Purpose:    This method is being called by the lifeChecker. after user has lost the battle with the boss. 
            This game will ask users if they want to restart the battle or not.

Process:    This method will take input from users.  If the input is YES, then call the battlePrep method.  If the input is NO, then terminate the method.
-} 
deadOption :: User -> [Pokemon] ->  GameMap -> [Item] -> [String] -> Room -> IO()
deadOption user pokemons gameMap items commands room = do
    putStrLn ("\nShall we try again?\n Type " ++ chooseCommand commands 7 ++ " to restart the battle with the boss or type " ++ chooseCommand commands 8 ++ " to quit the game")
    -- take the input from user
    option' <- getLine
    empty
    let option = map toUpper option'
    -- if input is "YES", call battlePrep method to restart the game
    if option == chooseCommand commands 7 then
        battlePrep user pokemons gameMap items commands room
    -- if input is "NO", show string and terminate the game
    else if option == chooseCommand commands 8 then do
        putStrLn "Bye for now!!"
    -- if the input does not match...
    else do
        wrongInput
        deadOption user pokemons gameMap items commands room