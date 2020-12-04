module BattleFunctions (
pokeBattleOption,
takePokeFromUser,
showBattleMenu,
pickMoveInBattle,
pickItemInBattle,
get1MoveEN,
getBossEN,
attackBoss,
attackUser,
showBossMove,
showUserMove,
get1MoveName,
useItemToPoke,
removeItemFromUser,
attackUserInItemMode, 
minusBossENInItemMode,
showItemDesc,
addPokeBack,

checkAllMovesEN,
addBasicMove

) where

import DeclaredData
import SupportFunctions




{-|
Syntax:     checkAllMovesEN Pokemon
Purpose:    This method will return True value When a user and boss have got EN left in any of their moves,

Example:    Assume the user has 0 EN in all his moves then the output will be False.

Process:    This method takes a parameter: (Pokemon name type' hp (x:xs))(Pokemon data)
            Then it passes x(the first move in the list) to get1MoveEN to check if the EN is higher than 0.  If this is the case, it returns true value
            otherwise, it will place xs(the rest of the list) in a pokemon data, and then call itself again with this parameter.
            It will stop calling itself until xs becomes empty, then it will return false.
-}
checkAllMovesEN :: Pokemon -> Bool
checkAllMovesEN (Pokemon name type' hp [] tf) = False
checkAllMovesEN (Pokemon name type' hp (x:xs) tf) = if get1MoveEN x > 0 then True else checkAllMovesEN (Pokemon name type' hp (xs) tf)



{-|
Syntax:     addBasicMove Pokemon [Move]
Purpose:    After the pokemon has ran out of their moves, this method will update a move list with a basicMove in the pokemon data.
            This will put that basic move to the pokemon so the user can carry on the battle in case they ran out of move.

Example:    Assume the user has 0 EN in all his moves then the output will be: a pokemon data with a list of move [basicMove]


Process:    This method takes a parameter: (Pokemon name type' hp moves)(user data) basicMove(the new move)
            It will replace the current list(moves) with the basicMove in the pokemon data.
-}
addBasicMove :: Pokemon -> [Move] -> Pokemon
addBasicMove (Pokemon name type' hp moves tf) basicMove = (Pokemon name type' hp basicMove tf)



{-|
Syntax:     pokeBattleOption User Int
Purpose:    When a user is about to battle the boss of the room, they will have to choose their pokemon to battle.
            This method will return a menu where the user can see their pokemon options.

Example:    Assume the user has two pokemons (Name: Hello, HP: 100, Move: Tackle) and (Name: World, HP: 100, Move: Tackle)
            The output will be:

            Option 1: NAME: [[Hello]] HP: 100
                    MOVE: Tackle POWER: 100 EN: 2

            Option 2: NAME: [[World]] HP: 100
                    MOVE: Tackle POWER: 100 EN: 2

            INSTRUCTION: type the name of a pokemon to make a choice to fight the boss!
             - HP: Health Point of a Pokemon 
             - MOVE: The attack move of a pokemon 
             - EN: Times that a pokemon can use this move

Process:    This method takes two parameters: user(the User data) and n(The number display the order of the pokemon)
            It will first pass x(The first item on the list) to get1PokeName and get1PokeHP to get the pokemon's name and HP.
            Then, it will return a string showing these two value.  
            Then, it will pass x and n to the showMoveInBattle method to get the list of this pokemon's moves.
            Once (x:xs) becomes empty, it will show return a string showing the instruction how to operate this menu.
-}
pokeBattleOption :: User -> Int -> IO ()
pokeBattleOption (User name pos [] items) n = do 
    putStrLn "---\n INSTRUCTION: type the NUMBER of a pokemon to make a choice to fight the boss!\n - HP: Health Point of a Pokemon \n - MOVE: The attack move of a pokemon \n - EN: Times that a pokemon can use this move\n\n"

pokeBattleOption (User name pos (x:xs) items) n = do 
    putStrLn ("Option " ++ show(n) ++": NAME: [["++get1PokeName x  ++ "]] HP: " ++ show(get1PokeHP x))
    showMoveInBattle x n
    putStrLn "\n"
    pokeBattleOption (User name pos xs items) (n+1)


{-|
Syntax:     showMoveInBattle Pokemon Int
Purpose:    This method is to return all the pokemon's moves to pokeBattleOption method and showBattleMenu
Example:    Assume the user has two pokemons (Name: Hello, HP: 100, Move: Tackle)
            The output will be:
            
                    MOVE: Tackle POWER: 100 EN: 2
Process:    This method takes two parameters: (Pokemon name type' hp' (x:xs))(The pokemon we are showing all the moves) and n(the number of each move)
            It will first pass x(the first element of the list) to get1MoveName, get1MovePower and get1MoveEN to get the name of a move and its power and EN.
            It then returns all these values in a string with the value of 'n'
            Then, it will place xs(the rest of the list) in the pokemon data.  The method call itself with this pokemon data and (n+1).
            It will stop once (x:xs) becomes empty. 
-}
showMoveInBattle :: Pokemon -> Int -> IO()
showMoveInBattle (Pokemon name type' hp' [] tf) n = do 
    putStrLn " "
showMoveInBattle (Pokemon name type' hp' (x:xs) tf) n = do  
    putStrLn ("         ||Move " ++ show(n) ++ ": " ++ get1MoveName x ++ " POWER: " ++ show(get1MovePower x) ++ " EN: " ++ show(get1MoveEN x)++"||")
    showMoveInBattle (Pokemon name type' hp' xs tf) (n+1)

 

{-|
Syntax:     takePokeFromUser User Pokemon
Purpose:    This method is to return a user data with their updated list of pokemon after user has chosen a pokemon to battle.
Example:    Assume the user has two pokemons in their list [poke1, poke2]. and users have chosen poke2 to battle
            The output will be: (User name pos [poke1] items)
Process:    This method takes two parameters: (User name pos pokes items)(the user data) n(the number position of the pokemon)
            It will first pass pokes(The list of user's pokemons) and n to updataUserPokeList to get the list updated.
            It will then replace the current pokes with the new value returned. 
-}
takePokeFromUser :: User -> Int -> User
takePokeFromUser (User name pos pokes items) n = (User name pos (updateUserPokeList pokes n) items)



{-|
Syntax:     updateUserPokeList [Pokemon] Pokemon
Purpose:    This method is to return a updated pokemon list of user back to takePokeFromUser method
            
Example:    Assume the user has two pokemons in their list [poke1, poke2]. and users have chosen poke2 to battle
            The output will be: [poke1]

Process:    This method takes two parameters: (x:xs)(users' list of pokemon) n(the number position of the pokemon).
            It will first check if n is 0 or not.  If it is 0, it will put x in front of the list and then call the same method with xs(without the first element), and (n-1)
            Once n is zero, it will return the xs.
-}
updateUserPokeList :: [Pokemon] -> Int -> [Pokemon]
updateUserPokeList (x:xs) n = if n /= 0 then x:updateUserPokeList xs (n-1) else xs




{-|
Syntax:     showBattleMenu Pokemon Pokemon User [String]
Purpose:    It shows users' pokemon and boss's HP.
            This method also returns a string showing what are the move and item options that user can use to attack the boss in a battle.
            
            
Example:    Assume the user has a pokemon(hello) has a list of move [move1, move2] and the boss is (world)
            The output will be :
            [[hello HP: 100 VS world HP: 100]]
              INSTRUCTION: Type 'USE MOVE NAME' to attack 
              or type 'USE ITEM NAME' to use the item.
            
            The list of moves
            The list of items

Process:    This method takes four parameters: poke(the pokemon in battle), boss(the boss of the room), user(user data), commands(the commands list)
            It first passes poke and boss to get1PokeName and get1PokeHP to get the HP and name
            Then, it passes poke to showMoveInBattle and user to showItemInBattle to get the list of moves and items of the users.
            Then, it will return a string with all these value.
-}
showBattleMenu :: Pokemon -> Pokemon -> User -> [String] ->IO()
showBattleMenu poke boss user commands = do
    putStrLn ("\n\n[["++get1PokeName poke ++ " HP: " ++ show(get1PokeHP poke) ++  " VS " ++ get1PokeName boss ++ " HP: " ++ show(get1PokeHP boss) ++ "]]\n\n")
    putStrLn (" INSTRUCTION: Type '" ++ chooseCommand commands  5 ++ " MOVE NAME' to attack \n  or type '" ++ chooseCommand commands  5 ++ " ITEM NAME' to use the item.'\n\n" )
    showMoveInBattle poke 1
    showItemInBattle user 1



{-|
Syntax:     showItemInBattle User Int
Purpose:    It return all users' item to showBattleMenu during the battle.
        
            
Example:    Assume the user has a list of item [item1, item2]
            the outputwill be:
            Item 1: item1
            Item 2: item2

Process:    This method takes two parameters: (User name pos pokes (x:xs))(the user data) n(the number position)
            It first pass x(the first item) ti get1ItemName to get the name.  Then, it will pass xs(the rest of the list) to showItemInBattle to call itself again with (n+1)
            It will stop stop calling itself once the list become empty and return a space string.
-}
showItemInBattle :: User -> Int -> IO()
showItemInBattle (User name pos pokes []) n = putStrLn " "
showItemInBattle (User name pos pokes (x:xs)) n = do
    putStrLn ("Item " ++ show(n) ++ ": " ++ get1ItemName x)
    showItemInBattle (User name pos pokes xs) (n+1)



{-|
Syntax:     pickMoveInBattle Pokemon [String] String
Purpose:    It checks whether the users' input is matching one of the moves in the list during the battle.  If it matches one of these move, it will return true value.
        
Example:    Assume the user has input "USE MOVE1". And we have a list of move. [MOVE1, MOVE2], and also, command "USE" is also in our command list.
            The output will be true.
            

Process:    This method takes three parameters: (Pokemon name type' hp (x:xs))(the pokemon in battle) commands(a list of commands) option(users input)
            It passes x out of (x:xs)(the list of move) to get1MoveName to get the name of the move.  Then it concatenate the result of (chooseCommand commands 5) which takes
            the sixth commands on the commands list(USE ).  Then it compares the input from user to this value.  If it is the same, it will return True value.
            otherwise, it will pass xs(the rest of list), commands and option to the same method. 
            It will stop calling itself once the (x:xs) becomes empty

-}
pickMoveInBattle :: Pokemon -> [String] -> String -> Bool
pickMoveInBattle (Pokemon name type' hp [] tf) commands option = False
pickMoveInBattle (Pokemon name type' hp (x:xs) tf) commands  option = 
    if option == chooseCommand commands  5 ++ get1MoveName x then 
        True 
    else 
        pickMoveInBattle (Pokemon name type' hp xs tf) commands option



{-|
Syntax:     pickItemInBattle User [String] String
Purpose:    It checks whether the users' input is matching one of the item in their item list during the battle.  If it matches one of these, it will return true value.
Example:    Assume the user has input "USE ITEM1". And we have a list of move. [ITEM1, ITEM2], and also, command "USE" is also in our command list.
            The output will be true.
Process:    This method takes three parameters: (User name pos pokes (x:xs))(the user data) commands(a list of commands) option(users input)
            It passes x out of (x:xs)(the list of item) to get1ItemName to get the name of the item.  Then it concatenate the result of (chooseCommand commands 5) which takes
            the sixth commands on the commands list(USE ).  Then it compares the input from user to this value.  If it is the same, it will return True value.
            otherwise, it will pass xs(the rest of list), commands and option to the same method. 
            It will stop calling itself once the (x:xs) becomes empty
-}
pickItemInBattle :: User -> [String] -> String -> Bool
pickItemInBattle (User name pos pokes []) commands option = False
pickItemInBattle (User name pos pokes (x:xs)) commands option = 
    if option == chooseCommand commands  5 ++ get1ItemName x then
        True
    else 
        pickItemInBattle (User name pos pokes (xs)) commands option


{-|
Syntax:     get1MoveEN Move
Purpose:    It returns the EN value of the move
Example:    Assume the we have get1MoveEN (Move name hp 20) = 20   
Process:    It returns the last parameter of a move.
-}
get1MoveEN :: Move -> EN
get1MoveEN (Move name hp en) = en


{-|
Syntax:     getBossEN Pokemon Int
Purpose:    It returns the EN value of boss' move.  Since boss move is randomly picked, this method will act as a checker to make sure the boss has enough EN to use this move.
Example:    Assume the the boss has a move tackle and EN 1.
            The output will be 1.
            
Process:    It takes two parameters: (Pokemon name type' hp moves)(boss) z(it represents the number position of the move in the list).  
            It first passes z and moves to getBossMoveNum to get the a number between 1 and 2.  
            Since z could be between 1 and 10 and the boss only has 2 moves, therefore, we need to pass 1 and 2 to the operator '!!'.
            If z is 1, then this method will return 1 to get the first move, if z is between 2 and 10, it will returns 2 to get the second move.
            Then, we use get1MoveEN to get the EN of such move.

-}
getBossEN :: Pokemon -> Int -> EN
getBossEN (Pokemon name type' hp moves tf) z = get1MoveEN (pickBossMove moves z)



{-|
Syntax:     attackBoss Pokemon Move Int
Purpose:    It updated the boss HP and its move EN after user attacked.
Example:    Assume the user attacked boss and its HP went down to 70 and boss used move1 with only 1 EN left.
            attackBoss (Pokemon name type' hp moves) (Move name' power en) z =  (Pokemon name type' 70 (moves EN=1))  
Process:    It takes three parameters: (Pokemon name type' hp moves)(the boss) (Move name' power en)(user's move) z(the number to pick the boss' move)
            It will first pass moves(the boss' list of move) and z(the number to pick a move) to pickBossMove to get the move from the boss.
            If z is 1, then this method will return 1 to get the first move, if z is between 2 and 10, it will returns 2 to get the second move.
            Then passes it to changeEN to minus 1 from the EN.    
            Then, use passes the values to the replaceBossMove to get the boss' move list updated.
            Finally, hp(user's pokemon HP minus the power of boss' move.)
-}
attackBoss :: Pokemon -> Move -> Int -> Pokemon
attackBoss (Pokemon name type' hp moves tf) (Move name' power en) z = 
    (Pokemon name type' (hp-power) (replaceMove (changeEN(pickBossMove moves z)) moves) tf)






{-|
Syntax:     pickBossMove [Move] Int
Purpose:    It return a move that the boss has used in the battle 

Example:    Assume the move1 which has EN as 1, after the method, the output will be 0.
            
Process:    It takes two parameters moves(the list of moves) z(the random number to pick boss' move).
            Since the boss has two moves, if the random number z is 1, then the first move will be picked.
            if the random number z is anything but 1, then the second move will be picked.
            Usually the first move is most powerful since the chance of being picked is lower than usual.
-}  
pickBossMove :: [Move] -> Int -> Move
pickBossMove moves z 
    | z == 1 = moves!!0
    | otherwise = moves!!1



{-|
Syntax:     changeEN Move 
Purpose:    It minus 1 from the last parameter of a Move

Example:    Assume changeEN (Move name hp 10) = (Move name hp 9)
            
Process:    It takes a parameter a move, and then minus 1 from the last parameter (en).
            
-} 
changeEN :: Move -> Move
changeEN (Move name hp en) = (Move name hp (en-1))



{-|
Syntax:     get1MoveName Move 
Purpose:    It return the name of the move.
Example:    Assume get1MoveName (Move "Hello" hp en) = Hello
Process:    It takes a parameter a move, and then return the first parameter of the data.
-} 
get1MoveName :: Move -> Name
get1MoveName (Move name hp en) = name


{-|
Syntax:     get1MovePower Move 
Purpose:    It return the Power of the move.
Example:    Assume get1MovePower (Move name 100 en) = 100       
Process:    It takes a parameter a move, and then return the second parameter of the data.      
-} 
get1MovePower :: Move -> Power
get1MovePower (Move name power en) = power



{-|
Syntax:     attackUser Pokemon
Purpose:    It return a updated pokemon data after the boss attacked.  It also, update the pokemon move EN value.
Example:    Assume the boss has attacked the user, and user got 10HP left
            The output will be: (Pokemon name type' 10 moves) 
Process:    It takes four parameters: (Pokemon name type' hp moves)(user's pokemon) UsedMove(the move that the pokemon has used in a round of battled) (Pokemon name' type'' hp' moves')(boss) 
            and z(the random number to pick the boss' move)
            First it passes the 'z' and 'moves''(The boss' move) to pickBossMove and get1MovePower to get the boss' move power, 
            then it minus this value from hp'users' pokemon HP'.  After that, it passes usedMove and moves to minusUserEN to minus the EN from the move that user just used.
            Then it passes this to replaceMove with moves to get a updated list of move. Next, it gather all these value to place them to the parameter of a pokemon data.
-} 
attackUser :: Pokemon ->Move-> Pokemon-> Int ->Pokemon
attackUser (Pokemon name type' hp moves tf) usedMove (Pokemon name' type'' hp' moves' tf') z = 
    (Pokemon name type' (hp - (get1MovePower (pickBossMove moves' z))) (replaceMove (changeEN usedMove) moves) tf)




{-|
Syntax:     replaceMove Move [Move] Move
Purpose:    It returns the updated list of move after a round of battle for both user and boss

Example:    Assume the boss has its EN reduced to 1 in its move1, 
             The output will be [(move1 EN =1)]
            
Process:    It takes two parameter: newMove(the move with the updated EN) and (x:xs)(the list of move)
             It passes x and newMove to get1MoveName to check if they have the same name.  If this is the case, newMove replace the first item of xs
             Otherwise, it will place x in front of the list and then call itself again with newMove and xs.
             The base case with a empty list does not exist, since we already make sure the move that chosen by the boss or users will be in the list of move.
             The attackMode method on Game.hs has already made sure the move is within the range of list.
-} 
replaceMove:: Move -> [Move] -> [Move]
replaceMove newMove (x:xs) = 
    if get1MoveName newMove == get1MoveName x then 
        newMove:xs 
    else x:replaceMove newMove xs




{-|
Syntax:     showBossMove Pokemon Int 
Purpose:    It returns a string showing what move that the boss has used.

Example:    Assume the boss has used "Tackle"
            The output will be: PIKACHU used TACKLE to attack.
            Your pokemon's HP got decreased by 20
            
Process:    It takes two parameter: (Pokemon name type' hp moves)(boss) z(the number to pick the boss move)
            It first passes moves and z to pickBossMove and get1MovePower to get the power of the move
            Then, it pass moves and z to get1MoveName to get the name of the move.

            Then it return the value within a string.
-} 
showBossMove:: Pokemon -> Int -> IO()
showBossMove (Pokemon name type' hp moves tf) z = do
    putStrLn ("\n"++name ++ " used " ++ get1MoveName (pickBossMove moves z)++" to attack.\n" ++ "Your pokemon's HP got decreased by " ++ show(get1MovePower (pickBossMove moves z)))


{-|
Syntax:     showUserMove Poke 
Purpose:    It returns a string showing what move that the user has used.

Example:    Assume the user has used "Tackle"
            You used TACKLE, and it is super effective!
            Your pokemon's HP got decreased by 20
            
Process:    It takes a parameter, it the name of the move and return it within a string

-} 
showUserMove:: Move -> IO()
showUserMove (Move name hp en) = do
    putStrLn ("\nYou used " ++ name ++ ", and it is super effective!\n")



{-|
Syntax:     useItemToPoke Pokemon Item
Purpose:    It returns a updated pokemon data after using an item to increase or decrease its HP.

Example:    Assume the user's pokemon's HP has used got increased by 10
            The output will be: useItemToPoke (Pokemon name type' 100 moves) item = (Pokemon name type' 110 moves)
            
Process:    It takes two parameters, (Pokemon name type' hp moves)(the pokemon) item (the item that user has chosen to use)
            It first passes the item to getItemHP to return the hp parameter.
            Then it adds to hp of the pokemon data.
            Then returns the updated pokemon data

-} 
useItemToPoke :: Pokemon -> Item -> Pokemon
useItemToPoke (Pokemon name type' hp moves tf) item = 
    (Pokemon name type' (hp+(getItemHP item)) moves tf)
    where getItemHP (Item name hp pos desc) = hp


{-|
Syntax:     removeItemFromUser User Item
Purpose:    It returns a updated item list of the user, after they used an item.

Example:    Assume the user has used item1 out of a list [item1, item2]
            The output will be a user data with a list of item: [item2]
            
Process:    It takes two parameters: (User name pos pokes items)(the user data) y(the used item)
            It first pass y and items(the current item list) to updateUserItems to get the list updated.
            In updateUserItems, (x:xs) represents the current item list, while y represent the item to be remove.
            It will passes these two values to get1ItemName to get the name.  Then check if they are the same.
                If this is the case, it will return xs(the list without the first item)
                otherwise, it will place the first item x on in front of the list and called the method again with xs(the list without the first item) and y.
                the base case with empty list dose not exist, since we already make sure the item chosen by the user is within the list.
                The method "pickItemMode" on game.hs has already made sure.
-} 
removeItemFromUser :: User -> Item -> User
removeItemFromUser (User name pos pokes items) y = (User name pos pokes (updateUserItems items y))
    where updateUserItems (x:xs) y = 
            if get1ItemName x == get1ItemName y then 
                xs 
            else
                x:updateUserItems xs y



{-|
Syntax:     attackUserInItemMode Pokemon Pokemon Int
Purpose:    It returns a updated pokemon data for user's pokemon after it got attacked by the boss while they are using the item.

Example:    Assume the user has HP 100 decreased to 50.
            The output will be: attackUserInItemMode (Pokemon name type' 100 moves) boss z =  (Pokemon name type' 50 moves)
            
Process:    It takes three parameters: (Pokemon name type' hp moves)(user's pokemon) (Pokemon name' type'' hp' moves')(boss) 
            and z(the number to pick the boss' move)
            It first passes moves'(the boss' move) and z to pickBossMove to get the move of the boss. then it passes this move to get1MovePower to get the actual power out of the move.
            Then it minus user's pokemon's hp from this power returned by get1MovePower method.
            Next, it replace the current parameter with this in the pokemon data.

-} 
attackUserInItemMode :: Pokemon -> Pokemon -> Int -> Pokemon
attackUserInItemMode (Pokemon name type' hp moves tf) (Pokemon name' type'' hp' moves' tf') z = 
    (Pokemon name type' (hp - (get1MovePower (pickBossMove moves' z))) moves tf)

{-|
Syntax:     minusBossENInItemMode Pokemon Int 
Purpose:    It returns a boss value with an updated move list with updated EN after the boss attacked the user in the item mode.

Example:    Assume the boss has EN decrease from 1 to 0 in move1, 
            The output will be: (Pokemon name type' hp [(move1 EN = 0), move2])
            
Process:    It takes two parameters: (Pokemon name type' hp moves)(boss) z(the number to pick the boss move)
            It passes moves and z to pickBossMove to get the move that the boss has used.  Then the changeEN method will minus 1 from that Move EN.
            It then passes the value to replaceMove to replace the current move with the updated move in the pokemon data.

-} 
minusBossENInItemMode :: Pokemon -> Int -> Pokemon
minusBossENInItemMode (Pokemon name type' hp moves tf) z = 
    (Pokemon name type' hp (replaceMove (changeEN (pickBossMove moves z)) moves) tf)


{-|
Syntax:     showItemDesc Item
Purpose:    It returns a string telling what is the effect of the item after they have used it.

Example:    Assume the user has used a sushi with a description of 'and Your pokemon is enjoying it! HP got increased by 20'
            the output will be : You just used SUSHI, and Your pokemon is enjoying it! HP got increased by 20
            
Process:    It takes a parameter (Item name hp pos desc)(the item user chose).  It return a string with the name and desc parameters.

-} 
showItemDesc :: Item -> IO()
showItemDesc (Item name hp pos desc) = do 
    putStrLn ("\nYou just used " ++ name ++ ", and " ++ desc ++"\n")



{-|
Syntax:     addPokeBack User Pokemon 
Purpose:    After user has won the battle with the boss, that pokemon in battle will be returned back to user's pokemon list.
            This method is to update the pokemon list of the user after the battle.

Example:    Assume the user has got the pokemon "hello" back to his pokemon list
            the output will be : addPokeBack (User name pos (xs) items) x = (User name pos ("hellow":xs) items) 
            
Process:    It takes two parameter (User name pos (xs) items)(user data) x(the pokemon finished the battle)
            It then place x in front of the list of xs(the user's pokemon list) by using ":" operator.

-} 
addPokeBack :: User -> Pokemon -> User
addPokeBack (User name pos (xs) items) x = (User name pos (x:xs) items) 




