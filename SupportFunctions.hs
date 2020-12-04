module SupportFunctions(
chooseCommand, 
getUserName,
wrongInput,
getRoomName,
empty,
get1ItemName,
get1PokeName,
getUserPos,
get1PokeHP,
moveChecker,
checkMapSize,
showRadar, 
updateUserPos,
atPickItem,
passItem, 
checkKey,
checkEventPosition,
pause,
movingInstruction,
gatherPosition,
get1ItemPos,
getRoomPos,
addItemPosition,
checkBoolList,
checkInput,
checkPokeLeft,
get1PokeBool,
checkItemPosWithAllPos,
gatherItemPosWithChar,
gatherPositionWithChar,
randomNum
) where

import DeclaredData
import Data.Char(toUpper, isDigit, isSpace)
import Data.Time.Clock.POSIX





--------

{-|
Syntax:     checkInput String 
Purpose:    This method is to check if a user's input is a number only when we are taking a number from the user to decide the room size and deleting a pokemon on the Game.hs
Example:    Assume there is a input "123", then the output will be True
Process:    This method take a parameter: input(user's input)
            This passes input to isDigit and isSpace to return a list of boolane by mapping each elements
            Then passes it to checkBoolList method to check whether all the element on the list are true or false.
            If the result of checkBoolList( map isDigit input) is true and the result of checkBoolList (map isSpace input) is false, then it will return true.
            And it would mean that the user input contain only number
-}
checkInput :: String -> Bool
checkInput input = 
    if checkBoolList (map isDigit input) == True && checkBoolList (map isSpace input) == False then True else False

{-|
Syntax:     checkBoolList [Bool]
Purpose:    This method returns the list of boolena to checkInput method.
Example:    Assume there is a input "123", then the output will be True
Process:    This method take a parameter: input(user's input)
            This passes input to isDigit and isSpace to return a list of boolane by mapping each elements
            Then passes it to checkBoolList method to check whether all the element on the list are true or false.
            If if the result of checkBoolList( map isDigit input) is true and the result of checkBoolList (map isSpace input) is false, then it will return true.
            And it would mean that the user input contain only number
-}
checkBoolList :: [Bool] -> Bool
checkBoolList [] = True
checkBoolList (x:xs) = if x == True then checkBoolList xs else False


{-|
Syntax:     gatherPositionWithChar Room [Item] 
Purpose:    Return the all the position including room and item in the list with a speical character, so it could be one of the parameter of showRadar method.  
            Then this could generate the speical position with different character on a special location on the map.
Example:    Assume there are three position (1,2), (2,1), (3,1) in the Room data, the output will be [((1,2),'O'), ((2,1), 'E'), ((3,1), 'P')]
Process:    This method takes two parameters which is (Room nam poke pos1 pos2 pos3 tf n)(The room) items(the list of item).  
            The program will put all three position from the room on the top of the list then, passes items to gatherItemPos to get all the positions of the item and  
            place it at the end. With the special character next to the position.  
            'O' is for the first position of the room. 'E' is for the second position of the room, 'P' is for the third position
-}
gatherPositionWithChar :: Room -> [Item] -> [(Position, Char)]
gatherPositionWithChar (Room nam poke poke' pos1 pos2 pos3 tf n) items = (pos1, 'O'):(pos2, 'E'):(pos3, 'P'):gatherItemPosWithChar items




{-|
Syntax:     gatherItemPosWithchar [Item] 
Purpose:    Return all the item position in the list to gatherPositionWithChar with the character 'I'
            
Example:    Assume there are three position (1,2), (2,1), (3,1), the output will be [((1,2) 'I'), ((2,1),'I'), ((3,1),'I')]

Process:    This method a parameter: (x:xs) the item list.  
            This is a recursive method, it will stop once (x:xs) becomes empty, then it will return the whole list.
            In each recursive call, it will put x(the first item on the list) with 'I' to get1ItemPos to get the position, and then place it in front of the list
            Then, call gatherItemPosWithChar method again with the rest of the list (xs)
-}
gatherItemPosWithChar :: [Item] -> [(Position, Char)]
gatherItemPosWithChar [] = []
gatherItemPosWithChar (x:xs) = ((get1ItemPos x), 'I'):gatherItemPosWithChar xs






{-|
Syntax:     showRadar GameMap User [Position]
            
Purpose:    Show the game map with the location of the user and all special location such as item and room.

Example:    Radar
            I ░ ░ ░ O 
            I ░ ░ ░ ░ 
            ░ ░ ░ P █ 
            E ░ ░ ░ ░ 
            ░ ░ ░ ░ ░ 

Process:    It takes three parameters GameMap(the map), User(user data for the user position), xs'(the list of positions of all object in this game with the special character)
            
            It first pass xs' to convert the position that could be displayed by the map in xs (see more detail on convertPos method)
            
            Then, check whether the last parameter of the map(tf)is true, if it is true, it will not show any speical location.
            
            If this is false, it will show the special location as according to its special character.

            There are 9 main steps to generate this map:

            1. In this if statement, it first passes (x,y)(the user's position) to the front of the list in 'xs'(the position of all object), 

            2. then reverse the order, so the user position will be the last object in the list now. 
               Because we want to make the user's indicator █ is the last thing to be placed on the map row (see more detail on getAllRows method)

            3. and then use pass string "░ "(the block of the radar) xup(the width of the map) to mulitpleElement to replicate this "░ " according to the size "xup"

            4. After that, we should have a string, then we place it in a list, so now we have [(mulitpleElement "░ " xup)].

            5. We pass this [(mulitpleElement "░ " xup)] with yup (the height of the map) to replicate the map row in a list.

            6. We then passes this list of map rows to getAllRows with the speical Character('░' or the special charaacter, depends on the last arguement. i.e. 1 or 2) to place this element on the map depends on the list of positions: (reverse(((((x)*2),y))
            
            7. After that, we should have a list of map rows with all the speical character on it.

            8. However, we will have to reverse, otherwise, if we want to show (0,0) position on the map, the position will be on the top row even x of grid being 0.
            
            9. After reversing these rows, we will use unlines to seperate all these rows and print it on seperate lines.

-}
showRadar :: GameMap -> User -> [(Position, Char)] -> IO()
showRadar (GameMap xup xbot yup ybot tf) (User name (x, y) pokes items) xs' = do 
    let xs = convertPos xs'
    if tf == True then do
            putStrLn ("\nRadar\n" ++ unlines (reverse(getAllRows (mulitpleElement [(mulitpleElement "░ " xup)] yup) (reverse((((x*2),y), '█'):xs)) 2)))
    else do
            putStrLn ("\nRadar\n" ++ unlines (reverse(getAllRows (mulitpleElement [(mulitpleElement "░ " xup)] yup) (reverse((((x*2),y), '█'):xs)) 1)))






{-|
Syntax:     convertPos [Position] [Position]
            
Purpose:    This method is to convert the position from room, item and user into the position that could be reflected on the map

            Since on the map, the map will be displayed as followed (5X5 size):
            ░ ░ ░ ░ ░ 
            ░ ░ ░ ░ ░ 
            ░ ░ ░ ░ ░ 
            ░ ░ ░ ░ ░ 
            █ ░ ░ ░ ░ 
            the user is now in the position of (0,0)

            As you can see there is a space between each block.  If we simply use the position striaght from the object, the map will be as like this as the user move:
            ░ ░ ░ ░ ░ 
            ░ ░ ░ ░ ░ 
            ░ ░ ░ ░ ░ 
            ░ ░█░ ░ ░ 
            ░ ░ ░ ░ ░ 
            the user is now in the position of (3,1).

            [[Therefore, we need to make sure the x is always 2 times bigger than itself]]

           This convertPos method is to convert the position into a position that could be clearly shown on the map like the following:
            ░ ░ ░ ░ ░ 
            ░ ░ ░ ░ ░ 
            ░ ░ ░ ░ ░ 
            ░ ░ ░ ░ ░ 
            ░ ░ █ ░ ░   
            the user is now in the position of (2,0)
            the map will have to take the position as (4, 0) to show it
            this method is to convert (2,0) to (4,0)

            In this this method, it will returns all the list of the position of object based on this rule.


Example:    assume 
            convertPos [((2,1),'I'),((4,1),'I')]
            the output: [((2,0),'I'),((4,0),'I')]

Process:    The method takes a parameter (((x,y), c):xs). ((x,y), c) represents the first element of the list, xs represent the rest of the position
            This is the recursive method, it will stop calling itself and return the list of position once (((x,y),c):xs) becomes empty
            In each recursive call,
            It will passes x and y to do (x*2 and pass it to getxNum to get obtain the x value, and y), 
            then place this position in the front of the list.
            Then, call the same method with the rest of list (xs)
-}
convertPos ::[(Position,Char)] -> [(Position,Char)]
convertPos [] = []
convertPos (((x,y), c) :xs) = ((((x)*2),y), c):convertPos xs




{-|
Syntax:     mulitpleElement [a] Int 
            The reason why we need to specify type [a] is because the function will be used to either concat and replicate a single string or more than 2 lists of string.
            Therefore, we cannot have a type specified neither for [String] or String.  Also, the output could also be either a single string or a list containing muliple string. 

Purpose:    This method replicate a string depends on the value of n.  And then it will concatenate it and return to showRadar function.
            This is to mulitple the block "░ " on the map and also mulitple the rows of the map.

Example:    If the input is a single string such as "ABC"
            Output:
            mulitpleElement "ABC" 3
            "ABCABCABC"

            If the input is a list of strings 
            Output:
            mulitpleElement ["ABC","ABC"] 3
            ["ABC","ABC","ABC","ABC","ABC","ABC"]

Process:    This method take two parameters: string(the string we need to mulitple), n is the times we are replicating.
            Pass it to replicate method to replicate the times we need and then concat them.
-}
mulitpleElement :: [a] -> Int -> [a]
mulitpleElement string n =  concat (replicate n string)



{-|
Syntax:     getAllRows [[Char]] [(Position, Char)] Int

Purpose:    This will returns a list of all row on the map with all the speical location and user's position on the map. Then it will returns this list to showRadar method.

Example:    Assume getAllRows ["AAAA", "BBBB", "CCCC"] [((0,0),'I'),((1,1)'I'),((2,2),'I')] 
            The output will be ["IAAA","BIBB","CCIC"]

Process:    This method take three parameters: allMapRow(The default map row without any special location),
            (((x,y),c):xs) (the list of all position of speical location including the item, user room position, with its special character), 
            1 or 2 (it is for the pattern matching, if 1 is for showing special character on map while 2 is not)
            This is a recursive method, it will stop calling itself once the list of position become empty (((x,y),c):xs), and it will return allMapRow value.

            It will first use !! operator to y(the second element of a position which represents the height of the location) and allMapRow to find the specified row in the list we want to place a
            specialChar(c) on.  Then passes this row(allMapRow!!y), c(speical char) and x(the first element of a psotion which represent the width of the location) to the INNER getRow method to get the updated row.

            Then, passes this updated row with y and allMapRow(the list of the row that has not been updated yet) to the OUTTER getRow method to get the allMapRow updated with the new row.
            Next, it calls the function again with 1 or 2 and xs(the rest of postions) to do the same thing.
            
            
-}
getAllRows :: [[Char]] -> [(Position, Char)] -> Int -> [[Char]]
getAllRows allMapRow [] n = allMapRow
getAllRows allMapRow (((x,y),c):xs) 1 = getAllRows (getRow allMapRow (getRow (allMapRow!!y) c x)  y) xs 1
--If user did not turn on the tourch.  this pattern will be called before the base case. In here, we want to make sure the users' block is shown on top of every block, therefore before it reaches to the base case, it will change the speical character to users' block.
getAllRows allMapRow [((x,y),c)] 2 = getAllRows (getRow allMapRow (getRow (allMapRow!!y) '█' x)  y) [] 2
getAllRows allMapRow (((x,y),c):xs) 2 = getAllRows (getRow allMapRow (getRow (allMapRow!!y) '░' x)  y) xs 2


{-|
Syntax:     getRow [a] a Int.  a could be a char or string, depends on the input of getAllRows method.  If it is a string, we are replacing a whole row in a map, 
            if it is a char, we are replacing one element in the row of the map.  Therefore, the data type has to be a instead of either string or char
Purpose:    This method sets and returns a row value of a map.  It returns this row to getAllRows method.
Example:    Assume getRow "ABC" 'X' 0 
            the output will be "XBC"
Process:    This method take three parameters: xs(one row of the map) y(the element we are placing in one of the value in this row), n(the number position of the element in the roll we are placing)
            This is recursive method, it will stop once n becomes 0.
            It first place x(the first elements of the row) and then call itself again with xs(the rest of the row) together with y and n-1.
            Once it reached the base case, it will place y in front of the list and then take off the first elements of the xs.
            In this case, y will be replacing the "nth" item on the row.
-}
getRow :: [a] -> a -> Int -> [a]
getRow xs y 0 = y:(tail xs)
getRow (x:xs) y n = x:getRow xs y (n-1)



{-|
Syntax:     This is called by more than one methods on the Game.hs
Purpose:    This method is generate the seed for random number by using getPOSIXTime
Example:    Assume we have a number from getPOSIXTime 11232321321.123213s, the output will be '31'
Process:    It will first a number from getPOSIXTime and declare it as x.
            Then, convert x to string by using show, then reverse it.
            Then take the first 3 string , at this time, we should have 's' with 2 digit.
            we are going to tail the 's' out of it
            then we use read function convert this 2 digit back to a Int
-}
randomNum :: IO Int
randomNum = do
    x <- getPOSIXTime
    let y =  (tail (take 3 (reverse(show x))))
    let num = read (y) :: Int
    return num

{-|
Syntax:     checkItemPosWithAllPos [Position] Position
Purpose:    This method returns a Boolean value to indicate if a new item generated by createItemPosition on Games.hs has the same position with a list generated by gatherPosition

Example:    Assume the new item position is (1,1) and there is (1,1) in a list of [(1,1),(2,2)(3,3)], the output will be False

Process:    This method take two parameters: (x:xs)(the list of all position of room and items that just gathered by gatherPosition method) and y (the new item)
            It first passes x(the first item of the position list) and y to check if they are the same, if they are the same, it will return False value and terminate the method
            otherwise it will call the method again with only xs and y.

            It will stop calling itself once the xs becomes empty
-}
checkItemPosWithAllPos :: [Position] -> Position -> Bool
checkItemPosWithAllPos [] y = True
checkItemPosWithAllPos (x:xs) y = 
    if  x == y then False else checkItemPosWithAllPos xs y

{-|
Syntax:     addItemPosition Item Position
Purpose:    this method will put the new position created by createItemPosition method on Game.hs and return a new item data
            
Example:    Assume addItemPosition (Item name hp pos desc) (1,1) =  (Item name hp (1, 1) desc)

Process:    This will put the newPos into the item current position.
-}
addItemPosition:: Item -> Position -> Item
addItemPosition (Item name hp pos desc) newPos =  (Item name hp newPos desc)


{-|
Syntax:     gatherPosition Room [Item] 
Purpose:    Return all the position in the list, so in createItemPosition on Game.hs, we can avoid making the new item with the same position with the room and other items
            according to this list.
Example:    Assume there are three position (1,2), (2,1), (3,1), the output will be [(1,2), (2,1), (3,1)]
Process:    This method takes two parameters which is (Room nam poke pos1 pos2 pos3 tf n)(The room) items(the list of item).  
            The program will put all three position from the room on the top of the list then, passes items to gatherItemPos to get all the positions of the item and  
            place it at the end.
-}
gatherPosition :: Room -> [Item] -> [Position]
gatherPosition (Room nam poke poke' pos1 pos2 pos3 tf n) items = pos1:pos2:pos3:gatherItemPos items


{-|
Syntax:     gatherPosition Room [Item] 
Purpose:    Return the all the item position in the list to gatherPosition
Example:    Assume there are three position (1,2), (2,1), (3,1), the output will be [(1,2), (2,1), (3,1)]
Process:    This method a parameter: (x:xs) the item list.  
            This is a recursive method, it will stop once (x:xs) becomes empty, then it will return the whole list.
            In each recursive call, it will put x(the first item on the list) to get1ItemPos to get the position, and then place it in front of the list
            Then, call gatherItemPos method again with the rest of the list (xs)
-}
gatherItemPos :: [Item] -> [Position]
gatherItemPos [] = []
gatherItemPos (x:xs) = (get1ItemPos x):gatherItemPos xs




{-|
Syntax:     get1PokeBool Pokemon
Purpose:    Return the pokemon last parameter which is a Bool.  If this is true, then the pokemon is a wild pokemon
Example:    Assume get1PokeBool (Pokemon name type' hp moves True) = True
Process:    This method a parameter of a pokemon data and return the last parameter
-}
get1PokeBool :: Pokemon -> Bool
get1PokeBool (Pokemon name type' hp moves tf) = tf





{-|
Syntax:     movingInstruction [String]
Purpose:    Return an instruction showing how to move in the game  
Example:    

                type: W to go up.

                type: S to go down.

                type: A to go left.

                type: D to go right.


                type: POCKET to view your pocket.
Process:    This method takes a parameter: a list of commands and then place them in a string.
-}
movingInstruction commands = do
    putStrLn ("\n INSTRUCTION:\n\n")
    putStrLn ("\n type: " ++ chooseCommand commands 0 ++ " to go up.")
    putStrLn ("\n type: " ++ chooseCommand commands 1 ++ " to go down.")
    putStrLn ("\n type: " ++ chooseCommand commands 2 ++ " to go left.")
    putStrLn ("\n type: " ++ chooseCommand commands 3 ++ " to go right.\n")
    putStrLn ("\n type: " ++ chooseCommand commands 9 ++ " to view your pocket.\n")


{-|
Syntax:     It is called by different methods from Game.hs
Purpose:    Pause the game so the user can read text.
Process:    It show the string to tell user to press enter to carry on, p<-getLine is to waiting for any input from the user.
            In this case the game will not carry on until user enter anything.
-}
pause:: IO()
pause = do 
    putStrLn "Press ENTER to continue"
    p <-getLine
    putStrLn " "

{-|
Syntax:     chooseCommand [String] Int
Purpose:    Return the command in the list of commands 'commands' by using their index.  
Example:    Assume the first command in the list 'xs' is "Hello".  chooseCommand xs 0 = "Hello"
Process:    This method takes two parameters which is 'xs' and 'n'.  The program checks if will tail the first element in the list and then minus one from 'n'.  
            Once 'n' becomes 0, it will take the first item of the list.  Therefore, 'n' is the number position.
-}
chooseCommand :: [String] -> Int -> String
chooseCommand xs 0 = head xs
chooseCommand xs n = chooseCommand (tail xs) (n-1)


{-|
Syntax:     It is called by different methods from Game.hs
Purpose:    Return a string to let users know that we do not recognise their input.
Example:    [[WRONG INPUT, please try again.]]
Process:    Use putStrLn method to show the string.
-}
wrongInput :: IO()
wrongInput = do putStrLn "[[WRONG INPUT, please try again.]]"


{-|
Syntax:     It is called by different methods from Game.hs
Purpose:    Return a string to let users know that we do not recognise their input.
Example:    N/A
Process:    Use putStrLn method to show the string.
-}
empty :: IO()
empty = do putStr "\ESC[2J"


{-|
Syntax:     moveChecker GameMap User String [String]
Purpose:    It generated the new position according to the input from user.  Then it checks if user's move is out of the map range. 
            If this is the case, it will return false value.
Example:    Assume the size of the Map is x = 3, y = 3. If user's move position is (3, 3), then it will return True value.
Process:    It takes four parameters: gameMap(The map), user(User data), option(User' input to move), commands(a list of all commands).  
            It first passes the user, option and commands to 'userNewPosition' to get the new position after the move.
            Then it passes this value with gameMap to the 'checkMapsize'.  This function is to actually compare the map size with the new position of the user.
            If it is out of the range, it will return False value.
-}
moveChecker:: GameMap-> User -> String -> [String] -> Bool
moveChecker gameMap user option commands = if checkMapSize gameMap (userNewPosition user option commands) then True else False


{-|
Syntax:     checkMapSize GameMap Position
Purpose:    It gets the new position of the users and checks if user's move is out of the map range. 
            If this is the case, it will return false value to "moveChecker"
Example:    Assume the size of the Map is x = 3, y = 3. If user's move position is (3, 3), then it will return True value.
            checkMapSize (GameMap 3 1 3 1 s0) (3,3) = if (3 == 3 && 3 == 3) <--- It returns true value.

Process:    It takes two parameters: (GameMap xup xbot yup ybot s0)(the map) and (x, y)(the user new position).  
            'xup' is the height of the map 'xbot' is the bottom of the map. 
            'yup' and 'ybot' is the width of the map.
            Then, it checks if x is within the range of xup-1 and xbot and y is within the range of yup-1 and ybot.  If it is the case, it returns the True value.
            The reason why we need to minus 1 from xup and yup is that because the position includes zero.  
            Without this, when we print the radar, the radar position will also be 1 ahead of the original position.
-}
checkMapSize :: GameMap -> Position -> Bool
checkMapSize (GameMap xup xbot yup ybot tf) (x, y) = if (x<=xup-1 && x>=xbot) && (y<=yup-1 && y>=ybot) then True else False


{-|
Syntax:     userNewPosition User String [String]
Purpose:    It returns a new position of user according to the user input to "moveChecker"
Example:    Assume, the user input is 'W'.  'W' is assocated with the 'direction UP' method. 
            Assume the user old position is (1,1)
            Assume the first commands of the list is "W"
            
            userNewPosition users "W" commands  
                    | "W" == "W =  direction UP (1 , 1)

            We will have the output as (1, 2)

Process:    This function is defined by guarded equations.
            It takes three parameters: user(User data), option(User input), commands (a list of commands).
            It first passes the commands to chooseCommand to get the related commands.  Then it compares if the 'option' is the same as this command.
            Then, it will pass 'user' to the 'getUserPos' to get the current position of the user
            Next it will pass it to 'direction' to get the actual updated position.  Different 'option' will lead to different parameter of UP, DOWN, LEFT, RIGHT.
            In 'otherwise' operator, there will not be used since we already have a checker to make sure user's input is within the command list.
-}
userNewPosition:: User -> String -> [String] -> Position
userNewPosition user option commands
        | option == (chooseCommand commands 0) =  getDirection UP (getUserPos user)
        | option == (chooseCommand commands 1) =  getDirection DOWN (getUserPos user)
        | option == (chooseCommand commands 2) =  getDirection LEFT (getUserPos user)
        | option == (chooseCommand commands 3) =  getDirection RIGHT (getUserPos user)
        | otherwise = error "Wrong Input"


{-|
Syntax:     getDirection Direction Position
Purpose:    It returns a updated position of the user to "userNewPosition"
Example:    Assume, the user's position is now (1,2) and the 'Direction' is now 'UP'.
            getDirection UP (1,2) = (1, 3)
            We will have the output as (1, 2)
Process:    This function was defined by pattern matching.  EG. If the method receive 'UP' data and position.  Then (x, y+1) will be called.
-}
getDirection :: Direction -> Position -> Position
getDirection UP (x, y) = (x, y+1)
getDirection DOWN (x, y) = (x, y-1)
getDirection LEFT (x, y) = (x-1, y)
getDirection RIGHT (x, y) = (x+1, y)




{-|
Syntax:     updateUserPos User String [String] 
Purpose:    It returns a new user data in order to update user's position after moving on the radar.
Example:    Assume, the user's position is now (1,2) and the 'option' (user's input) is 'UP'.
            The output will be updated to (1,3).  Then the output will be a user data with the updated parameter of position (1,3).

Process:    This function takes three parameters: (User name pos pokes items)(User data), option(user input), commands(list of commands).
            It first passes user data to "userNewPosition" to get the updated position.  Then, it replaced the old position by placing on the same place of the parameter.
-}
updateUserPos :: User -> String -> [String] -> User
updateUserPos (User name pos pokes items) option commands = (User name (userNewPosition (User name pos pokes items) option commands) pokes items)



{-|
Syntax:     atPickItem User [Item]
Purpose:    It checks whether user is in the position where they encounter any item on the map.
            If this is the case, it will return a True value.

Example:    Assume we have a Item whose third parameter is (1,1), and User current position is (1,1) as well
            The output will be: True

Process:    This function takes two parameters: user(User data) (x:xs)(list of item on the map).
            It will first passes x(the first element on the list) to "get1ItemPos" to get its position.
            Also, it passes user to "getUserPos" to get the user position.
            Then, it compares both of them.  If it is true, then it returns the True Value.
            otherwise, it will call itself again with user and xs(the rest of the list except from the first element)
            it will stop once (x:xs) becomes empty
-}
atPickItem :: User -> [Item] -> Bool
atPickItem user [] = False
atPickItem user (x:xs) = if (get1ItemPos x == getUserPos user) then True else atPickItem user xs


{-|
Syntax:     passItem User [Item]
Purpose:    It returns the item that the user has encountered.  Then it passes it to the method "pickItemMenu" on Game.hs
            

Example:    Assume we have list of item [item1, item2, item3]. If item1 is the item that users have encounted, the output will be item1.

Process:    This recursive function is to find out that item on the list by going throught the whole list.
            it takes two parameters of user(user data) and (x:xs) (the list of item on the map)
            It then passes x(the first item on the list) to get1ItemPos to get the position of the item on the map and passes user to getUserPos to get the position
            of the user. then it checks whether they are the same.  If this is the case it will return 'x'.
            Otherwise, it will call itself with xs(the rest of the list escape from the first element).
            It will stop until (x:xs) only got one item left on the list.  It will then return such item.
-}
passItem :: User -> [Item] -> Item
passItem user [x] = x
passItem user (x:xs) = if (get1ItemPos x == getUserPos user) then x else passItem user xs


{-|
Syntax:     get1ItemPos Item
Purpose:    It returns the postion of the item to "atPickItem" or passItem
Example:    get1ItemPos (Item name hp (1,1) desc) = (1,1) 
Process:    This function takes Item as a parameter and returns the third parameter 'pos'.
-}
get1ItemPos :: Item -> Position
get1ItemPos (Item name hp pos desc) = pos




{-|
Syntax:     checkEventPosition User Room Int
Purpose:    It is to check whether a user is either in "pokemon centre"(where user can heal all the pokemon), "professor OAK"(a place where user can pick a pokemon),
            and "exit of the room". 
Example:    Assume n = 1, so we are checking if the user is in the exit of the room. 
            Assume the third parameter is (1,1) which is the exit position
            Assume the user position is (1,1)
            Then the value will be True.
Process:    it takes three parameters: User (user data), room(the current room), n(the option which position we are checking). 
            if n = 1, we are checking "professor OAK" position
            if n = 2, we are checking "Exit of the room" position
            if n = 3, we are checking "pokemon Centre" position

            It first passes user to getUserPos to get the position of the user.  Then it passes room to getRoomPos with 'n' to get the position.
            Then, it checks whether they have the same values.  Then it returns True value if they are the same.
-}
checkEventPosition user room n = 
    if getUserPos user == getRoomPos room n then True else False


{-|
Syntax:     getRoomPos Room Int
Purpose:    It returns different position of the different event on the map to checkEventPosition according to n
            if n = 1, we are checking "professor OAK" position
            if n = 2, we are checking "Exit of the room" position
            if n = 3, we are checking "pokemon Centre" position
Example:    Assume n = 1, so we are checking if the user is in the exit of the room. 
            (Room name poke (1,1) pos2 pos3 tf n), the output will be (1,1)
Process:    it takes two parameters: (Room name poke pos1 pos2 pos3 tf n)(the current room), n(the option which position we are checking). 
            if n = 1, we are checking pos1
            if n = 2, we are checking pos2
            if n = 3, we are checking pos3
-}
getRoomPos (Room name poke poke' pos1 pos2 pos3 tf n) 1 = pos1
getRoomPos (Room name poke poke' pos1 pos2 pos3 tf n) 2 = pos2
getRoomPos (Room name poke poke' pos1 pos2 pos3 tf n) 3 = pos3



{-|
Syntax:     checkKey Room
Purpose:    It returns the sixth parameter of a Room.  This is a Boolean value.  If it is true, it means that user has got the pokemon and they can unlock the room to escape. 
Example:    Assume checkKey(Room name poke pos1 pos2 pos3 True n), the output will be True.
Process:    It takes Room as the parameter and returns the sixth parameter.
-}
checkKey :: Room -> Bool
checkKey (Room name poke poke' pos1 pos2 pos3 tf n) = tf





{-|
Syntax:     getUserName User
Purpose:    Return the name of the user
Example:    getUserName (User "Hello world" pos pokes items) = "Hello world"
Process:    Return the first parameter of 'User' which is Name.
-}
getUserName :: User -> Name
getUserName (User name pos pokes items) = name


{-|
Syntax:     getUserPos User
Purpose:    Return the position of the user
Example:    getUserName (User name (1,2) pokes items) = (1,2)
Process:    Return the second parameter of 'User' which is the position.
-}
getUserPos :: User -> Position
getUserPos (User name pos pokes moves) = pos

{-|
Syntax:     getRoomName Room
Purpose:    Return the name of a room
Example:    getUserName (Room "Hello world" poke pos1 pos2 pos3 tf s) = "Hello world"
Process:    Return the first parameter of 'Room' which is Name.
-}
getRoomName :: Room -> Name
getRoomName (Room name poke poke' pos1 pos2 pos3 tf s) = name

{-|
Syntax:     get1PokeName Pokemon
Purpose:    Return the name of a pokemon
Example:    getUserName (Pokemon "Hello world" type' hp moves) = "Hello world"
Process:    Return the first parameter of 'Pokemon' which is Name.
-}
get1PokeName :: Pokemon -> Name
get1PokeName (Pokemon name type' hp moves tf) = name

{-|
Syntax:     get1PokeName Pokemon
Purpose:    Return the name of a pokemon
Example:    getUserName (Pokemon "Hello world" type' hp moves) = "Hello world"
Process:    Return the first parameter of 'Pokemon' which is Name.
-}
get1PokeHP :: Pokemon -> HP
get1PokeHP (Pokemon name type' hp moves tf) = hp

{-|
Syntax:     get1ItemName Item
Purpose:    Return the name of an Item
Example:    getUserName (Item "Hello world" hp pos desc) = "Hello world"
Process:    Return the first parameter of 'Item' which is Name.
-}
get1ItemName :: Item -> Name
get1ItemName (Item name hp pos desc) = name




{-|
Syntax:     checkPokeLeft User
Purpose:    This returns the lenght of the pokemon list of the users
Example:    Assume the user has got a list of pokemon [poke1, poke2], the output will be 2.     
Process:    It takes a parameter: (User name pos pokes items)(user data).  It then use length method to check the pokes size(the pokemon list).
-} 
checkPokeLeft :: User -> Int
checkPokeLeft (User name pos pokes items) = length pokes