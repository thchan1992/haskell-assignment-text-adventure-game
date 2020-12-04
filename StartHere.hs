import Game
import SupportFunctions (pause, empty)


-- start the game by typing pokemon
pokemon :: IO()
pokemon = do
    putStrLn "\n\nFOR EDUCATIONAL USE\n\n"
    pause
    empty

    putStrLn "\n\n POKEMON - UWL VERSION\n\n"
    pause
    empty

    putStrLn "\n\n\n1. INTRODUCTION TO THIS GAME:\n\n"
    pause
    empty

    putStrLn "This game is based on the famous cartoon 'Pokemon' from Japan."  
    putStrLn "'Pokemon' is like a monster that people will use it to battle in this cartoon.\n\n"
    pause
    empty

    putStrLn "In this game, you need to go through three different rooms to win this game.\nIn each room, you will have no idea where the exit is because it is too DARK!! you need to go explore every corner of the room!"
    putStrLn "Also, you might encounter some wlid pokemon that you could capture for battle later" 
    putStrLn "In order to get through each room, you will need to first get a pokemon from a bloke called 'Professor OAK' who can give you a pokemon.\n\n" 
    pause
    empty

    putStrLn "Then you can use this pokemon to unlock a room."
    putStrLn "However, you will have to battle a pokemon which is a boss of the room to escape."
    putStrLn "There might be some item in the room that users could find it. (It could be good or bad, we will tell you if they are good when you use it!)\n\n"
    pause
    empty

    putStrLn "\n\n\n2. POKEMON & ROOM:\n\n" 
    pause
    empty

    putStrLn "Each pokemon has its own type namely FIRE, GRASS, WATER"
    putStrLn "Room has the same type like those pokemon\n\n"
    pause
    empty

    putStrLn "FIRE"
    putStrLn "|   \\"
    putStrLn "|    \\"
    putStrLn "|     \\"
    putStrLn "GRASS--WATER\n\n"
    
    putStrLn "WATER type is effective to fight against FIRE type"
    putStrLn "GRASS type is effective to fight against WATER type"
    putStrLn "FIRE type is effective to fight against GRASS type"
    pause
    empty

    putStrLn "Therefore, when we choose a pokemon to escape a room of FIRE, we should choose WATER type pokemon"
    putStrLn "If you choose a right pokemon to open the door for you, this pokemon's HP will increase.\n\n"
    pause
    empty

    putStrLn "If you feel good to go, press any KEY to start the game!\n\n"

    option<- getLine
    empty

    putStrLn "                                  ,'\\"
    putStrLn "    _.----.        ____         ,'  _\\   ___    ___     ____"
    putStrLn "_,-'       `.     |    |  /`.   \\,-'    |   \\  /   |   |    \\  |`."
    putStrLn "\\      __    \\    '-.  | /   `.  ___    |    \\/    |   '-.   \\ |  |"
    putStrLn " \\.    \ \  |    | __  |  |/    ,','_  `.  |          | __  |    \\|  |"
    putStrLn "   \\    \\/   /,' _`.|      ,' / / / /   |          ,' _`.|     |  |"
    putStrLn "    \\     ,-'/  / \\ \\    ,'   | \\/ / ,`.|         /  /   \\  |     |"
    putStrLn "     \\    \\ |   \\_/  |   `-.  \\    `'  /|  |    ||   \\_/  | |\\    |"
    putStrLn "      \\    \ \         /       `-.`.___,-' |  |\\  /| \\      /  | |   |"
    putStrLn "       \\    \\ `.__,'|  |`-._    `|      |__| \\/ |  `.__,'|  | |   |"
    putStrLn "        \\_.-'       |__|    `-._ |              '-.|     '-.| |   |"
    putStrLn "                                `'                            '-._|"


    start 


        