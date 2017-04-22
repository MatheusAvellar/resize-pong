open System

let sleep(time:int) = Threading.Thread.Sleep(time);

let div = "              -------------------------------------"
let board_width = 34
let board_height = 9
let std_x_pos = 16
let std_y_pos = 3
let limit_bottom = 10
let limit_top = 15
let std_board = Array2D.init<int> board_height (board_width+1) (
                    fun row col ->
                        if (row = std_y_pos && col = board_width
                        || row = std_y_pos+1 && col = board_width
                        || row = std_y_pos+2 && col = board_width) then
                            2
                        else if (row = std_y_pos && col = 0
                        || row = std_y_pos+1 && col = 0
                        || row = std_y_pos+2 && col = 0) then
                            1
                        else if (row = 3 && col = 16) then
                            3
                        else 0
                )

let colors =
    dict[
        "black", ConsoleColor.Black;
        "blue", ConsoleColor.Blue;
        "cyan", ConsoleColor.Cyan;
        "darkcyan", ConsoleColor.DarkCyan;
        "darkgray", ConsoleColor.DarkGray;
        "darkgreen", ConsoleColor.DarkGreen;
        "darkred", ConsoleColor.DarkRed;
        "darkmagenta", ConsoleColor.DarkMagenta;
        "darkyellow", ConsoleColor.DarkYellow;
        "gray", ConsoleColor.Gray;
        "white", ConsoleColor.White;
        "yellow", ConsoleColor.Yellow;
    ]

let printc(str:string, color:string) =
    Console.ForegroundColor <- colors.Item(color)
    printf "%s" str
    Console.ForegroundColor <- ConsoleColor.Gray

let printb(str:string, color:string) =
    Console.ForegroundColor <- ConsoleColor.White
    Console.BackgroundColor <- colors.Item(color)
    printf "%s" str
    Console.ForegroundColor <- ConsoleColor.Gray
    Console.BackgroundColor <- ConsoleColor.Black

let print_header(str:string) =
    if (str = "title") then
        printc("\n" + div + "\n                         RESIZE PONG\n" + div + "\n", "darkcyan")
    else if (str = "game") then
        printf "\n                    "
        if (Console.WindowHeight - 15 > limit_top) then
            printf "   -   |   10   |"
            printc("██", "darkred")
            printf " %i " (Console.WindowHeight - 15);
            printc("██", "darkred")
            printc("\n\n                              \\\\//\n\n", "darkred")
        else if (Console.WindowHeight - 15 < limit_bottom && Console.WindowHeight - 15 >= 0) then
            printc("██", "darkgreen")
            printf " %i " (Console.WindowHeight - 15);
            printc("██", "darkgreen")
            printf "|   10   |   -"
            printc("\n\n                              //\\\\\n\n", "darkgreen")
        else if (Console.WindowHeight - 15 > limit_bottom-1 && Console.WindowHeight - 15 < limit_top+1) then
            printf "   -   |"
            printc("██", "darkyellow")
            printf " %i " (Console.WindowHeight - 15);
            printc("██", "darkyellow")
            printf "|   -"
            printc("\n\n                              <<>>\n\n", "darkyellow")
        else
            printc("██ - ██|██ -- ██|██ - ██", "darkred");
            printf "\n\n                    "

let print_board(arr:int[,]) =
    let space = "              |"
    for i = 0 to (board_height-1) do
        for j = 0 to board_width do
            if j = 0 then printf "%s" space
            let c = (arr.[i,j])
            if c = 1 then
                printc("█", "darkgray")
            else if c = 2 then
                printc("█", "gray")
            else if c = 3 then
                printc("▄", "white")
            else
                printf " "
            if j = board_width then printf "|\n"

let update_board(board:int[,], pos_player:int, pos_comp:int, pos_ball:int[]) =
    Array2D.init<int> board_height (board_width+1) (
        fun row col ->
            if (row = pos_player && col = board_width
            || row = pos_player+1 && col = board_width
            || row = pos_player+2 && col = board_width) then
                2
            else if (row = pos_comp && col = 0
            || row = pos_comp+1 && col = 0
            || row = pos_comp+2 && col = 0) then
                1
            else if (row = pos_ball.[1] && col = pos_ball.[0]) then
                3
            else 0
    )

let rec loop(state:string, height:int, board:int[,], pos:int[], dir:int[], points:int[]) =
    sleep(100)
    try
        if Console.WindowHeight > 5 then
            Console.Clear()
            Console.BufferHeight <- Console.WindowHeight+5
    with
        | _ -> ()

    if state = "title" then
        print_header(state)
        printf "\n  RESIZE PONG works by "
        printc("vertically", "darkyellow")
        printf " resizing the window.\n\n"
        printc("  Window ", "darkcyan")
        printc("height", "cyan")
        printc(" < ", "darkcyan")
        printc("5  ", "cyan")
        printf "-  Move paddle down\n"
        printc("  Window ", "darkcyan")
        printc("height", "cyan")
        printc(" > ", "darkcyan")
        printc("5  ", "cyan")
        printf "-  Move paddle up\n\n\n\n  Resize to 5 to start: "
        if (Console.WindowHeight - 15 >= 0) then printf "%i/5" (Console.WindowHeight - 15);
        else printc("0/5", "darkred");
        if (Console.WindowHeight - 15 <> 5) then
            loop(state, Console.WindowHeight, std_board, pos, dir, points)
        else
            loop("game", Console.WindowHeight, std_board, pos, dir, points)
    else if(state = "game") then
        print_header(state)
        printf "%s\n" div
        print_board(board)
        printf "%s" div

        let new_dir_ball_x = (if (pos.[2] + dir.[0] > board_width-1 || pos.[2] + dir.[0] < 1) then -1 * dir.[0] else dir.[0])
        let new_pos_ball_x = (
            if ((pos.[2] + dir.[0] = 0
            && not (pos.[3] + dir.[1] = pos.[1] || pos.[3] + dir.[1] = pos.[1]+1 || pos.[3] + dir.[1] = pos.[1]+2))
            || (pos.[2] + dir.[0] = board_width
            && not (pos.[3] + dir.[1] = pos.[0] || pos.[3] + dir.[1] = pos.[0]+1 || pos.[3] + dir.[1] = pos.[0]+2))) then
                std_x_pos
            else
                if (pos.[2] + dir.[0] > board_width-1) then board_width-1 else if (pos.[2] + dir.[0] < 1) then 1 else pos.[2] + dir.[0])

        let new_dir_ball_y = (if (pos.[3] + dir.[1] < 0 || pos.[3] + dir.[1] > board_height-1) then -1 * dir.[1] else dir.[1])
        let new_pos_ball_y =  (
            if ((pos.[2] + dir.[0] = 0
            && not (pos.[3] = pos.[1] || pos.[3] = pos.[1]+1 || pos.[3] = pos.[1]+2))
            || (pos.[2] + dir.[0] = board_width
            && not (pos.[3] = pos.[0] || pos.[3] = pos.[0]+1 || pos.[3] = pos.[0]+2))) then
                std_y_pos
            else
                if (pos.[3] + dir.[1] < 0) then 0 else if (pos.[3] + dir.[1] > board_height-1) then board_height-1 else pos.[3] + dir.[1])

        let points_player = (
            if (pos.[2] + dir.[0] = 0
            && not (pos.[3] = pos.[1] || pos.[3] = pos.[1]+1 || pos.[3] = pos.[1]+2)) then
                points.[0]+1
            else
                points.[0])

        let points_comp = (
            if (pos.[2] + dir.[0] = board_width
            && not (pos.[3] = pos.[0] || pos.[3] = pos.[0]+1 || pos.[3] = pos.[0]+2)) then
                points.[1]+1
            else
                points.[1])

        printf "\n              "
        printc("CPU: ", "darkgray")
        printf "%i                 " points_comp
        printc("YOU: ", "gray")
        printf "%i" points_player

        let new_pos_comp = 3 // TODO: Computer position changing and such
        let new_pos_player = (if (Console.WindowHeight - 15 > limit_top && pos.[0] < board_height-3) then pos.[0]+1 else if (Console.WindowHeight - 15 < limit_bottom && pos.[0] > 0) then pos.[0]-1 else pos.[0])
        loop(
            "game",                // state:string
            Console.WindowHeight,  // height:int
            update_board(          // board:int[,]
                board,                 // board:int[,]
                new_pos_player,        // player_pos:int
                new_pos_comp,          // comp_pos:int
                [|                     // pos_ball:int[]
                    new_pos_ball_x;
                    new_pos_ball_y
                |]
            ),
            [|                     // pos:int[]
                new_pos_player;
                new_pos_comp;
                new_pos_ball_x;
                new_pos_ball_y
            |],
            [|                      // dir:int[]
                new_dir_ball_x;
                new_dir_ball_y
            |],
            [|                      // points:int[]
                points_player;
                points_comp
            |]
        )

[<EntryPoint>]
let main argv = 
    Console.Title <- "Resize Pong"
    Console.CursorVisible <- false
    Console.WindowWidth <- 70
    Console.BufferWidth <- Console.WindowWidth
    Console.WindowHeight <- 30
    Console.BufferHeight <- 45
    loop("title", Console.WindowHeight, std_board, [| std_y_pos; std_y_pos; std_x_pos; std_y_pos |], [| 1; 1 |], [| 0; 0 |])
    0