open System

type Orientation = Horizontal | Vertical
type Ship = Battleship of Orientation | Destroyer of Orientation

let rando = new System.Random()
let arr = [|0..9|]
let orientArr = Array.init<int> 3 (fun x -> rando.Next(2))
let sunkShipsArr = [|false;false;false|]
let mutable grid = Array2D.create 10 10 (false, false)      // the tuple: (ship or water, revealed or not)
let mutable squaresLeft = 0
let mutable battleshipPos, destroyer1Pos, destroyer2Pos = (0,0), (0,0), (0,0)

let battleship = if orientArr.[0] = 0 then Battleship Horizontal else Battleship Vertical
let destroyer1 = if orientArr.[1] = 0 then Destroyer  Horizontal else Destroyer  Vertical
let destroyer2 = if orientArr.[2] = 0 then Destroyer  Horizontal else Destroyer  Vertical

// visualize the grid
let plotGrid (grd:(bool*bool)[,]) =
    for x in [0..9] do
        Array.map (fun x -> match x with | (true, true) -> printf "█" | (false, true) -> printf "." | _ -> printf "?") grd.[x,0..] |> ignore
        printfn ""

// take input and verify compliance
let inputValidate (inputO: string option) =
    match inputO with
    | None -> None
    | Some input ->
        match input |> String.IsNullOrWhiteSpace with
        | true -> None
        | false ->
            match input.[0..0] |> "ABCDEFGHIJ".Contains
                 ,input.[1..] |> Int32.TryParse |> snd < 11
                 ,input.[1..] |> Int32.TryParse |> snd > 0 with
            | true, true, true -> Some input
            | _, _, _ -> None

// translate the input coordinates into integers and rebase while you're at it - will be handy for handling 0-indexed array
let coordsTranslate (str:string option) =
    match str with
    | None -> None
    | Some coords -> Some ((coords.[0] |> int) - 65, (coords.[1..] |> int) - 1)
                    
// place a ship on the grid
let placeShip (position, ship) =
    match position, ship with
    | (x, y), Battleship Horizontal -> grid.[x, y..(y+4)] <- Array.create 5 (true, false)
    | (x, y), Battleship Vertical   -> grid.[x..(x+4), y] <- Array.create 5 (true, false)
    | (x, y), Destroyer Horizontal  -> grid.[x, y..(y+3)] <- Array.create 4 (true, false)
    | (x, y), Destroyer Vertical    -> grid.[x..(x+3), y] <- Array.create 4 (true, false)

// check if a ship is completely sunk
let checkShip (position, ship) =
    match position, ship with
    | (x, y), Battleship Horizontal -> if grid.[x, y..(y+4)] = Array.create 5 (true, true) then true else false
    | (x, y), Battleship Vertical   -> if grid.[x..(x+4), y] = Array.create 5 (true, true) then true else false
    | (x, y), Destroyer Horizontal  -> if grid.[x, y..(y+3)] = Array.create 4 (true, true) then true else false
    | (x, y), Destroyer Vertical    -> if grid.[x..(x+3), y] = Array.create 4 (true, true) then true else false
    
// take valid coordinates, update the grid, feedback the player
let squareProcess (coords: (int*int) option) =
    match coords with
    | None -> coords |> ignore
    | Some (x,y) ->
        match grid.[x,y] with
        | (true, false) -> do
            grid.[x,y] <- (true, true)
            squaresLeft <- squaresLeft - 1
            printf "hit!"
            if checkShip (battleshipPos, battleship) && not sunkShipsArr.[0] then
                sunkShipsArr.[0] <- true
                printfn " you have sunk my battleship!"
            if checkShip (destroyer1Pos, destroyer1) && not sunkShipsArr.[1] then
                sunkShipsArr.[1] <- true
                printfn " you have sunk my destroyer!"
            if checkShip (destroyer2Pos, destroyer2) && not sunkShipsArr.[2] then
                sunkShipsArr.[2] <- true
                printfn " you have sunk my destroyer!"
        | (false, false) -> do
                 grid.[x,y] <- (false, true)
                 printf "miss!"
        | _ -> do
                 printf "you have tried that one already. these ships are not moving."
    Console.ReadLine() |> ignore

// keep re-populating the grid until total number of placed squares is 13 (under 13 means ships overlap or haven't been placed yet)
while squaresLeft < 13 do
    grid <- (Array2D.create 10 10 (false, false))
    battleshipPos <- if battleship = Battleship Horizontal then (rando.Next(0,10), rando.Next(0,6)) else (rando.Next(0,6), rando.Next(0,10))
    destroyer1Pos <- if destroyer1 =  Destroyer Horizontal then (rando.Next(0,10), rando.Next(0,7)) else (rando.Next(0,7), rando.Next(0,10))
    destroyer2Pos <- if destroyer2 =  Destroyer Horizontal then (rando.Next(0,10), rando.Next(0,7)) else (rando.Next(0,7), rando.Next(0,10))
    placeShip (battleshipPos, battleship)
    placeShip (destroyer1Pos, destroyer1)
    placeShip (destroyer2Pos, destroyer2)
    for i = 0 to 9 do
        arr.[i] <- grid.[i,0..] |> Array.filter (fun x -> x = (true, false)) |> Array.length
    squaresLeft <- arr |> Array.sum

// let sumArray array = Array.fold (fun acc elem -> acc + elem) "" arra

// main loop of the game - repeat until all the occupied squares are sunk
while squaresLeft > 0 do
    Console.Clear()
    grid |> plotGrid
    Console.Write("\nhit a squre: ")
    Some (Console.ReadLine().ToUpper()) |> inputValidate |> coordsTranslate |> squareProcess
printfn "You have sunk all my ships. you monster."
Console.ReadLine() |> ignore