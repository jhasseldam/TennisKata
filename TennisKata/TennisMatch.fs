module TennisMatch

open System
open Tennis

let rand = new Random()

let names = ["Adam"; "Bob"; "Cathrine"; "Donna"; "Eric"; "Fiona"; "Garry"; "Harry"; "Ivan"; "John"; "Garry"]

let rec getRandomNames () = 
    let maxIndex = names.Length - 1
    let name1 = List.item (rand.Next(maxIndex)) names 
    let name2 = List.item (rand.Next(maxIndex)) names
    if name1 = name2 then getRandomNames () 
    else name1, name2 

let getBallWinner () = 
    let s = rand.Next(2)
    match s with
    | 0 -> PlayerOne
    | 1 -> PlayerTwo
    | _ -> failwith "Random is broken, contact MS"

let prettyPrintPoint p =
    match p with
    | Love -> "Love" 
    | Fifteen -> "15"
    | Thirty -> "30"


let runGame () =
    let n1, n2 = getRandomNames ()
    printfn "Welcome to a new existing game between %s and %s\n" n1 n2
    let getName player =
        match player with
        | PlayerOne -> n1
        | PlayerTwo -> n2
    let other p1 =
        match p1 with
        | PlayerOne -> PlayerTwo
        | PlayerTwo -> PlayerOne
    let rec nextPoint score =
        let scoringPlayer = getBallWinner ()
        let newScore = pointFor scoringPlayer score
        match newScore with
        | Game p -> printfn "%s won the match" (getName p)
        | Forty fd -> 
            printfn "%s : %s - %s : %s" (getName fd.Player) "40" (getName (other fd.Player)) (prettyPrintPoint fd.OtherPlayerPoint)
            nextPoint newScore
        | Deuce -> 
            printfn "Deuce"
            nextPoint newScore
        | Advantage p -> 
            printfn "Advantage %s" (getName p) 
            nextPoint newScore
        | Points pd -> 
            printfn "%s : %s - %s : %s" n1 (prettyPrintPoint pd.PlayerOnePoints) n2 (prettyPrintPoint pd.PlayerTwoPoints) 
            nextPoint newScore
    nextPoint (Points { PlayerOnePoints = Love; PlayerTwoPoints = Love})



