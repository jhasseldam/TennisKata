module Tennis

// Make illegal states unrepresentable 

type Player = PlayerOne | PlayerTwo

type Point = Love | Fifteen | Thirty 

type PointsData = { PlayerOnePoints : Point; PlayerTwoPoints : Point }

type FortyData = { Player : Player; OtherPlayerPoint : Point }

type Score = 
    Points of PointsData
    | Forty of FortyData
    | Deuce
    | Advantage of Player
    | Game of Player

let getNextScore prevScore =
   match prevScore with
    | Love -> Some Fifteen
    | Fifteen -> Some Thirty
    | _ -> None

let changeScore points scoringPlayer =
    match scoringPlayer with 
    | PlayerOne -> match getNextScore points.PlayerOnePoints with
                   | None -> Forty { Player = scoringPlayer; OtherPlayerPoint = points.PlayerTwoPoints} 
                   | Some p -> Points { points with PlayerOnePoints = p} 
    | PlayerTwo -> match getNextScore points.PlayerTwoPoints with
                   | None -> Forty { Player = scoringPlayer; OtherPlayerPoint = points.PlayerOnePoints} 
                   | Some p -> Points { points with PlayerTwoPoints = p} 

let settleFortyScore fd scoringPlayer =
    if scoringPlayer = fd.Player 
    then Game scoringPlayer
    else
        match getNextScore fd.OtherPlayerPoint with
        | Some p -> Forty { fd with OtherPlayerPoint = p }
        | None -> Deuce
     
let pointFor scoringPlayer currentScore = 
    match currentScore with
    | Points p -> changeScore p scoringPlayer
    | Forty fd -> settleFortyScore fd scoringPlayer
    | Deuce -> Advantage scoringPlayer
    | Advantage p -> if p = scoringPlayer then Game scoringPlayer else Deuce 
    | Game p -> Game scoringPlayer 