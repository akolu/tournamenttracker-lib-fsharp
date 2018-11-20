namespace TournamentTracker
    
type Pairing = { Number: int; Player1: string; Player2: string; Player1Score: int; Player2Score: int }
type Round = { Number: int; Pairings: List<Pairing>; Finished: bool } with
    member this.Standings = 
        this.Pairings 
        |> List.map (fun p -> [(p.Player1, p.Player1Score);(p.Player2, p.Player2Score)])
        |> List.concat
        |> Map.ofList
        
type Tournament = {Rounds: List<Round>; Players: List<string>} with    
    member this.CurrentRound = 
        let ongoing = this.Rounds |> List.tryFind(fun round -> (=) round.Finished false)
        match ongoing with | Some rnd -> Ok rnd | None -> Error "Tournament already finished"
    
    member this.Standings = 
        let mergeMaps map1 map2 =
            Map.fold (fun state key value -> match Map.tryFind key state with 
                                             | Some v -> Map.add key (value + v) state 
                                             | None -> Map.add key value state) map1 map2
        let players = this.Players |> List.map (fun p -> (p,0)) |> Map.ofList
        this.Rounds 
        |> List.map (fun r -> r.Standings)
        |> List.fold (fun acc scores -> mergeMaps acc scores) players

//type FooBar =
//    inherit Tournament
//    override u.Standings = 
//        []

module Tournament =

    let (>>=) x f = Result.bind f x

    let private replace fn item list = list |> List.map (fun i -> if fn i then item else i)

    let createTournament rounds =
        let defaultRound index = {Number=(+) index 1; Pairings=[]; Finished=false}
        match rounds with
        | n when n > 0 -> Ok {Rounds=(List.init rounds defaultRound); Players=[]}
        | _ -> Error "Tournament should have at least one round"

    let addPlayer player tournament =
        let existing = tournament.Players |> List.tryFind (fun p -> (=) player p) 
        match existing with
        | Some _ -> Error "Player with that name already exists"
        | None -> Ok {tournament with Players=player::tournament.Players |> List.sort}

    let rec addPlayers players (tournament: Tournament) =
        match players with | [] -> Ok tournament
                           | [x] -> addPlayer x tournament
                           | x::rest -> addPlayer x tournament >>= addPlayers rest

    let private modifyCurrentRound fn (tournament: Tournament) =
        match tournament.CurrentRound with
        | Ok rnd -> Ok {tournament with Rounds=(tournament.Rounds |> replace ((=) rnd) (fn rnd))}
        | Error err -> Error err

    let private modifyPairing (modified: Pairing) (rnd: Round) =
        {rnd with Pairings=(rnd.Pairings |> replace (fun p -> ((=) p.Number modified.Number)) modified)}

    let finishRound tournament =
        tournament |> modifyCurrentRound (fun round -> {round with Finished=true})

    let private allocateToPairings (players: (string * int) list): Pairing list =
        let makePairing number p1 p2 = {Number=number;Player1=p1;Player2=p2;Player1Score=0;Player2Score=0}
        players
        |> List.chunkBySize 2 
        |> List.fold (fun acc chunk -> acc @ [makePairing (acc.Length) (fst chunk.[0]) (fst chunk.[1])]) []

    let pair (pairingFunc: (string * int) list -> (string * int) list) (tournament: Tournament) =
        let oddNumberOfPlayers (list: 'a list) = (<>) ((%) list.Length 2) 0
        let addPairings playersWithScores = 
            let playerList = pairingFunc (playersWithScores |> Map.toList)
            tournament |> modifyCurrentRound (fun r -> {r with Pairings=(allocateToPairings playerList)})
        match tournament.Players with 
        | [] -> Error "Player list is empty" 
        | list when oddNumberOfPlayers list -> addPairings (tournament.Standings.Add ("BYE",0))
        | _ -> addPairings tournament.Standings

    let score number result (tournament: Tournament) =
        let addResults pairing round = 
            round |> modifyPairing {pairing with Player1Score=fst result;Player2Score=snd result}
        let pairing = tournament.CurrentRound 
                      >>= (fun r -> match r.Pairings |> List.tryFind (fun p -> ((=) number p.Number)) with 
                                    | Some pairing -> Ok pairing 
                                    | None -> Error (sprintf "Match %i not found!" number))
        match pairing with
        | Ok p -> tournament |> modifyCurrentRound (addResults p)
        | Error err -> Error err

    let private swapPlayers p1 p2 pairing = 
        let tryReplace = function | p when ((=) p1 p) -> p2 | p when ((=) p2 p) -> p1 | p -> p
        {pairing with 
            Player1=(tryReplace pairing.Player1);
            Player2=(tryReplace pairing.Player2)}

    let private verifyUnscored pairing round =
        match pairing with
        | p when ((=) p.Player1Score 0) && ((=) p.Player2Score 0) -> Ok round
        | _ -> Error "Can't swap players if either player's round has already been scored!"

    let private validatePlayerSwap player round =
        let exists player pairing = ((=) player pairing.Player1) || ((=) player pairing.Player2)
        match round.Pairings |> List.tryFind (exists player) with
        | Some pairing -> round |> verifyUnscored pairing
        | None -> Error (sprintf "Player %s not found" player)

    let swap player1 player2 (tournament: Tournament) =
        let pairings = tournament.CurrentRound
                        >>= validatePlayerSwap player1
                        >>= validatePlayerSwap player2
                        >>= (fun round -> Ok (round.Pairings |> List.map (swapPlayers player1 player2)))
        match (pairings) with
        | Ok pairings -> tournament |> modifyCurrentRound (fun rnd -> {rnd with Pairings=pairings})
        | Error err -> Error err

