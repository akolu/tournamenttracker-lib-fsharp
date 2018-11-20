module TournamentTests

open System
open TournamentTracker.Tournament
open TournamentTracker
open NUnit.Framework

[<TestFixture>]
type TestClass () =

    let unwrap res = match res with | Ok res -> res | Error err -> raise (Exception(err.ToString()))

    let createTestTournament numOfRounds =
        (Tournament.createTournament numOfRounds)

    let table number (p1, p2) = 
        let pairing = {Number=0;Player1="";Player2="";Player1Score=0;Player2Score=0}
        {pairing with Number=number;Player1=p1;Player2=p2}

    let result (p1Score, p2Score) pairing =
        {pairing with Player1Score=p1Score;Player2Score=p2Score}

    let playerNames pairing = (pairing.Player1, pairing.Player2)
    
    [<Test>]
    member this.``creating a tournament with zero or negative rounds returns Error`` () =
        match createTestTournament 0 with 
        | Ok _ -> failwith "createTournament should not succeed with 0 rounds"
        | Error msg -> Assert.AreEqual("Tournament should have at least one round", msg)

    [<Test>]
    member this.``tournament can be created with a specified number of rounds`` () = 
        let tournament = createTestTournament 5 
                         |> unwrap
        Assert.AreEqual(5, tournament.Rounds.Length)

    [<Test>]
    member this.``initial rounds are numbered, unfinished and have no pairings``  () =
        let tournament = createTestTournament 2 
                         |> unwrap
        let expected = [
            {Number=1;Pairings=[];Finished=false};
            {Number=2;Pairings=[];Finished=false}
        ]
        Assert.AreEqual(expected, tournament.Rounds)

    [<Test>]
    member this.``players can be added one at a time`` () = 
        let tournament = createTestTournament 1 
                         >>= addPlayer "Alice" 
                         |> unwrap
        CollectionAssert.AreEqual(["Alice"], tournament.Players)

    [<Test>]
    member this.``players are added in alphabetical order`` () =
        let tournament = createTestTournament 1 
                         >>= addPlayer "Michael"
                         >>= addPlayer "Alice"
                         >>= addPlayer "Bob"
                         |> unwrap
        CollectionAssert.AreEqual(["Alice";"Bob";"Michael"], tournament.Players)

    [<Test>]
    member this.``duplicate players may not be added`` () =
        let tournament = createTestTournament 1 
                        >>= addPlayer "Alice" 
                        >>= addPlayer "Alice" 
        match tournament with
        | Ok _ -> failwith "Should not be possible to add duplicate players"
        | Error err -> Assert.AreEqual("Player with that name already exists", err)

    [<Test>]
    member this.``multiple players can be added`` () =
       let tournament = createTestTournament 1
                        >>= addPlayers ["Bob";"Alice"]
                        |> unwrap
       CollectionAssert.AreEqual(["Alice";"Bob"], tournament.Players)

    [<Test>]
    member this.``current round can be marked as finished`` () =
        let t1: Tournament = createTestTournament 2 
                             >>= finishRound 
                             |> unwrap
        Assert.AreEqual(true, t1.Rounds.[0].Finished)
        Assert.AreEqual(false, t1.Rounds.[1].Finished)

        let t2: Tournament = t1 
                             |> finishRound 
                             |> unwrap
        Assert.AreEqual(true, t2.Rounds.[0].Finished)
        Assert.AreEqual(true, t2.Rounds.[1].Finished)

    [<Test>]
    member this.``trying to finish round returns Error if all rounds are finished`` () =
        let tournament = createTestTournament 1 
                         >>= finishRound 
                         >>= finishRound
        match tournament with
        | Ok _ -> failwith "Trying to finish already finished round should not succeed"
        | Error err -> Assert.AreEqual(err, "Tournament already finished")

    [<Test>]
    member this.``pairings for current round are determined by the order of pairing algorithm`` () =
        let tournament = createTestTournament 1
                         >>= addPlayers ["Alice";"Bob";"James";"Michael"]
                         >>= pair (fun lis -> List.rev lis)
                         |> unwrap
        let pairings = tournament.Rounds.[0].Pairings
        Assert.AreEqual("Michael", pairings.[0].Player1)
        Assert.AreEqual("James", pairings.[0].Player2)
        Assert.AreEqual("Bob", pairings.[1].Player1)
        Assert.AreEqual("Alice", pairings.[1].Player2)

    [<Test>]
    member this.``pairings cannot be set if player list is empty`` () =
        let tournament = createTestTournament 1
                         >>= pair (fun lis -> lis)
        match tournament with
        | Ok _ -> failwith "trying to create pairings with empty player list should fail"
        | Error err -> Assert.AreEqual("Player list is empty", err)

    [<Test>]
    member this.``on odd number of players, BYE is added to player list for pairings`` () =
        let tournament = createTestTournament 1
                         >>= addPlayer "Alice"
                         >>= pair (fun lis -> lis)
                         |> unwrap
        Assert.AreEqual("Alice", tournament.Rounds.[0].Pairings.[0].Player1)
        Assert.AreEqual("BYE", tournament.Rounds.[0].Pairings.[0].Player2)
    
    [<Test>]
    member this.``players cannot face same opponent twice if unplayed opponent can be found`` () =
        let addRoundsWithPairings round1Pairings round2Pairings tournament = 
            Ok {tournament with Rounds=[{Number=1; Pairings=round1Pairings; Finished=true};
                                        {Number=2; Pairings=round2Pairings; Finished=true};
                                        {Number=3; Pairings=[]; Finished=false}]}

        // standings before round 1:
        // Alice=0, Bob=0, James=0, Michael=0, Lily=0, Jack=0
        let round1Pairings = [table 1 ("Alice","Bob") |> result (11,9);
                              table 2 ("James","Michael") |> result (0,20);
                              table 3 ("Lily","Jack") |> result (10,10)]

        // standings before round 2:
        // Michael=20, Alice=11, Jack=10, Lily=10, Bob=9, James=0
        let round2Pairings = [table 1 ("Michael","Alice") |> result (1,19);
                              table 2 ("Jack","Lily") |> result (0,20);
                              table 3 ("Bob","James") |> result (0,20)]

        //  standings before round 3:
        //  Alice=30, Lily=30, Michael=21, James=20, Jack=10, Bob=9
        let tournament = createTestTournament 1
                         >>= addPlayers ["Alice";"Bob";"James";"Michael";"Lily";"Jack"]
                         >>= addRoundsWithPairings round1Pairings round2Pairings
                         >>= pair (fun lis -> lis) 
                         |> unwrap

        let round3Pairings = tournament.Rounds.[2].Pairings
        Assert.AreEqual(("Alice", "Lily"), round3Pairings.[0] |> playerNames)
        Assert.AreEqual(("Michael", "Jack"), round3Pairings.[1] |> playerNames)
        Assert.AreEqual(("James", "Bob"), round3Pairings.[2] |> playerNames)

    [<Test>]
    member this.``score can be set for pairing by table number`` () =
        let unscored = [{Number=1;Finished=false;Pairings=[table 123 ("Alice","Bob");
                                                           table 456 ("James","Michael")]}]
        let tournament = 
            {(createTestTournament 1 |> unwrap) with Rounds=unscored}
            |> score 456 (13,8)
            |> unwrap
        
        let round = tournament.Rounds.[0];
        Assert.AreEqual(unscored.[0].Pairings.[0], round.Pairings.[0])
        Assert.AreEqual(13, round.Pairings.[1].Player1Score)
        Assert.AreEqual(8, round.Pairings.[1].Player2Score)
        
    [<Test>]
    member this.``score is set for current round only`` () =
        let unscored = [{Number=1;Finished=true;Pairings=[table 123 ("Alice","Bob")]}
                        {Number=2;Finished=false;Pairings=[table 123 ("Bob","Alice")]}]
        let tournament = 
            {(createTestTournament 1 |> unwrap) with Rounds=unscored}
            |> score 123 (15,5)
            |> unwrap

        Assert.AreEqual(unscored.[0], tournament.Rounds.[0])
        Assert.AreEqual(15, tournament.Rounds.[1].Pairings.[0].Player1Score)
        Assert.AreEqual(5, tournament.Rounds.[1].Pairings.[0].Player2Score)

    [<Test>]
    member this.``score returns Error if specified pairing cannot be found`` () =
        let tournament = 
            {(createTestTournament 1 |> unwrap) with Rounds=[{Number=1;Finished=false;Pairings=[]}]}
            |> score 123 (11,9)
        match tournament with 
        | Ok _ -> failwith "Trying to score non-existent round should return Error"
        | Error err -> Assert.AreEqual("Match 123 not found!", err)

    [<Test>]
    member this.``score returns Error if round has already been marked as finished`` () =
        match createTestTournament 1 >>= finishRound >>= score 123 (10,10) with
        | Ok _ -> failwith "Should fail with 'already finished' error"
        | Error err -> Assert.AreEqual("Tournament already finished", err)

    [<Test>]
    member this.``round standings are listed in map displaying names and scores`` () =
        let round = 
            {Number=1;Finished=true;Pairings=
                        [{Number=1;Player1="Alice";Player2="Bob";Player1Score=4;Player2Score=16};
                        {Number=2;Player1="James";Player2="Michael";Player1Score=15;Player2Score=5}]}
        CollectionAssert.AreEqual(
            Map.ofList([("Bob",16);("James",15);("Michael",5);("Alice",4)]), round.Standings)

    [<Test>]
    member this.``total standings are listed in map with scores from all rounds`` () =
        let rounds = [
            {Number=1;Finished=true;Pairings=
                [table 1 ("Alice","Bob") |> result (12,8);
                 table 2 ("James","Michael") |> result (1,19)]}
            {Number=2;Finished=true;Pairings=
                [table 1 ("Michael","Alice") |> result (13,7);
                 table 2 ("Bob","James") |> result (10,10)]}] 
        let tournament = {(createTestTournament 2 |> unwrap) with Rounds=rounds}
        CollectionAssert.AreEqual(
            Map.ofList([("Michael", 32);("Alice", 19);("Bob", 18);("James", 11)]), tournament.Standings)

    [<Test>]
    member this.``total standings are equal to player list if no rounds have been played`` () =
        let round = {Number=1;Finished=false;Pairings=[]}
        let tournament = createTestTournament 1
                         >>= addPlayers ["Alice";"Bob";"James";"Michael"]
                         |> unwrap
        CollectionAssert.AreEqual(
            Map.ofList([("Alice",0);("Bob",0);("James",0);("Michael",0)]), tournament.Standings)

    [<Test>]
    member this.``swap changes places of two players in current round`` () =
        let rounds = [{Number=1;Finished=false;Pairings=
                        [table 1 ("Alice","Bob")
                         table 2 ("James","Michael")]}]
        let tournament = {(createTestTournament 1 |> unwrap) with Rounds=rounds}
                         |> swap "Bob" "Michael" 
                         |> unwrap
        Assert.AreEqual(table 1 ("Alice","Michael"), tournament.Rounds.[0].Pairings.[0])
        Assert.AreEqual(table 2 ("James","Bob"), tournament.Rounds.[0].Pairings.[1])
        
    [<Test>]
    member this.``swap changes places even if they are already against each other`` () =
        let rounds = [{Number=1;Finished=false;Pairings=
                        [table 1 ("Alice","Bob")]}]
        let tournament = {(createTestTournament 1 |> unwrap) with Rounds=rounds}
                         |> swap "Alice" "Bob"
                         |> unwrap
        Assert.AreEqual(table 1 ("Bob","Alice"), tournament.Rounds.[0].Pairings.[0])

    [<Test>]
    member this.``swap returns Error if either player is not found in pairings list`` () =
        let rounds = [{Number=1;Finished=false;Pairings=[table 1 ("Alice","Bob")
                                                         table 2 ("James","Michael")]}]
        match ({(createTestTournament 1 |> unwrap) with Rounds=rounds} |> swap "Alice" "Nyarlathotep") with
        | Ok _ -> failwith "Trying to swap nonexistent players should return Error"
        | Error err -> Assert.AreEqual("Player Nyarlathotep not found", err)

    [<Test>] 
    member this.``swap returns Error if either player's initial round has been scored`` () =
        let rounds = [{Number=1;Finished=false;Pairings=
                        [table 1 ("Alice","Bob") |> result (19,1)
                         table 2 ("James","Michael")]}]
        match ({(createTestTournament 1 |> unwrap) with Rounds=rounds} |> swap "Alice" "Michael") with
        | Ok _ -> failwith "Trying to swap players should return Error if either player's round is scored"
        | Error err -> Assert.AreEqual("Can't swap players if either player's round has already been scored!", err)