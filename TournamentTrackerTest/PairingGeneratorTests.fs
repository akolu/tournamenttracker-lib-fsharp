module PairingGeneratorTests
open TournamentTracker.PairingGenerator
open TournamentTracker
open NUnit.Framework

[<TestFixture>]
type TestClass () =

    let players = [("Alice", 28); ("Bob", 14); ("James", 21); ("Michael", 17)]

    [<Test>]
    member this.``shuffle yields different result in two parallel shuffles`` () =
        let playerLists = List.init 10 (fun _ -> shuffle players)
        let playerLists2 = List.init 10 (fun _ -> shuffle players)
        CollectionAssert.AreNotEqual(playerLists, playerLists2)

    [<Test>]
    member this.``shuffle retains the exact amount of items in a list`` () =
        let shuffled = shuffle players
        Assert.AreEqual(players.Length, shuffled.Length)
        CollectionAssert.Contains(shuffled, ("Alice", 28))
        CollectionAssert.Contains(shuffled, ("Bob", 14))
        CollectionAssert.Contains(shuffled, ("James", 21))
        CollectionAssert.Contains(shuffled, ("Michael", 17))

    [<Test>]
    member this.``swiss orders player list by score`` () =
        let swissed = swiss players
        Assert.AreEqual(4, swissed.Length)
        Assert.AreEqual(("Alice",28), swissed.[0])
        Assert.AreEqual(("James",21), swissed.[1])
        Assert.AreEqual(("Michael",17), swissed.[2])
        Assert.AreEqual(("Bob",14), swissed.[3])

    [<Test>]
    member this.``swiss orders players by alphabetically if score is tied``() =
       let swissed = swiss [("Bob", 20); ("Michael", 10); ("Alice", 0); ("James", 10)]
       Assert.AreEqual(("Bob",20), swissed.[0])
       Assert.AreEqual(("James",10), swissed.[1])
       Assert.AreEqual(("Michael",10), swissed.[2])
       Assert.AreEqual(("Alice",0), swissed.[3])