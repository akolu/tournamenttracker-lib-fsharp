namespace TournamentTracker
open System
module PairingGenerator =

    let shuffle (items: 'a list): 'a list =
        items |> List.sortBy(fun _ -> Guid.NewGuid()) 
    
    let swiss (items: ('a * int) list): ('a * int) list =
        items |> List.sortBy(fun (name, score) -> -score, name)
