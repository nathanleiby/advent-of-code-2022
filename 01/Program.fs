// For more information see https://aka.ms/fsharp-console-apps

open System.IO
open System.Collections.Generic


let lines = File.ReadAllLines(@"./input.txt")

let mutable runningTotal = 0
let ll = new List<int>()

for i in lines do
    if (i = "") then
        ll.Add(runningTotal)
        runningTotal <- 0
    else
        runningTotal <- runningTotal + (i |> int)

let sumTopN count =
    (ll.ToArray() |> seq<int>)
    |> Seq.sortBy (fun x -> -x)
    |> Seq.take count
    |> Seq.sum
    |> printfn "%d"

// pt 1
sumTopN 1

// pt 2
sumTopN 3
