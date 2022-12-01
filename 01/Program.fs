// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

open System.IO
open System.Collections.Generic

let dict = new Dictionary<int, int>()

let lines = File.ReadAllLines(@"./input1.1.txt")

let mutable id = 0
let mutable runningTotal = 0
let list = Seq.toList lines
for i in list do
    printfn "i = '%s'\n" i
    if (i = "") then
        dict[id] <- runningTotal
        id <- (id + 1)
        runningTotal <- 0
    else
        runningTotal <- runningTotal + (i |> int)

printfn "%A" dict

let mutable maxKey = -1
let mutable maxVal = -1
for kv in dict do
    if (kv.Value > maxVal) then
        maxKey <- kv.Key
        maxVal <- kv.Value

// pt 1
printfn "maxKey = %d maxVal = %d" maxKey maxVal

// pt 2
