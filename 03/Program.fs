// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#!"

open System.IO
open System.Collections.Generic

// utils
// copied from https://github.com/jack-pappas/ExtCore/blob/2adff09ca1ed6a555a8e6564203ec8dbfeeb76c8/ExtCore/Pervasive.fs#L210-L212
let tap (action: 'T -> unit) (value: 'T) : 'T =
    action value
    value


let splitList (list) =
    let n = (Seq.length list) / 2
    let firstList = list |> Seq.take n |> Seq.toList
    let secondList = list |> Seq.skip n |> Seq.toList
    (firstList, secondList)

let boolToInt b =
    match b with
    | true -> 1
    | false -> 0

let letterScore c =
    let capitalLetterBonus = boolToInt (System.Char.IsUpper c) * 26
    let letterVal = int (System.Char.ToLower c) - (int 'a')
    letterVal + capitalLetterBonus + 1

let processLine line =
    // split in half
    splitList line
    // convert to Set
    // |> tap (fun x -> printfn "line =  %A" x)
    |> (fun pair -> (Set.ofList (fst pair), Set.ofList (snd pair)))
    // set intersection
    |> (fun pair -> Set.intersect (fst pair) (snd pair))
    |> Set.toList
    |> (fun x -> x[0])
    |> letterScore


let partOne file =
    File.ReadAllLines(file)
    |> Seq.ofArray
    |> Seq.map processLine
    |> Seq.sum
    |> printfn "%A"

partOne "./example.txt"
partOne "./input.txt"
