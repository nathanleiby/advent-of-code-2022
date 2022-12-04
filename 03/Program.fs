open System.IO

// utils
// copied from https://github.com/jack-pappas/ExtCore/blob/2adff09ca1ed6a555a8e6564203ec8dbfeeb76c8/ExtCore/Pervasive.fs#L210-L212
let tap (action: 'T -> unit) (value: 'T) : 'T =
    action value
    value

let tee (value: 'T) = tap (fun x -> printfn "%A" x) value

let splitListInHalf (list) =
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
    splitListInHalf line
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

// Intersects three sets and computes letter score of the output
let partTwoHelper seq =
    let l = List.ofSeq seq

    let i1 = Set.intersect l[0] l[1]
    let i2 = Set.intersect i1 l[2]

    i2 |> Set.toList |> (fun x -> x[0]) |> letterScore


let seqToSet x = x |> Seq.toList |> Set.ofList

let partTwo file =
    File.ReadAllLines(file)
    |> Array.chunkBySize 3
    |> Seq.ofArray
    // |> tee
    |> Seq.map (Seq.map seqToSet)
    // |> tee
    |> Seq.map partTwoHelper
    // |> tee
    |> Seq.sum
    |> printfn "%A"

printfn "Part One:"
partOne "./example.txt"
partOne "./input.txt"

printfn "Part Two:"
partTwo "./example.txt"
partTwo "./input.txt"
