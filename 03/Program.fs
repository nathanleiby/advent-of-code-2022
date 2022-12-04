open System.IO

// tap copied from https://github.com/jack-pappas/ExtCore/blob/2adff09ca1ed6a555a8e6564203ec8dbfeeb76c8/ExtCore/Pervasive.fs#L210-L212
let tap (action: 'T -> unit) (value: 'T) : 'T =
    action value
    value

let tee (value: 'T) = tap (fun x -> printfn "%A" x) value

let boolToInt b =
    match b with
    | true -> 1
    | false -> 0

let letterScore c =
    let capitalLetterBonus = boolToInt (System.Char.IsUpper c) * 26
    let letterVal = int (System.Char.ToLower c) - (int 'a')
    letterVal + capitalLetterBonus + 1

let processLine line =
    line
    // split in half
    |> Array.ofSeq
    |> Array.splitInto 2
    // turn into sets
    |> Array.map Set.ofSeq
    // set intersection
    |> (fun list -> Set.intersect list[0] list[1])
    // get the one character in the intersection
    |> Set.toSeq
    |> Seq.head
    // convert to letter value
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


let partTwo file =
    File.ReadAllLines(file)
    |> Array.chunkBySize 3
    // |> tee
    |> Seq.map (Seq.map Set.ofSeq)
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
