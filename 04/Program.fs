// For more information see https://aka.ms/fsharp-console-apps

open System.IO

//
// Utilities
//

// tap copied from https://github.com/jack-pappas/ExtCore/blob/2adff09ca1ed6a555a8e6564203ec8dbfeeb76c8/ExtCore/Pervasive.fs#L210-L212
let tap (action: 'T -> unit) (value: 'T) : 'T =
    action value
    value

let tee (value: 'T) = tap (fun x -> printfn "%A" x) value

let boolToInt b =
    match b with
    | true -> 1
    | false -> 0

let readInput f = File.ReadAllLines(f)

let solve solveFn file = printfn "%d" (solveFn (readInput file))

//
// Problem
//

// Part 1

let fullyContains ((x: int, y: int), (a: int, b: int)) =
    match (x, y, a, b) with
    | _ when (x >= a && y <= b) -> true
    | _ when (a >= x && b <= y) -> true
    | _ -> false

let parseIntPair (s: string) =
    s.Split [| '-' |] |> Array.map int |> Array.ofSeq

let parseLine (line: string) =
    line.Split [| ',' |]
    |> Array.map parseIntPair
    |> (fun x -> fullyContains ((x[0][0], x[0][1]), (x[1][0], x[1][1])))
    |> boolToInt

let partOne (lines: seq<string>) = lines |> Seq.map parseLine |> Seq.sum

// Part 2
let overlaps (r1: int * int, r2: int * int) =
    let min1 = fst r1
    let max1 = snd r1
    let min2 = fst r2
    let max2 = snd r2

    match (min1, max1, min2, max2) with
    | _ when (min1 <= min2 && min2 <= max1) -> true
    | _ when (min2 <= min1 && min1 <= max2) -> true
    | _ -> false

let parseLine2 (line: string) =
    line.Split [| ',' |]
    |> Array.map parseIntPair
    |> Array.map (fun x -> (x[0], x[1]))

let partTwo (lines: seq<string>) =
    lines
    |> Seq.map parseLine2
    |> Seq.map (fun x -> overlaps (x[0], x[1]))
    |> Seq.map boolToInt
    |> Seq.sum


printfn "Part One:"
solve partOne "./example.txt"
solve partOne "./input.txt"

printfn "Part Two:"
solve partTwo "./example.txt"
solve partTwo "./input.txt"
