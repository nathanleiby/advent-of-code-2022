open System.IO


//
// Utilities
//

// tap copied from https://github.com/jack-pappas/ExtCore/blob/2adff09ca1ed6a555a8e6564203ec8dbfeeb76c8/ExtCore/Pervasive.fs#L210-L212
let tap (action: 'T -> unit) (value: 'T) : 'T =
    action value
    value

let tee (value: 'T) = tap (fun x -> printfn "%A" x) value

let pipelog s x =
    printfn s
    x
//
// Problem
//

let toStr (cArr: char[]) =
    cArr
    |> Seq.map (fun x -> string x)
    |> String.concat ""
    |> (fun x -> x.Replace('[', ' '))
    |> (fun x -> x.Replace(']', ' '))
    |> (fun x -> x.Trim())

let parseLine line =
    // line |> Seq.chunkBySize 4 |> Seq.map (fun x -> String.concat "," x)
    let chunked = line |> Seq.chunkBySize 4

    chunked |> Seq.map toStr

let parseInput input rowCount colCount =
    let parsedInput =
        input
        |> Seq.take rowCount
        |> Seq.map parseLine
        |> List.ofSeq
        |> List.rev
        |> Seq.ofList

    let mutable stacksList = Array.init colCount (fun x -> [])

    for row in parsedInput do
        for (colIdx, colVal) in Seq.indexed row do
            if not (colVal.Equals("")) then
                let mutable stack = stacksList[colIdx]
                stack <- List.append stack [ colVal ]
                stacksList[colIdx] <- stack

    stacksList |> Array.map List.rev

let parseSingleInstruction (line: string) : (int * int * int) =
    line.Split(" ")
    |> Array.ofSeq
    |> (fun x -> [| x[1]; x[3]; x[5] |])
    |> Array.map int
    |> (fun x -> (x[0], x[1], x[2]))

let parseInstructions (input: string array) rowCount : seq<int * int * int> =
    input
    |> Seq.skip (rowCount + 2) // also skip col idxs and blank row
    |> Seq.map parseSingleInstruction

let partOne f rowCount colCount =
    printfn "\nRunning part one for %s...\n" f

    let lines = File.ReadAllLines(f)
    let stacks = parseInput lines rowCount colCount
    let instructions = parseInstructions lines rowCount

    let moveHelper fromList toList =
        let h = fromList |> List.head
        let t = fromList |> List.tail

        // put on top of stack
        let newToList = List.append [ h ] toList

        t, newToList

    let move fromId toId =
        // handle 1-indexing
        let (fromNew, toNew) = moveHelper stacks[fromId - 1] stacks[toId - 1]

        stacks[fromId - 1] <- fromNew
        stacks[toId - 1] <- toNew

        stacks

    let run instructions =
        for inst in instructions do
            let count, fromId, toId = inst

            for _ in 1..count do
                move fromId toId |> ignore

    run instructions

    stacks |> Array.map List.head |> Array.map string |> String.concat ""

let partTwo f rowCount colCount =
    printfn "\nRunning part two for %s...\n" f

    let lines = File.ReadAllLines(f)
    let stacks = parseInput lines rowCount colCount
    let instructions = parseInstructions lines rowCount

    let moveHelper fromList toList (q: int) =
        let front, back = List.splitAt q fromList


        // put on top of stack
        let newToList = List.append front toList

        back, newToList

    let move instruction =
        // handle 1-indexing
        let count, fromId, toId = instruction

        let (fromNew, toNew) = moveHelper stacks[fromId - 1] stacks[toId - 1] count

        stacks[fromId - 1] <- fromNew
        stacks[toId - 1] <- toNew

        stacks

    let run (ii: seq<int * int * int>) =
        for inst in ii do
            move inst |> ignore

    run instructions

    stacks |> Array.map List.head |> Array.map string |> String.concat ""


// part 1
let exampleOne = partOne "./example.txt" 3 3
printfn "%A" exampleOne
assert (exampleOne = "CMZ")
printfn "%A" (partOne "./input.txt" 8 9)

// part 2
let example2 = partTwo "./example.txt" 3 3
printfn "%A" exampleOne
assert (example2 = "MCD")
printfn "%A" (partTwo "./input.txt" 8 9)
