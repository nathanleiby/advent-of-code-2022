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

// Part 1
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

let parseOneInst (line: string) : (int * int * int) =
    line.Split(" ")
    |> Array.ofSeq
    |> (fun x -> [| x[1]; x[3]; x[5] |])
    |> Array.map int
    |> (fun x -> (x[0], x[1], x[2]))

// // |> fun x -> x.Split(" ")
// // |> Array.ofSeq
// // |> fun x -> [| x[1]; x[3]; x[5] |]
// // |> tee
// // |> Array.map int
// // |> fun x -> (x[0], x[1], x[2])
// |> (1, 2, 1)

// (1, 2, 1)

let parseInstructions (input: string array) rowCount =
    input
    |> Seq.skip (rowCount + 2) // also skip col idxs and blank row
    |> Seq.map parseOneInst

// |> Seq.map (fun x -> x.Split(" "))
// |> Seq.map Array.ofSeq
// |> Seq.map (fun x -> [| x[1]; x[3]; x[5] |])
// |> tee
// |> Seq.map (fun x -> Array.map int)
// // |> Seq.map Array.ofSeq
// |> Seq.map (fun x -> (x[0], x[1], x[2]))
// |> List.ofSeq
// |> List.rev
// |> Seq.ofList

// let instructions = [ (1, 2, 1); (3, 1, 3); (2, 2, 1); (1, 1, 2) ]
// instructions

let partOne f rowCount colCount =
    let lines = File.ReadAllLines(f)
    let stacks = parseInput lines rowCount colCount
    let instructions = parseInstructions lines rowCount

    // printfn "Stacks = %A" stacks

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

    // TODO: parse the instructions
    let run instructions =
        for inst in instructions do
            let count, fromId, toId = inst

            for _ in 1..count do
                move fromId toId |> ignore
    // printfn "%A"

    printfn "\nRunning...\n"

    // let instructions = [ (1, 2, 1); (3, 1, 3); (2, 2, 1); (1, 1, 2) ]
    run instructions

    let out = stacks |> Array.map List.head |> Array.map string |> String.concat ""
    // assert (out = "CMZ")

    out



// TODO: Seq.take controls how many lines are considered as "cargo"
// -> maybe.. takeWhile  line != "" and omit last line with numbers


// if (idx = 0) then

// initialize em do

// let mutable stack = []
// stack <- List.append s1 [ 'N' ]
// stack <- List.append s1 [ 'Z' ]

// // create the the list of stacks (whole "cargo" representati)
// stacksList <- List.append stacksList [ s1 ]
// stacksList <- List.append stacksList [ s2 ]
// stacksList <- List.append stacksList [ s3 ]

// let arrayFill1 = [| 1..9 |]
// Array.fill arrayFill1 1 9 ' '
// let parsedInput = lines |> Seq.take 3 |> List.ofSeq |> List.rev |> Seq.ofList
// go through lines in revrse
printfn "%A" (partOne "./example.txt" 3 3)
printfn "%A" (partOne "./input.txt" 8 9)

// printfn "%A" parsedInput

// TODO:
// - fill rows with empty
// - transpose input (flip x-y) to create stacks

// charArrToStr

// let x = [| ' '; ' '; ' '; ' ' |]
// let xs = Seq.map (fun x -> string x) x
// let xss = String.concat "" xs
// printfn "%A" x
// printfn "%A" xs
// printfn "hello: %s" xss
// printfn "hello: %s!" xss
// printfn "hello: %s!" (xss.Trim())
