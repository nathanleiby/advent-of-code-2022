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
let f = "./example.txt"
let lines = File.ReadAllLines(f)

// design a data structure for the stacks

// parse the initial state of the stacks
// - figure out approach for vertical -> horizontal

// parsew the instructions
// execute the instructions on the initial state
let example =
    // create the stacks
    let mutable s1 = []
    s1 <- List.append s1 [ 'N' ]
    s1 <- List.append s1 [ 'Z' ]

    let mutable s1 = [ 'N'; 'Z' ]
    let mutable s2 = [ 'D'; 'C'; 'M' ]
    let mutable s3 = [ 'P' ]

    // create the the list of stacks (whole "cargo" representati)
    let mutable stacksList = []
    stacksList <- List.append stacksList [ s1 ]
    stacksList <- List.append stacksList [ s2 ]
    stacksList <- List.append stacksList [ s3 ]

    let stacks = Array.ofList stacksList

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
                printfn "%A" (move fromId toId)


    let partOne =
        printfn "\nRunning...\n"

        let instructions = [ (1, 2, 1); (3, 1, 3); (2, 2, 1); (1, 1, 2) ]
        run instructions

        let out = stacks |> Array.map List.head |> Array.map string |> String.concat ""
        printfn "%A" out
        assert (out = "CMZ")

    partOne

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

// TODO: Seq.take controls how many lines are considered as "cargo"
// -> maybe.. takeWhile  line != "" and omit last line with numbers


let parseInput input =
    let parsedInput =
        input |> Seq.take 3 |> Seq.map parseLine |> List.ofSeq |> List.rev |> Seq.ofList

    // let mutable s1 = [ 'N'; 'Z' ]
    // let mutable s2 = [ 'D'; 'C'; 'M' ]
    // let mutable s3 = [ 'P' ]
    // let mutable stacks2 = []

    printfn "%A" parsedInput

    let mutable stacksList =
        // Array.init (List.length (List.ofSeq (Seq.take 1 parsedInput))) (fun x -> [])
        // TODO: Example only
        Array.init 3 (fun x -> [])

    printfn "Stacks List = %A" stacksList
    // let mutable stack = []
    // stacksList <- List.append stacksList [ stack ]

    for (rowIdx, row) in Seq.indexed parsedInput do
        for (colIdx, colVal) in Seq.indexed row do
            // if (rowIdx = 0) then

            if not (colVal.Equals("")) then
                printfn "colIdx = %d, colVal = %s" colIdx colVal
                let mutable stack = stacksList[colIdx]
                stack <- List.append stack [ colVal ]
                stacksList[colIdx] <- stack
                printfn "stack = %A" stack

            printfn "Stacks List = %A" stacksList


    stacksList

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
printfn "hello"
printfn "%A" (parseInput lines)
// printfn "%A" parsedInput
printfn "bye"

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
