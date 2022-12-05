// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

open System.IO

let f = "./example.txt"
let lines = File.ReadAllLines(f)

module Stack =
    type 'a stack =
        | EmptyStack
        | StackNode of 'a * 'a stack

    let hd =
        function
        | EmptyStack -> failwith "Empty stack"
        | StackNode(hd, tl) -> hd

    let tl =
        function
        | EmptyStack -> failwith "Empty stack"
        | StackNode(hd, tl) -> tl

    let cons hd tl = StackNode(hd, tl)

    let empty = EmptyStack

let s = Stack.empty

// design a data structure for the stacks

// parse the initial state of the stacks
// - figure out approach for vertical -> horizontal

// parsew the instructions
// execute the instructions on the initial state

let mutable s1 = [ 'N'; 'Z' ]
let mutable s2 = [ 'D'; 'C'; 'M' ]
let mutable s3 = [ 'P' ]

let stacks = Array.ofList [ s1; s2; s3 ]

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

printfn "\nRunning...\n"
// for i in 1..quantity do
// printfn "%A" (move s1 s2)
// printfn "%A" (move s2 s1)
printfn "%A" (move 2 1)

printfn "%A" (move 1 3)
printfn "%A" (move 1 3)
printfn "%A" (move 1 3)

printfn "%A" (move 2 1)
printfn "%A" (move 2 1)
printfn "DONE!"
printfn "%A" (move 1 2)

let out = stacks |> Array.map List.head |> Array.map string |> String.concat ""
printfn "%A" out
