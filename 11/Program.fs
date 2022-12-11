﻿// For more information see https://aka.ms/fsharp-console-apps
open System.IO

let DEBUG_SUMMARY = true
let DEBUG_EACH_STEP = false

// immutable approach to queue
// see: https://www.reddit.com/r/fsharp/comments/mygptg/help_on_creating_queue_in_f/
let enqueue x q = x :: q

let dequeue q =
    match (List.rev q) with
    | h :: t -> (h, List.rev t)
    | [] -> failwith "Empty list"

let parseMonkey (lines: seq<string>) =
    // Each monkey has several attributes:
    // Starting items lists your worry level for each item the monkey is currently holding in the order they will be inspected.
    // Operation shows how your worry level changes as that monkey inspects an item. (An operation like new = old * 5 means that your worry level after the monkey inspected the item is five times whatever your worry level was before inspection.)
    // Test shows how the monkey uses your worry level to decide where to throw an item next.
    // If true shows what happens with an item if the Test was true.
    // If false shows what happens with an item if the Test was false.

    // printfn "parseMonkey = %A" lines
    let lArray = Array.ofSeq lines

    // Monkey 0:
    let monkeyId = (Array.ofSeq lArray[0])[7] |> fun c -> c.ToString() |> int

    // Starting items: 79, 98
    let startItems =
        lArray[ 1 ].Split(": ")
        |> fun x -> x[1] |> fun x -> x.Split(", ") |> Seq.map int

    // Operation: new = old * 19
    let op = lArray[2][23]
    let opVal = lArray[2][25..]

    // Test: divisible by 23
    let parts = lArray[ 3 ].Split("Test: divisible by ")
    let divBy = parts[1] |> int

    //     If true: throw to monkey 2
    let trueDest = (Array.ofSeq lArray[4])[29] |> fun c -> c.ToString() |> int
    //     If false: throw to monkey 3
    let falseDest = (Array.ofSeq lArray[5])[30] |> fun c -> c.ToString() |> int

    (monkeyId, startItems, op, opVal, divBy, trueDest, falseDest)

let partOne f =
    let lines = File.ReadLines f

    // parse
    let monkeys = lines |> Seq.chunkBySize 7 |> Seq.map parseMonkey |> Array.ofSeq

    // track items; initialize from parsed input
    let mutable items = Array.create (Seq.length monkeys) []

    for idx, monkey in Seq.indexed monkeys do
        let _, startItems, _, _, _, _, _ = monkey
        items[idx] <- List.ofSeq startItems |> List.rev

    let mutable inspectCounts = Array.create (Seq.length monkeys) 0

    // execute rounds
    // let rounds = 1 // TODO: debugging
    let rounds = 20

    for round = 1 to rounds do
        if DEBUG_SUMMARY then
            printfn "** Round %d **" round

        // The monkeys take turns inspecting and throwing items.
        for m in monkeys do
            let monkeyId, _, op, opVal, divBy, trueDest, falseDest = m

            if DEBUG_EACH_STEP then
                printfn "Monkey %d:" monkeyId

            // NOTE: does a monkey ever throw to itself? if so this logic should be a `while dequeue ...` instead
            // -> it seems like NO in the input
            // NOTE: My queues are reversed. I don't _think_  it matters because monkeys process all their items
            for i = 1 to List.length items[monkeyId] do
                // inspect item
                let value, rest = dequeue items[monkeyId]

                if DEBUG_EACH_STEP then
                    printfn "  Monkey inspects an item with a worry level of %d." value

                inspectCounts[monkeyId] <- inspectCounts[monkeyId] + 1

                // Operation shows how your worry level changes as that monkey inspects an item. (An operation like new = old * 5 means that your worry level after the monkey inspected the item is five times whatever your worry level was before inspection.)
                let computedOpVal =
                    match opVal with
                    | "old" -> value // old value (`* old`)
                    | _ -> int opVal // some other number (`* 4`)

                let newValuePostOp =
                    match op with
                    | '*' ->
                        let out = value * computedOpVal

                        if DEBUG_EACH_STEP then
                            printfn "    Worry level is multiplied by %s to %d." opVal out

                        out
                    | '+' ->
                        // printfn "%d %s %d" value "+" computedOpVal
                        let out = value + computedOpVal

                        if DEBUG_EACH_STEP then
                            printfn "    Worry level increases by %s to %d." opVal out

                        out
                    | _ -> raise (System.ArgumentException "invalid math operation")


                // After each monkey inspects an item but before it tests your worry level,
                // your relief that the monkey's inspection didn't damage the item causes your worry level to be divided by three and rounded down to the nearest integer.
                let newValue = newValuePostOp / 3 // should already round down (floor), since item is an int

                if DEBUG_EACH_STEP then
                    printfn "    Monkey gets bored with item. Worry level is divided by 3 to %d." newValue

                let isDivBy = newValue % divBy = 0
                let destIdx = if isDivBy then trueDest else falseDest

                if DEBUG_EACH_STEP then
                    printfn "    Current worry level is%s divisible by %d." (if not isDivBy then " not" else "") divBy

                if DEBUG_EACH_STEP then
                    printfn "    Item with worry level %d is thrown to monkey %d." newValue destIdx

                items[monkeyId] <- rest // remove from current list
                items[destIdx] <- enqueue newValue items[destIdx] // add to new list

        if DEBUG_SUMMARY then
            // debugging: round end
            printfn "(round end)"

            for mIdx, _ in Seq.indexed monkeys do
                printfn "Monkey %d: %A" mIdx items[mIdx]

    // When a monkey throws an item to another monkey, the item goes on the end of the recipient monkey's list.
    // A monkey that starts a round with no items could end up inspecting and throwing many items by the time its turn comes around.
    // If a monkey is holding no items at the start of its turn, its turn ends.


    // if DEBUG then
    for mIdx, _ in Seq.indexed monkeys do
        printfn "Monkey %d inspected items %d times." mIdx inspectCounts[mIdx]

    // compute monkey business
    inspectCounts
    |> Array.sortDescending
    |> Seq.take 2
    |> Array.ofSeq
    |> (fun x -> x[0] * x[1])

// printfn "== Part One =="
let example1 = partOne "./example.txt"
printfn "example = %d" example1

let input1 = partOne "./input.txt"
printfn "input = %d" input1
