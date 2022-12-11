// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

open System.IO

let partOne f =
    let lines = File.ReadLines f

    let mutable cycle = 1
    let mutable total = 0
    let mutable registerX = 1

    let output = Array.create 240 "."

    // addx V takes two cycles to complete. After two cycles, the X register is increased by the value V. (V can be negative.)
    // noop takes one cycle to complete. It has no other effect.


    let incrCycle count =
        for i = 1 to count do
            // printfn "incrCycle"
            cycle <- cycle + 1

            // 3-char wide sprite
            let xPos = cycle % 40

            if (xPos = registerX || xPos - 1 = registerX || xPos - 2 = registerX) then
                output[cycle] <- "#"

            if
                (cycle = 20
                 || cycle = 60
                 || cycle = 100
                 || cycle = 140
                 || cycle = 180
                 || cycle = 220)
            then
                total <- total + (registerX * cycle)

    // printfn "Cycle = %d , RegisterX = %d" cycle registerX

    // printfn "Cycle = %d , RegisterX = %d" cycle registerX

    for l in lines do
        // printfn "Executing %s ..." l

        match l with
        | "noop" -> incrCycle 1
        | _ ->
            l.Split(" ")
            |> (fun x ->
                incrCycle 1
                registerX <- registerX + (int x[1])
                incrCycle 1)

    // printfn "Cycle = %d , RegisterX = %d" cycle registerX

    // print screen
    printfn "== SCREEN (start) =="

    for y = 0 to 5 do
        for x = 0 to 39 do
            printf "%s" output[40 * y + x]

        printf "\n"

    printfn "== SCREEN (end) =="

    total

printfn "== Part One == "
let example1 = partOne "./example.txt"
printfn "Example = %d" example1
assert (example1 = 13140)
printfn "Input = %d" (partOne "./input.txt")

// printfn "== Part One == "
// printfn "Example = %d" (partOne "./example.txt")
// printfn "Input = %d" (partOne "./example.txt")


// Find the signal strength during the 20th, 60th, 100th, 140th, 180th, and 220th cycles. What is the sum of these six signal strengths?
