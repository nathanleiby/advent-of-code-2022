open System.IO

// outputs a tuple like ("U", 3)
let parseLine (line: string) =
    line.Split(" ") |> Array.ofSeq |> (fun x -> x[0], int x[1])

let parseFile f =
    f |> File.ReadAllLines |> Seq.map parseLine

exception InvalidDirection of string

let move h direction =
    match direction with
    | "U" -> (fst h, (snd h) + 1)
    | "D" -> (fst h, (snd h) - 1)
    | "L" -> (fst h - 1, (snd h))
    | "R" -> (fst h + 1, (snd h))
    | _ -> raise (InvalidDirection direction)

let follow h t =

    let h_x = fst h
    let h_y = snd h
    let t_x = fst t
    let t_y = snd t

    if (h_y = t_y || h_x = t_x) then
        // If the head is ever two steps directly up, down, left, or right from the tail,
        // the tail must also move one step in that direction so it remains close enough:
        let newX =
            if ((h_x - t_x) > 1) then (t_x + 1)
            elif ((t_x - h_x) > 1) then (t_x - 1)
            else t_x

        let newY =
            if ((h_y - t_y) > 1) then
                (t_y + 1)
            elif ((t_y - h_y) > 1) then
                (t_y - 1)

            else
                t_y

        (newX, newY)
    else
    // Otherwise, if the head and tail aren't touching and aren't in the same row or column,
    if
        ((abs (h_x - t_x) = 1) && (abs (h_y - t_y) = 1))
    then
        // touching diagonially
        (t_x, t_y)
    else

        // the tail always moves one step diagonally to keep up:
        let newX = if (h_x > t_x) then t_x + 1 else t_x - 1

        let newY = if (h_y > t_y) then t_y + 1 else t_y - 1

        (newX, newY)

let partOne f =
    let steps = parseFile f
    let mutable h = (0, 0)
    let mutable t = (0, 0)

    let mutable visited = Set.empty
    visited <- visited.Add(t) // add starting location

    // printfn "start: h = %A t = %A" h t

    for s in steps do
        let (direction, count) = s

        for i = 0 to (count - 1) do
            h <- move h direction
            t <- follow h t
            // printfn "direction = %A -> h = %A t = %A" direction h t
            visited <- visited.Add(t)

    // printfn "Visited = %A" visited
    Seq.length visited

printfn "== Part 1 =="
let example1 = partOne "./example.txt"
printfn "Example 1 = %d" example1
assert (example1 = 13)

let input1 = partOne "./input.txt"
printfn "Input 1 = %d" input1

// printfn "== Part 2 =="
// let example2 = partTwo "./example.txt"
// printfn "Example 1 = %d" example2
// assert (example2 = 13)

// let input2 = partTwo "./input.txt"
// printfn "Input 1 = %d" input2
