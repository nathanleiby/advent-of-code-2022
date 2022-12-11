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

let printRope rope =
    for y = 10 downto -10 do
        for x = -10 to 10 do
            if (rope |> Seq.exists (fun pt -> pt = (x, y))) then
                printf "X"
            else
                printf "."

        printfn ""

let helper f ropeLength =
    let steps = parseFile f
    let mutable rope = Array.create ropeLength (0, 0)

    let tailIdx = (Array.length rope) - 1

    let mutable visited = Set.empty
    visited <- visited.Add(rope[tailIdx]) // add starting location

    for s in steps do
        let (direction, count) = s

        for i = 0 to (count - 1) do
            // printfn "direction = %A" direction

            rope[0] <- move rope[0] direction

            for ropeIdx = 0 to tailIdx - 1 do
                rope[ropeIdx + 1] <- follow rope[ropeIdx] rope[ropeIdx + 1]

            visited <- visited.Add(rope[tailIdx])

    // printRope rope
    // printfn ""

    // printfn "Visited = %A" visited
    Seq.length visited

let partOne f = helper f 2
let partTwo f = helper f 10

printfn "== Part 1 =="
let example1 = partOne "./example.txt"
printfn "Example= %d" example1
assert (example1 = 13)

let input1 = partOne "./input.txt"
printfn "Input = %d" input1

printfn "== Part 2 =="
let example2 = partTwo "./example.txt"
printfn "Example = %d" example2
assert (example2 = 1)

let example2larger = partTwo "./example-larger.txt"
printfn "Example (larger) = %d" example2larger
assert (example2larger = 36)

let input2 = partTwo "./input.txt"
printfn "Input = %d" input2
