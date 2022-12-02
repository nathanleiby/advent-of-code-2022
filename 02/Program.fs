open System.IO

exception InvalidInput of string

let LOSS = 0
let DRAW = 3
let WIN = 6

let partOneScore one two =
    let action_score =
        match two with
        | "X" -> 1 // rock
        | "Y" -> 2 // paper
        | "Z" -> 3 // scissors
        | _ -> raise (InvalidInput($"invalid play: {two}"))

    let player_two_result =
        match (one, two) with
        | ("A", "X") -> DRAW
        | ("A", "Y") -> WIN
        | ("A", "Z") -> LOSS
        | ("B", "X") -> LOSS
        | ("B", "Y") -> DRAW
        | ("B", "Z") -> WIN
        | ("C", "X") -> WIN
        | ("C", "Y") -> LOSS
        | ("C", "Z") -> DRAW
        | _ -> raise (InvalidInput($"invalid pairing: {one} {two}"))

    action_score + player_two_result

let partTwoChooseAction one two =
    // the second column says how the round needs to end:
    // X means you need to lose,
    // Y means you need to end the round in a draw
    // Z means you need to win.

    match (one, two) with
    | ("A", "X") -> [ "A"; "Z" ] // lose
    | ("A", "Y") -> [ "A"; "X" ] // draw
    | ("A", "Z") -> [ "A"; "Y" ] // win
    | ("B", "X") -> [ "B"; "X" ] // lose
    | ("B", "Y") -> [ "B"; "Y" ] // draw
    | ("B", "Z") -> [ "B"; "Z" ] // win
    | ("C", "X") -> [ "C"; "Y" ] // lose
    | ("C", "Y") -> [ "C"; "Z" ] // draw
    | ("C", "Z") -> [ "C"; "X" ] // win
    | _ -> raise (InvalidInput($"invalid pairing: {one} {two}"))

let partOne file =
    File.ReadAllLines(file)
    |> Seq.ofArray
    |> Seq.map (fun x -> x.Split [| ' ' |])
    |> Seq.map (fun x -> partOneScore x[0] x[1])
    |> Seq.sum
    |> printfn "%d"

let partTwo file =
    File.ReadAllLines(file)
    |> Seq.ofArray
    |> Seq.map (fun x -> x.Split [| ' ' |])
    |> Seq.map (fun x -> partTwoChooseAction x[0] x[1])
    |> Seq.map (fun x -> partOneScore x[0] x[1])
    |> Seq.sum
    |> printfn "%d"

partOne "./example.txt"
partOne "./input.txt"
partTwo "./example.txt"
partTwo "./input.txt"
