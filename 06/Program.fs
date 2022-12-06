open System.IO

// examples
// bvwbjplbgvbhsrlpgdmjqwftvncz: first marker after character 5
// nppdvjthqldpwncqszvftbrmjlhg: first marker after character 6
// nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg: first marker after character 10
// zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw: first marker after character 11

let helper s len =
    let idxd = s |> Seq.windowed len |> Seq.indexed


    let mutable result = -1
    let mutable found = false

    for (idx, item) in idxd do

        let s = Set.ofArray item

        if (s.Count = len && not found) then
            result <- (idx + len)
            found <- true

    // TODO: how to break early; idiomatic F# preferred

    printfn "result = %d" result
    result

let partOne s = helper s 4
let partTwo s = helper s 14

// examples
assert (partOne "bvwbjplbgvbhsrlpgdmjqwftvncz" = 5)
assert (partOne "nppdvjthqldpwncqszvftbrmjlhg" = 6)
assert (partOne "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" = 10)
assert (partOne "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" = 11)

let input = File.ReadAllLines("./input.txt") |> Seq.concat
printfn "part one = %d" (partOne input)

assert (partTwo "mjqjpqmgbljsphdztnvjfqwrcgsmlb" = 19)
assert (partTwo "bvwbjplbgvbhsrlpgdmjqwftvncz" = 23)
assert (partTwo "nppdvjthqldpwncqszvftbrmjlhg" = 23)
assert (partTwo "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" = 29)
assert (partTwo "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" = 26)

printfn "part two = %d" (partTwo input)
