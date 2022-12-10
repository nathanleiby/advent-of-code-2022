open System.IO

let parseLine line = line |> Seq.map int |> Array.ofSeq

let parseInput f =
    File.ReadAllLines f |> Seq.map parseLine |> Array.ofSeq

let isVisible (rowIdx: int, colIdx: int, matrix: int array array) =
    // from left?

    let height = matrix[rowIdx][colIdx]
    let matrixWidth = Array.length matrix[rowIdx]
    let matrixHeight = Array.length matrix

    let mutable visibleFromLeft = true

    for i = 0 to (colIdx - 1) do
        if (matrix[rowIdx][i] >= height) then
            visibleFromLeft <- false

    let mutable visibleFromRight = true

    for i = (colIdx + 1) to (matrixWidth - 1) do
        if (matrix[rowIdx][i] >= height) then
            visibleFromRight <- false

    let mutable visibleFromTop = true

    for i = 0 to (rowIdx - 1) do
        if (matrix[i][colIdx] >= height) then
            visibleFromTop <- false

    let mutable visibleFromBottom = true

    for i = (rowIdx + 1) to (matrixHeight - 1) do
        if (matrix[i][colIdx] >= height) then
            visibleFromBottom <- false

    visibleFromLeft || visibleFromRight || visibleFromTop || visibleFromBottom

let partOne f =
    let matrix = parseInput f

    let mutable total = 0

    // TODO: enumerate?
    for rowIdx, row in Array.indexed matrix do
        for colIdx, _ in Array.indexed row do
            if (isVisible (rowIdx, colIdx, matrix)) then
                total <- total + 1

    total

printfn "== Part one =="
let example = partOne "./example.txt"
printfn "Example = %d" example
assert (example = 21)
let input = partOne "./input.txt"
printfn "Input = %d" input
