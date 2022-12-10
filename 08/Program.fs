open System.IO

let parseLine line = line |> Seq.map int |> Array.ofSeq

let parseInput f =
    File.ReadAllLines f |> Seq.map parseLine |> Array.ofSeq

let isVisible (rowIdx: int, colIdx: int, matrix: int array array) =
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

let computeScenicScore (rowIdx: int, colIdx: int, matrix: int array array) =
    let height = matrix[rowIdx][colIdx]
    let matrixWidth = Array.length matrix[rowIdx]
    let matrixHeight = Array.length matrix

    let mutable isDone = false

    // left
    let mutable leftScore = 0
    isDone <- false

    for i = (colIdx - 1) downto 0 do
        if (not isDone) then
            leftScore <- leftScore + 1

        if (matrix[rowIdx][i] >= height) then
            isDone <- true

    // right
    let mutable rightScore = 0
    isDone <- false

    for i = (colIdx + 1) to (matrixWidth - 1) do
        if (not isDone) then
            rightScore <- rightScore + 1

        if (matrix[rowIdx][i] >= height) then
            isDone <- true

    // top
    let mutable topScore = 0
    isDone <- false

    for i = (rowIdx - 1) downto 0 do
        if (not isDone) then
            topScore <- topScore + 1

        if (matrix[i][colIdx] >= height) then
            isDone <- true

    // bot
    let mutable botScore = 0
    isDone <- false

    for i = (rowIdx + 1) to (matrixHeight - 1) do
        if (not isDone) then
            botScore <- botScore + 1

        if (matrix[i][colIdx] >= height) then
            isDone <- true

    leftScore * rightScore * topScore * botScore


let partTwo f =
    let matrix = parseInput f

    let mutable bestScore = 0

    for rowIdx, row in Array.indexed matrix do
        for colIdx, _ in Array.indexed row do
            let newScore = computeScenicScore (rowIdx, colIdx, matrix)

            if (newScore > bestScore) then
                bestScore <- newScore

    bestScore

printfn "== Part one =="
let example = partOne "./example.txt"
printfn "Example = %d" example
assert (example = 21)
let input = partOne "./input.txt"
printfn "Input = %d" input

printfn "== Part two =="
let example2 = partTwo "./example.txt"
printfn "Example = %d" example2
assert (example2 = 8)
let input2 = partTwo "./input.txt"
printfn "Input = %d" input2
