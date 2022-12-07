// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

open System.IO
open System.Collections.Generic

// learning from https://fsharpforfunandprofit.com/posts/recursive-types-and-folds-3b/


// type Tree<'LeafData, 'INodeData> =
//     | LeafNode of 'LeafData
//     | InternalNode of 'INodeData * Tree<'LeafData, 'INodeData> seq

// type FileInfo = { name: string; fileSize: int }
// type DirectoryInfo = { name: string }

// type FileSystemItem = Tree<FileInfo, DirectoryInfo>

// let fromFile (fileInfo: FileInfo) = LeafNode fileInfo

// let fromDir (dirInfo: DirectoryInfo) subitems = InternalNode(dirInfo, subitems)

// let readme = fromFile {name="readme.txt"; fileSize=1}
// let config = fromFile {name="config.xml"; fileSize=2}
// let build  = fromFile {name="build.bat"; fileSize=3}
// let src = fromDir {name="src"} [readme; config; build]
// let bin = fromDir {name="bin"} []
// let root = fromDir {name="root"} [src; bin]

type Action =
    | CD = 0
    | LS = 1
    | AddDir = 2
    | AddFile = 3
    | Unknown = -1

let parseLine (line: string) =
    match line with
    | line when line.StartsWith("$ cd") -> line.Split(" ") |> (fun x -> Action.CD, x[2])
    | line when line.StartsWith("$ ls") -> (Action.LS, "")
    | line when line.StartsWith("dir") -> line.Split(" ") |> (fun x -> Action.AddDir, x[1])
    | _ -> (Action.AddFile, line)
// | _ -> (Action.Unknown, line)

let curDirToStr curdir = (String.concat "/" (List.rev curdir))

let partOne f =
    let lines = File.ReadAllLines(f)

    let mutable curDir = []
    let mutable files = [] // path, size
    let dirs = new Dictionary<string, int>()

    for l in lines do
        let (action, value) = parseLine l
        // printfn "LINE = %s\n\t-> ACTION = %A , VALUE = %s" l action value

        if (action = Action.CD) then
            match value with
            | value when value.Equals("..") -> curDir <- curDir.Tail // pop a directory
            | value when value.Equals("/") -> curDir <- [ "/" ]
            | _ -> curDir <- List.append [ value ] curDir

        let cds = curDirToStr curDir

        // if dirs.ContainsKey(cds) then
        //     ignore true
        // else
        //     dirs.Add(cds, 0)

        if (action = Action.AddFile) then
            let split = value.Split(" ")
            let size = int split[0]
            let name = split[1]
            let fpathLst = (List.rev (List.append [ name ] curDir))
            let fpath = String.concat "/" fpathLst

            let f = (fpath, size)
            // printfn "\tAdding file = %A" f

            let curVal = dirs.GetValueOrDefault(cds)
            let newVal = curVal + size
            let _ = dirs.Remove(cds)
            dirs.Add(cds, newVal)
            printfn "cds = %s , f = %A, val = %d -> %d" cds f curVal newVal

            files <- List.append files [ f ]

    // printfn "\tcurdir = %A" curDir

    let mutable total = 0

    for d in dirs do
        let mutable dsize = 0
        printfn "dir = %s (size = %d)" d.Key d.Value

        for f, fsize in files do
            if f.StartsWith(d.Key) then
                printfn "\tfile = %s (size = %d)" f fsize
                dsize <- dsize + fsize

        printfn "  ... recursive size = %d" dsize

        if (dsize <= 100000) then
            total <- total + dsize
            printfn "  -> ADDED, new total = %d" total



    // // recursive directory size
    // for d in dirs do
    //     // get size of dir and all its subfolders
    //     let mutable currentTotal = 0

    //     for dd in dirs do
    //         if dd.Key.StartsWith(d.Key) then
    //             currentTotal <- currentTotal + dd.Value

    //     if (currentTotal <= 100000) then
    //         total <- total + currentTotal

    total


printfn "Part One:"
// printfn "example = %d" (partOne "./example.txt")
printfn "actual = %d" (partOne "./input.txt")
