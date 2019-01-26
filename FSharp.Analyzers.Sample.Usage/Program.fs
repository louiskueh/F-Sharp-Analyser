open System

let x = None

[<EntryPoint>]
let main argv =
    x.Value
    printfn "Hello World from F#!s"
    0 // return an integer exit code
