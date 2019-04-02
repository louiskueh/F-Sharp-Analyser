let parsenumExpr ls s = 
    Ok ((),s)
    |> ResExpr (fun _ e -> e)
    |> ResCheckDone
    |> Result.Bind (fun (_,exp) -> eval ls.SymTab expr
    |> Result.map snd
let calculate x y =
    x + y
// [<EntryPoint>]
// let main argv =
//     let x =
//     // printfn "Hello World from F#!s"
//     0 // return an integer exit code
    