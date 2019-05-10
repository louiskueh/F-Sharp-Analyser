let parsenumExpr ls s = 
    Ok ((),s)
    |> ResExpr (fun _ e -> e)
    |> ResCheckDone
    |> Result.Bind (fun (_,exp) -> eval ls.SymTab expr
    |> Result.map snd
let calculate x y =
    x + y
// [1..10] |> List.append

