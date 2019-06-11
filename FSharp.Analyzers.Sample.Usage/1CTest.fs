// let add x y = x + y 
// let result = add 1 2 3

// let op_addition x  = x 

// let result = 1 + 2 
let parsenumExpr ls s = 
    Ok ((),s)
    |> ResExpr (fun _ e -> e)
    |> ResCheckDone
    |> Result.Bind fun (_,exp)) -> eval ls.SymTab expr
    |> Result.map snd

let calculate x y =
    x + y 