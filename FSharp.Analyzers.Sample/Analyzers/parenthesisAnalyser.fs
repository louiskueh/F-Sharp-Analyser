module ParenthesisAnalyser

open FSharp.Analyzers.SDK
// open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast


let isBalanced str previousStack lastLine= 
    let rec loop xs stack = 
        match (xs, stack) with
        // find (, add ( to stack
        | '(' :: ys,  stack -> loop ys ('(' :: stack)
        // find ), and (  is already on top
        | ')' :: ys, '(' :: stack -> loop ys stack
        // find ), and ( is not on top, therefore error
        | ')' :: _, stack -> (false,stack)
        // any other character loop
        | _ :: ys, stack -> loop ys stack
        // both empty then fine
        | [], [] -> (true,stack)
        // empty line and stack is non empty
        | [], stack -> 
          let mutable returnRes = (false,[])
          if (lastLine = true) then do
            returnRes <- (false,stack)
          else 
            returnRes <- (true,stack)
          returnRes
    loop (Seq.toList str) previousStack



[<Analyzer>]
let ParenthesisAnalyser : Analyzer =
    printfn "Inside Parenthesis analyzer!"
    
    fun ctx ->
      // printfn "RUNNING ANALYSER"
      let state = ResizeArray<range>()
      // printfn "ctx %A" ctx.Content
      let contents = ctx.Content
      let mutable trackStack =  []
      let mutable trackBalance = true
      
      for i in 0..contents.Length-1 do
        // printfn "Lenght of contents %d" contents.Length
        // printfn "i is %d" i
        if trackBalance = true then do 
        // printfn "trackBalance start %b %d" trackBalance 
            let mutable LastLine = false
            if (i = contents.Length-1) then do
              LastLine <- true
            let (balanced,tempStack)= isBalanced contents.[i] trackStack LastLine
            // printfn "Line Content %d %s is   " i contents.[i] 
            trackStack <- tempStack
            trackBalance <- balanced
            // if error on line, stack is non empty
            if balanced = false then do 
              // printfn "Found bracket error at line %d" i
              let Startposition = mkPos (i+1) 0
              let EndPosition = mkPos (i+1) contents.[i].Length
              let range = mkRange ctx.FileName Startposition EndPosition
              state.Add range
              // printfn "state is (Inside loop) %A" state
              // printfn "Added range %A" range
            // else  
              // printfn "state is (Outside loop) %A" state
              // printfn "Expression is balanced"
      // printfn "Finish loop"
      // if state.ToArray().Length > 0 then do
      //   printfn "state exists"
      // else  
      //   printfn "state doesn't exists"
      
      
      state
      |> Seq.map (fun r ->
          { Type = "Parenthesis Analyser"
            Message = "Possible bracket error"
            Code = "P001"
            Severity = Warning
            Range = r
            Fixes = []}
      )
      |> Seq.toList