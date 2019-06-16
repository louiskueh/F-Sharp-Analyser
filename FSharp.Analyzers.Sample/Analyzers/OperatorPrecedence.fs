module OperatorPrecedence

open FSharp.Analyzers.SDK
// open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open System.IO
open Expecto.Logging

let checker = FSharpChecker.Create(keepAssemblyContents=true)
let parseAndCheckSingleFile (input) = 
    let file = Path.ChangeExtension(System.IO.Path.GetTempFileName(), "fsx")  
    File.WriteAllText(file, input)
    // Get context representing a stand-alone (script) file
    let projOptions, _errors = 
        checker.GetProjectOptionsFromScript(file, input)
        |> Async.RunSynchronously

    let fprojOptions = projOptions

    checker.ParseAndCheckProject (fprojOptions)
    |> Async.RunSynchronously

let checkPrefix str previousStack= 
    let rec loop xs stack = 
        match (xs, stack) with
        // find prefixes, add to stack
        | '+' :: ys,  stack -> loop ys ('+' :: stack)
        | '-' :: ys,  stack -> loop ys ('-' :: stack)
        // if we get a +, followed by a space, continue
        | ' ' :: ys, '+' :: stack -> loop ys stack
        | ' ':: ys, '-' :: stack -> loop ys stack
        // if there is no space between them possible error
        | c :: _, '+' :: stack -> (false,(c,'+'))
        | c :: _, '-' :: stack -> (false,(c,'-'))
        // any other character loop
        | _ :: ys, stack -> loop ys stack
        // both empty then fine
        | [], [] -> (true,(' ',' '))
        // empty, and possible continue to next line?
        | [], _ -> (false,(' ',' '))
    loop (Seq.toList str) previousStack

let checkPrefixSpacing (ctx:Context) (state:ResizeArray<(range *char * char)>) (error:FSharpErrorInfo) =
    printfn "Errors %A " error
    printfn "Error line: %A - %A " error.StartLineAlternate error.EndLineAlternate
    let contents = ctx.Content
    if error.Message = "This value is not a function and cannot be applied." then do 
        // check for prefixes here
        if error.StartLineAlternate = error.EndLineAlternate then do 
            let codeToCheck = contents.[error.StartLineAlternate - 1]
            let (result,(char,prefix)) = checkPrefix codeToCheck []
            if result = false then do 
                let Startposition = mkPos (error.StartLineAlternate) error.StartColumn
                let EndPosition = mkPos (error.EndLineAlternate) codeToCheck.Length
                let range = mkRange ctx.FileName Startposition EndPosition
                printfn "Result = %A, if false there is no space between + / - error! " result    
                state.Add (range,char,prefix)
                printfn "added to state"
           
                
[<Analyzer>]
let IncorrectParameters : Analyzer  =
    fun ctx ->
        printfn "ctxParseTree %A" ctx.ParseTree
        printfn "ctxTypedTree  %A" ctx.TypedTree 
        let state = ResizeArray<(range *char * char)>()
        let string = ctx.Content |> String.concat "\n"
        let checkProjectResults = parseAndCheckSingleFile(string)
        // printfn "Errors: %A" checkProjectResults.Errors
        let mutable FunctionName = ""
        
        if checkProjectResults.Errors.Length > 0 then
            checkProjectResults.Errors |> Array.iter (fun error -> checkPrefixSpacing ctx state error ) 
               
                    
        printfn "State is %A" state

        state
        |> Seq.map (fun (range,char,prefix) ->
            { Type = "Possibly no spacing between prefixes! \n"
              Message = "Try adding a space between " +  prefix.ToString()  + " and " + char.ToString()
              Code = "P001"
              Severity = Warning
              Range = range
              Fixes = []}

        )
        |> Seq.toList