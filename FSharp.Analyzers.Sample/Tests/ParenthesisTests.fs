module ParenthesisTests
open System.IO
open Expecto
open FSharp.Analyzers.SDK
open ParenthesisAnalyser
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range



let checker = FSharpChecker.Create(keepAssemblyContents=true)
/// Get untyped tree for a specified input
let getUntypedTree(file, input) = 
    // Get compiler options for the 'project' implied by a single script file
    let projectOptions, _errors = 
        checker.GetProjectOptionsFromScript(file, input) 
        |> Async.RunSynchronously
    let parsingOptions, _errors = 
        checker.GetParsingOptionsFromProjectOptions(projectOptions)
    // Run the first phase (untyped parsing) of the compiler
    let parseFileResults = 
        checker.ParseFile(file, input, parsingOptions) |> Async.RunSynchronously
    match parseFileResults.ParseTree with
    | Some tree -> tree
    | None -> failwith "Something went wrong during parsing!"

let file = "/home/user/Test.fsx"
//get typed tree
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

[<Tests>]  
let tests =
  testList "Parenthesis Tests" [
    test "Complex case parameter missing" {
      let input = """let parsenumExpr ls s =
  Ok ((),s)
  |> ResExpr (fun _ e -> e)
  |> ResCheckDone
  |> Result.Bind fun ((_,exp) ->
    eval ls.SymTab expr
  |> Result.map snd 
let hello = 1
    """

      let inputStringArray = input.Split "\n"
      // get implementation file contents (typed tree)
      let checkProjectResults = parseAndCheckSingleFile(input)
      let typeTree = checkProjectResults.AssemblyContents.ImplementationFiles.[0]

      // get untyped tree
      let tree = getUntypedTree(file, input) 
      // printfn "tree = %A" tree
      // mockIncorrectParamAnalyser tree
      let mockContext:Context = {FileName=""; Content=inputStringArray; ParseTree=tree; TypedTree= typeTree;Symbols=[] }
      let result = ParenthesisAnalyser mockContext
      let expectedResult = "Possible bracket error"
      Expect.equal result.[0].Message expectedResult "Possible bracket error"
    }
    test "No error should have no error messages" {
      let input = """let parsenumExpr ls s =
  Ok ((),s)
  |> ResExpr (fun _ e -> e)
  |> ResCheckDone
  |> Result.Bind fun ((_,exp)) ->
    eval ls.SymTab expr
  |> Result.map snd 
let hello = 1
    """

      let inputStringArray = input.Split "\n"
      // get implementation file contents (typed tree)
      let checkProjectResults = parseAndCheckSingleFile(input)
      let typeTree = checkProjectResults.AssemblyContents.ImplementationFiles.[0]

      // get untyped tree
      let tree = getUntypedTree(file, input) 
      // printfn "tree = %A" tree
      // mockIncorrectParamAnalyser tree
      let mockContext:Context = {FileName=""; Content=inputStringArray; ParseTree=tree; TypedTree= typeTree;Symbols=[] }
      let result = ParenthesisAnalyser mockContext
      let expectedResult = "Possible bracket error"
      Expect.equal result.Length 0  "No errors"
    }
    test "Bracket error should be detected accross multiple lines" {
      // currently detects error correctly as only considering one line
      let input = """let parsenumExpr ls s =
  Ok ((),s)
  |> ResExpr (fun _ e -> 
  e
  |> ResCheckDone
  |> Result.Bind fun ((_,exp)) ->
    eval ls.SymTab expr
  |> Result.map snd 
let hello = 1
    """

      let inputStringArray = input.Split "\n"
      // get implementation file contents (typed tree)
      let checkProjectResults = parseAndCheckSingleFile(input)
      let typeTree = checkProjectResults.AssemblyContents.ImplementationFiles.[0]

      // get untyped tree
      let tree = getUntypedTree(file, input) 
      // printfn "tree = %A" tree
      // mockIncorrectParamAnalyser tree
      let mockContext:Context = {FileName=""; Content=inputStringArray; ParseTree=tree; TypedTree= typeTree;Symbols=[] }
      let result = ParenthesisAnalyser mockContext
      let expectedResult = "Possible bracket error"
      Expect.equal result.Length 1  "Error detected "
    }
    test "Bracket error should not be detected if valid throughout program" {
      let input = """let main argv =  
    printfn "%i" ((fun (x:int) (y:int) ->
        printfn "%s" "Calling an anonymous function!!!"
        printfn "You have called this anonymous function with %i and %i" x y
        x + y) 5 9)
    """
      let inputStringArray = input.Split "\n"
      // get implementation file contents (typed tree)
      let checkProjectResults = parseAndCheckSingleFile(input)
      let typeTree = checkProjectResults.AssemblyContents.ImplementationFiles.[0]

      // get untyped tree
      let tree = getUntypedTree(file, input) 
      // printfn "tree = %A" tree
      // mockIncorrectParamAnalyser tree
      let mockContext:Context = {FileName=""; Content=inputStringArray; ParseTree=tree; TypedTree= typeTree;Symbols=[] }
      let result = ParenthesisAnalyser mockContext
      let expectedResult = "Possible bracket error"
      Expect.equal result.Length 0  "No errors"
    }
  ]
  |> testLabel "Parenthesis Tests"


