module Tests
open System.IO
open Expecto
open FSharp.Analyzers.SDK
open IncorrectParameters
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
  testList "TooManyArgs test" [
    test "paranthesis" {
      let input = """let add x y = x + y 
let result = add 1 2 3
    """
      // get implementation file contents (typed tree)
      let checkProjectResults = parseAndCheckSingleFile(input)
      let typeTree = checkProjectResults.AssemblyContents.ImplementationFiles.[0]

      // get untyped tree
      let tree = getUntypedTree(file, input) 
      printfn "tree = %A" tree
      // mockIncorrectParamAnalyser tree
      let mockContext:Context = {FileName=""; Content=null; ParseTree=tree; TypedTree= typeTree;Symbols=[] }
      let result = IncorrectParameters mockContext
      printfn "%A" result
      let expectedResult = "For function add, which expects 2 arguments "
      Expect.equal result.[0].Message expectedResult "Should detect function name correctly and identify correct number of arguments "

    }

  ]
  |> testLabel "Typography tests"


