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
  testList "Too Many Parameters test" [
    test "Default Case TooMany Parameters" {
      let input = """let add x y = x + y 
let result = add 1 2 3
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
      let result = IncorrectParameters mockContext
      let expectedResult = "For function add, which expects 2 arguments "
      Expect.equal result.[0].Message expectedResult "Should detect function name correctly and identify correct number of arguments "
    }
    test "Default Case Too Few Parameters No error" {
      let input = """let add x y = x + y 
let result = add 1
    """
      // get implementation file contents (typed tree)
      let checkProjectResults = parseAndCheckSingleFile(input)
      let typeTree = checkProjectResults.AssemblyContents.ImplementationFiles.[0]
      let inputStringArray = input.Split "\n"
      // printfn "TESTING "
      // for x in inputStringArray do
      //   printfn "string is %s" x
      // printfn "TESTING "
      // get untyped tree
      let tree = getUntypedTree(file, input) 
      // printfn "tree = %A" tree
      // mockIncorrectParamAnalyser tree
      let mockContext:Context = {FileName=""; Content=inputStringArray; ParseTree=tree; TypedTree= typeTree;Symbols=[] }
      let result = IncorrectParameters mockContext
      Expect.equal result.IsEmpty  true  "Should not have any output for correct code "

    }

  ]
  |> testLabel "Incorrect Parameter Tests"


