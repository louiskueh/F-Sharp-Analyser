module OperatorPrecedenceTests
open System.IO
open Expecto
open FSharp.Analyzers.SDK
open OperatorPrecedence
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
  testList "Operator Precedence test" [
    test "+ Case Infix test" {
      let input = """let result = 1 +3
    """
      // Generate Mock context for source code
      let inputStringArray = input.Split "\n"
      let checkProjectResults = parseAndCheckSingleFile(input)
      let typeTree = checkProjectResults.AssemblyContents.ImplementationFiles.[0]
      let tree = getUntypedTree(file, input) 
      let mockContext:Context = {FileName=""; Content=inputStringArray; ParseTree=tree; TypedTree= typeTree;Symbols=[] }

      let result = OperatorPrecedence mockContext
      printfn "Result is %A" result
      let expectedResult = "Try adding a space between + and 3"
      let Startposition = mkPos (1) 13
      let EndPosition = mkPos (1) 18
      let range = mkRange "" Startposition EndPosition
      // Expect.equal result.[0].Range 
      Expect.equal result.[0].Message expectedResult "Expected Message should match"
    }
    test "- Case Infix test" {
      let input = """let x = 1 -2
    """
      // Generate Mock context for source code
      let inputStringArray = input.Split "\n"
      let checkProjectResults = parseAndCheckSingleFile(input)
      let typeTree = checkProjectResults.AssemblyContents.ImplementationFiles.[0]
      let tree = getUntypedTree(file, input) 
      let mockContext:Context = {FileName=""; Content=inputStringArray; ParseTree=tree; TypedTree= typeTree;Symbols=[] }

      let result = OperatorPrecedence mockContext
      printfn "Result is %A" result
      let expectedResult = "Try adding a space between - and 2"
      let Startposition = mkPos (1) 13
      let EndPosition = mkPos (1) 18
      let range = mkRange "" Startposition EndPosition
      // Expect.equal result.[0].Range 
      Expect.equal result.[0].Message expectedResult "Expected Message should match"
    }


  ]
  |> testLabel "Operator Precedence Tests"


