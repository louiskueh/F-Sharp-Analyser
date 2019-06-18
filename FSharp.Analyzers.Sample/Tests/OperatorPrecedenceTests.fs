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

let getResultFromInput (input:string) = 
  let inputStringArray = input.Split "\n"
  let checkProjectResults = parseAndCheckSingleFile(input)
  let typeTree = checkProjectResults.AssemblyContents.ImplementationFiles.[0]
  let tree = getUntypedTree(file, input) 
  let mockContext:Context = {FileName=""; Content=inputStringArray; ParseTree=tree; TypedTree= typeTree;Symbols=[] }
  let result = OperatorPrecedence mockContext
  result
[<Tests>]  
let tests =
  testList "Operator Precedence  tests" [
    test "Basic function call test" {
    let input = """let result = String.length "hello" + "world"
  """
    // Generate Mock context for source code
    let result = getResultFromInput input
    // printfn "result %A" result
    let expectedMessage = "The arguments for function \"String.length\" may need brackets to define arguments near character: \"+\""
    let Startposition = mkPos (1) 37
    let EndPosition = mkPos (1) 44
    let ExpectedRange = mkRange "" Startposition EndPosition

    let expectedMessage2 = "The arguments for function \"String.length\" may need brackets to define arguments near character: \"+\""
    let Startposition = mkPos (1) 35
    let EndPosition = mkPos (1) 36
    let ExpectedRange2 = mkRange "" Startposition EndPosition
    Expect.equal result.Length 2 "Expected 2 errors"
    Expect.equal result.[0].Message expectedMessage "Matching message"
    Expect.equal result.[0].Range ExpectedRange "Range matches"
    Expect.equal result.[1].Message expectedMessage2 "Matching Message"
    Expect.equal result.[1].Range  ExpectedRange2 "Matching range"
    }
    test "Basic pipe test" {
    let input = """let result = 42 + [1..10] |> List.sum 
  """
    // Generate Mock context for source code
    let result = getResultFromInput input
    let expectedMessage = "The arguments for function \"List.sum\" may need brackets to define arguments near character: \"+\""
    let Startposition = mkPos (1) 18
    let EndPosition = mkPos (1) 25
    let ExpectedRange = mkRange "" Startposition EndPosition

    let expectedMessage2 = "The arguments for function \"List.sum\" may need brackets to define arguments near character: \"+\""
    let Startposition = mkPos (1) 29
    let EndPosition = mkPos (1) 37
    let ExpectedRange2 = mkRange "" Startposition EndPosition
    Expect.equal result.Length 2 "Expected 2 errors"
    Expect.equal result.[0].Message expectedMessage "Matching message"
    Expect.equal result.[0].Range ExpectedRange "Range matches"
    Expect.equal result.[1].Message expectedMessage2 "Matching Message"
    Expect.equal result.[1].Range  ExpectedRange2 "Matching range"
    }

//     test "Multiple errors on a line" {
//     let input = """let result = 42 + [1..10] |> List.sum  +  String.length "hello" + "world"
//   """
//     // Generate Mock context for source code
//     let result = getResultFromInput input
//     printfn "result %A" result
//     let expectedMessage = "The arguments for function \"List.sum\" may need brackets to define arguments near character: \"+\""
//     let Startposition = mkPos (1) 18
//     let EndPosition = mkPos (1) 25
//     let ExpectedRange = mkRange "" Startposition EndPosition

//     let expectedMessage2 = "The arguments for function \"List.sum\" may need brackets to define arguments near character: \"+\""
//     let Startposition = mkPos (1) 29
//     let EndPosition = mkPos (1) 37
//     let ExpectedRange2 = mkRange "" Startposition EndPosition
//     // printfn "result %A" result
//     Expect.equal result.Length 2 "Expected 2 errors"
//     Expect.equal result.[0].Message expectedMessage "Matching message"
//     Expect.equal result.[0].Range ExpectedRange "Range matches"
//     Expect.equal result.[1].Message expectedMessage2 "Matching Message"
//     Expect.equal result.[1].Range  ExpectedRange2 "Matching range"
//     }

    test "Multiple errors on a line" {
    let input = """let result = 42 + [1..10] |> List.sum  +  String.length "hello" + "world"
  """
    // Generate Mock context for source code
    let result = getResultFromInput input
    printfn "result %A" result
    let expectedMessage = "The arguments for function \"List.sum\" may need brackets to define arguments near character: \"+\""
    let Startposition = mkPos (1) 18
    let EndPosition = mkPos (1) 25
    let ExpectedRange = mkRange "" Startposition EndPosition

    let expectedMessage2 = "The arguments for function \"List.sum\" may need brackets to define arguments near character: \"+\""
    let Startposition = mkPos (1) 29
    let EndPosition = mkPos (1) 37
    let ExpectedRange2 = mkRange "" Startposition EndPosition
    // printfn "result %A" result
    Expect.equal result.Length 2 "Expected 2 errors"
    Expect.equal result.[0].Message expectedMessage "Matching message"
    Expect.equal result.[0].Range ExpectedRange "Range matches"
    Expect.equal result.[1].Message expectedMessage2 "Matching Message"
    Expect.equal result.[1].Range  ExpectedRange2 "Matching range"
    }

  ]
  |> testLabel "Operator Precedence Tests"


