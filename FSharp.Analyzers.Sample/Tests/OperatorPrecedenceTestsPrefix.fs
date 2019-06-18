module OperatorPrecedencePrefixTests
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
  testList "Operator Precedence prefix tests" [
    test "Error + Case Infix test" {
      let input = """let result = 1 +3
    """
      let result = getResultFromInput input
      // printfn "Result is %A" result
      let expectedResult = "Try adding a space between + and 3"
      let Startposition = mkPos (1) 13
      let EndPosition = mkPos (1) 18
      let range = mkRange "" Startposition EndPosition
      // Expect.equal result.[0].Range 
      Expect.equal result.[0].Message expectedResult "Expected Message should match"
    }
    test "No error + Case Infix test" {
      let input = """let result = 1 + 3
    """
      // Generate Mock context for source code
      let result = getResultFromInput input

      // Expect.equal result.[0].Range 
      Expect.equal result  [] "No expected match for false positives"
    }
    test "Error Case Infix test" {
      let input = """let x = 1 -2
    """
      // Generate Mock context for source code
      let result = getResultFromInput input
      let expectedResult = "Try adding a space between - and 2"
      let Startposition = mkPos (1) 13
      let EndPosition = mkPos (1) 18
      let range = mkRange "" Startposition EndPosition
      // Expect.equal result.[0].Range 
      Expect.equal result.[0].Message expectedResult "Expected Message should match"
    }
    test "No error - Case Infix test" {
      let input = """let x = 1 - 2
    """
      // Generate Mock context for source code
      let result = getResultFromInput input
      Expect.equal result  [] "No expected match for false positives"
    }

    test " No Error + Infix test" {
      let input = """let x = 1 - -2
    """
      // Generate Mock context for source code
      let result = getResultFromInput input
      Expect.equal result  [] "No expected match for false positives"
    }
    test " No Error +- Infix test" {
      let input = """let x = 1 - +2
    """
      // Generate Mock context for source code
      let result = getResultFromInput input
      Expect.equal result  [] "No expected match for false positives"
    }
    test " No Error -+ Infix test" {
      let input = """let x = 1 - +2
    """
      // Generate Mock context for source code
      let result = getResultFromInput input
      Expect.equal result  [] "Matching message"
    }
    test "Two errors on same line" {
      let input = """let x = 1 -2 -3
    """
      // Generate Mock context for source code
      let result = getResultFromInput input
      printfn "result %A" result
      Expect.equal result  [] "Matching message"
    }
    test "3 errors on same line" {
      let input = """let x = 1 -2 - 3 +2
    """
      // Generate Mock context for source code
      let result = getResultFromInput input
      // printfn "result %A" result
      Expect.equal result  [] "Matching message"
    }
    test "Error across multiple lines " {
      let input = """let x = 1 \n
      -2
    """
      // Generate Mock context for source code
      let result = getResultFromInput input
      // printfn "result %A" result
      Expect.equal result  [] "Matching message"
    }
    test "Error in nested expression " {
      let input = """let x = ( 1 + 5) * (6 * + 5 / 2 ) + 2 - 3 * 55 -3
    """
      // Generate Mock context for source code
      let result = getResultFromInput input
      // printfn "result %A" result
      let expectedMessage = "Try adding a space between - and 3"
      let Startposition = mkPos (1) 44
      let EndPosition = mkPos (1) 50
      let ExpectedRange = mkRange "" Startposition EndPosition
      Expect.equal result.[0].Message expectedMessage "Matching message"
      Expect.equal result.[0].Range ExpectedRange "Range matches"
    }
    test "Multiple errors accross multiple lines " {
      let input = """let x = 1 +3 
      let y = 2 +2
    """
      // Generate Mock context for source code
      let result = getResultFromInput input
      // printfn "result %A" result
      let expectedMessage = "Try adding a space between + and 3"
      let Startposition = mkPos (1) 8
      let EndPosition = mkPos (1) 14
      let ExpectedRange = mkRange "" Startposition EndPosition

      let Startposition = mkPos (2) 14
      let EndPosition = mkPos (2) 19
      let ExpectedRange2 = mkRange "" Startposition EndPosition
     
      Expect.equal result.Length 2 "Expected 2 errors"
      Expect.equal result.[0].Message expectedMessage "Matching message"
      Expect.equal result.[0].Range ExpectedRange "Range matches"
      Expect.equal result.[1].Message "Try adding a space between + and 2" "Matching Message"
      Expect.equal result.[1].Range  ExpectedRange2 "Matching range"
    }
    test "Using functions with incorrect code declarations" {
    let input = """let x = 1 +3 
    let customFunction = x + y
    let y = 2 +2
  """
    // Generate Mock context for source code
    let result = getResultFromInput input
    // printfn "result %A" result
    let expectedMessage = "Try adding a space between + and 3"
    let Startposition = mkPos (1) 8
    let EndPosition = mkPos (1) 14
    let ExpectedRange = mkRange "" Startposition EndPosition

    let Startposition = mkPos (2) 14
    let EndPosition = mkPos (2) 19
    let ExpectedRange2 = mkRange "" Startposition EndPosition
    // printfn "result %A" result
    Expect.equal result.Length 2 "Expected 2 errors"
    Expect.equal result.[0].Message expectedMessage "Matching message"
    Expect.equal result.[0].Range ExpectedRange "Range matches"
    Expect.equal result.[1].Message "Try adding a space between + and 2" "Matching Message"
    Expect.equal result.[1].Range  ExpectedRange2 "Matching range"
    }
    test "Correct simple code in between errors" {
    let input = """let x = 1 +3 
    let customFunction = 4 + 5
    let y = 2 +2
  """
    // Generate Mock context for source code
    let result = getResultFromInput input
    // printfn "result %A" result
    let expectedMessage = "Try adding a space between + and 3"
    let Startposition = mkPos (1) 8
    let EndPosition = mkPos (1) 14
    let ExpectedRange = mkRange "" Startposition EndPosition

    let Startposition = mkPos (2) 14
    let EndPosition = mkPos (2) 19
    let ExpectedRange2 = mkRange "" Startposition EndPosition
    printfn "result simple code %A" result
    Expect.equal result.Length 2 "Expected 2 errors"
    Expect.equal result.[0].Message expectedMessage "Matching message"
    Expect.equal result.[0].Range ExpectedRange "Range matches"
    Expect.equal result.[1].Message "Try adding a space between + and 2" "Matching Message"
    Expect.equal result.[1].Range  ExpectedRange2 "Matching range"
    }
    test "Function call code with addition in between errors" {
    let input = """let x = 1 +3 
    let customFunction = [1;2;3;4] |> List.map (fun x -> x + 1)
    let y = 2 +2
  """
    // Generate Mock context for source code
    let result = getResultFromInput input
    // printfn "result %A" result
    let expectedMessage = "Try adding a space between + and 3"
    let Startposition = mkPos (1) 8
    let EndPosition = mkPos (1) 14
    let ExpectedRange = mkRange "" Startposition EndPosition

    let Startposition = mkPos (2) 14
    let EndPosition = mkPos (2) 19
    let ExpectedRange2 = mkRange "" Startposition EndPosition
    // printfn "result %A" result
    Expect.equal result.Length 2 "Expected 2 errors"
    Expect.equal result.[0].Message expectedMessage "Matching message"
    Expect.equal result.[0].Range ExpectedRange "Range matches"
    Expect.equal result.[1].Message "Try adding a space between + and 2" "Matching Message"
    Expect.equal result.[1].Range  ExpectedRange2 "Matching range"
    }


  ]
  |> testLabel "Operator Precedence Tests"


