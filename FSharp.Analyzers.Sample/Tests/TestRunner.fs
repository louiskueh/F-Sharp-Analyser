module traverse
open Expecto
open Expecto.TestResults
[<EntryPoint>]
let main argv = 
    // Run tests
    // let writeResults = TestResults.writeNUnitSummary ("TestResults.xml", "Expecto.Tests")
    // let config = defaultConfig.appendSummaryHandler writeResults
    // runTestsInAssembly config argv
    runTestsInAssembly defaultConfig argv
