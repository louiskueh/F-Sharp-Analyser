module traverse
open Expecto

[<EntryPoint>]
let main argv =
    // Run tests
    let test = Tests.runTestsInAssembly defaultConfig argv
    0