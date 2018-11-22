usage_Dir="FSharp.Analyzers.Sample.Usage/packages/analyzers/Fsharp.Analyzers.Sample/"
cd FSharp.Analyzers.Sample
dotnet pack --output nupkgs
cp -r nupkgs/FSharp.Analyzers.Sample.1.0.1.nupkg ../${usage_Dir}