usage_Dir="FSharp.Analyzers.Sample.Usage/packages/analyzers/Fsharp.Analyzers.Sample/"
packageName="FSharp.Analyzers.Sample.1.0.2.nupkg"
dotnet pack FSharp.Analyzers.Sample.fsproj --output nupkgs 
echo "Compiled nupkg"
cp -fr nupkgs/${packageName} ../${usage_Dir}
echo "Copied nupkg to usage directory"
cd ..
cd FSharp.Analyzers.Sample.Usage/packages/analyzers/FSharp.Analyzers.Sample/
unzip -o ${packageName}
echo "Successfully unzipped and overwritten new files"