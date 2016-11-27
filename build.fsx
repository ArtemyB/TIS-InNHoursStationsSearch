// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"

open Fake

// Directories

let scMachineDir = @"D:/Study/Univer/OSTIS/ostis/sc-machine"

let defaultBuildDir  = "./build/"
let deployDir = "./deploy/"

let buildDir = scMachineDir </> "bin/netextensions/debug"


// Filesets
let appReferences  =
    !! "/**/*.csproj"
    ++ "/**/*.fsproj"

// version info
let version = "0.1"  // or retrieve from CI server

// Targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; deployDir]
)

Target "Build" (fun _ ->
    // compile all projects below src/app/
    MSBuildDebug buildDir "Build" appReferences
    |> Log "AppBuild-Output: "
)

Target "Deploy" (fun _ ->
    !! (buildDir + "/**/*.*")
    -- "*.zip"
    |> Zip buildDir (deployDir + "ApplicationName." + version + ".zip")
)

// Build order
"Clean"
  ==> "Build"
  ==> "Deploy"

// start build
RunTargetOrDefault "Build"
