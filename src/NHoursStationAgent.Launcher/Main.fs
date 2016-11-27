module Config =

    open Fake.EnvironmentHelper

    let ScMachineFolder = @"D:\Study\Univer\OSTIS\ostis\sc-machine"

    let ConfigFilePath = ScMachineFolder </> "bin/sc-memory.ini"
    let RepoPath = ScMachineFolder </> "bin/repo"
    let ExtensionPath = ScMachineFolder </> "bin/extensions"
    let NetExtensionPath = ""


open ScMemoryNet
open ScEngineNet.ScElements

let initializeScMemory () =
    let parameters = ScMemoryParams(true,
                        configFile = Config.ConfigFilePath,
                        repoPath = Config.RepoPath,
                        extensionsPath = Config.ExtensionPath,
                        netExtensionsPath = Config.NetExtensionPath)
    if (not ScMemory.IsInitialized)
        then ScMemory.Initialize(parameters)


[<EntryPoint>]
let main argv = 
    do initializeScMemory()
    use ctx = new ScMemoryContext(ScAccessLevels.MinLevel)
    0
