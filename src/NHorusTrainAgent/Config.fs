module Config

open Fake.EnvironmentHelper

let ScMachineFolder = @"D:\Study\Univer\OSTIS\ostis\sc-machine"

let ConfigFilePath = ScMachineFolder </> "bin/sc-memory.ini"
let RepoPath = ScMachineFolder </> "bin/repo"
let ExtensionPath = ScMachineFolder </> "bin/extensions"
let NetExtensionPath = ""