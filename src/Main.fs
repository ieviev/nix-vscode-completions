module Main

open Fable.Core
open Fable.Import.VSCode.Vscode
open Microsoft.FSharp.Core
open App.Common
open Fable.Core.JsInterop
open System

let console = Fable.Core.JS.console
let vscode = Fable.Import.VSCode.vscode
module Vscode = Fable.Import.VSCode.Vscode

type NixosOption = {
    Name: string
    Desc: string
    Type: string
    Default: string
}

type NixosPackage = { Name: string; Version: string }

type Global = {
    mutable context: ExtensionContext
    mutable nixos_options: NixosOption[]
    mutable hm_options: NixosOption[]
    mutable packages: NixosPackage[]
}

let mutable ctx: Global = {
    context = null
    nixos_options = [||]
    hm_options = [||]
    packages = [||]
}

let queryNixOSOption (editor: TextEditor) (_: TextEditorEdit) (_: ResizeArray<obj option>) =
    promise {
        let options = ctx.nixos_options

        let! picked =
            options
            |> QuickPick.pickFromSeq (fun o ->
                jsOptions<QuickPickItem> (fun v ->
                    v.label <- o.Name
                    v.description <- Some $": {o.Type} [{o.Default}]"
                    v.detail <- Some o.Desc
                )
            )

        match picked with
        | None -> ()
        | Some option ->
            let defaultvalue =
                options |> Array.find (fun o -> o.Name = option.label) |> (fun o -> o.Default)

            let! success =
                (TextEditBuilder(editor) {
                    insert (editor.selection.active) $"{option.label} = {defaultvalue};"
                })

            ()
    }
    |> ignore


let queryHomeManagerOption
    (editor: TextEditor)
    (_: TextEditorEdit)
    (_: ResizeArray<obj option>)
    =
    promise {
        let options = ctx.hm_options

        let! picked =
            options
            |> QuickPick.pickFromSeq (fun o ->
                jsOptions<QuickPickItem> (fun v ->
                    v.label <- o.Name
                    v.description <- Some $": {o.Type} [{o.Default}]"
                    v.detail <- Some o.Desc
                )
            )

        match picked with
        | None -> ()
        | Some option ->
            let defaultvalue =
                options |> Array.find (fun o -> o.Name = option.label) |> (fun o -> o.Default)

            let! success =
                (TextEditBuilder(editor) {
                    insert (editor.selection.active) $"{option.label} = {defaultvalue};"
                })

            ()
    }
    |> ignore

let queryNixPackage (editor: TextEditor) (_: TextEditorEdit) (_: ResizeArray<obj option>) =
    promise {
        let options = ctx.packages

        let! picked =
            options
            |> QuickPick.pickFromSeq (fun o ->
                jsOptions<QuickPickItem> (fun v ->
                    v.label <- o.Name
                    v.description <- Some $": [{o.Version}]"
                )
            )

        match picked with
        | None -> ()
        | Some option ->
            let! success =
                (TextEditBuilder(editor) {
                    insert (editor.selection.active) $"{option.label}"
                })

            ()
    }
    |> ignore

let inline cmdName name = "nix-vscode-completions." + name

let activate (context: ExtensionContext) =
    ctx.context <- context
    let read_unbox name =
        JS.JSON.parse (Node.Api.fs.readFileSync (context.extensionPath + name, "utf8"))
        |> unbox

    ctx.nixos_options <- read_unbox "/data/nixos-options.json"
    ctx.hm_options <- read_unbox "/data/hm-options.json"
    ctx.packages <- read_unbox "/data/pkgs.json"
    console.log ($"loaded nixos options and packages")

    [
        commands.registerTextEditorCommand (cmdName "queryNixOSOption", queryNixOSOption)
        commands.registerTextEditorCommand (
            cmdName "queryHomeManagerOption",
            queryHomeManagerOption
        )
        commands.registerTextEditorCommand (cmdName "queryNixPackage", queryNixPackage)
    ]
    |> List.iter (unbox >> context.subscriptions.Add)
