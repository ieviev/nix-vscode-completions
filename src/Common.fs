[<Microsoft.FSharp.Core.AutoOpen>]
module App.Common

open Fable.Core
open Fable.Import.VSCode
open Fable.Import.VSCode.Vscode
open Node.ChildProcess

module Msg =
    let info msg =
        window.showInformationMessage msg |> ignore

    let error msg = window.showErrorMessage msg |> ignore

type IExecOptions =
    abstract input: string option with get, set
    abstract cwd: string option with get, set
    abstract encoding: string option with get, set
    abstract timeout: int option with get, set

type QuickPickItemImpl(label, desc) =
    interface QuickPickItem with
        member val alwaysShow = None with get, set
        member val buttons = None with get, set
        member val description = Some desc with get, set
        member val detail = None with get, set
        member val kind = None with get, set
        member val label = label with get, set
        member val picked = None with get, set


[<RequireQualifiedAccess>]
module Promise =
    let ofThenable (th: Thenable<'t>) =
        Promise.create (fun resolve reject ->
            th.``then`` (
                (fun f -> resolve f |> U2.Case1),
                (fun f -> reject (Failure $"%A{f}"))
            )
            |> ignore
        )

type Thenable<'t> with
    member self.toPromise() =
        Promise.create (fun resolve reject ->
            self.``then`` (
                (fun f -> resolve f |> U2.Case1),
                (fun f -> reject (Failure $"%A{f}"))
            )
            |> ignore
        )


module Common =

    let getWorkspaceRoot () =
        workspace.workspaceFolders
        |> Option.defaultWith (fun _ -> failwith "no workspace set")
        |> Seq.head
        |> (fun f -> f.uri.fsPath)



module Range =
    let ofSelection (s: Selection) = vscode.Range.Create(s.start, s.``end``)


type TextEditBuilder(editor: TextEditor) =
    let functions = ResizeArray<TextEditorEdit -> unit>()
    member this.Yield _ = ()

    member this.Run _ = // ran in the end of the computation expr
        editor.edit (fun editBuilder -> functions |> Seq.iter (fun f -> f editBuilder))
        |> Promise.ofThenable


    [<CustomOperation("deleteRange")>]

    member this.DeleteRange(state, m: Range) =
        state
        functions.Add(fun f -> f.delete (U2.Case1 m))

    [<CustomOperation("deleteSelection")>]
    member this.DeleteSelection(_, m: Selection) =
        functions.Add(fun f -> f.delete (U2.Case2 m))

    [<CustomOperation("insert")>]
    member this.Insert(_, pos: Position, value: string) =
        functions.Add(fun f -> f.insert (pos, value))


module Position =
    let inline lineStart (lineIndex: float) = vscode.Position.Create(lineIndex, 0.)

    let inline withCharPos (charPos: float) (pos: Vscode.Position) =
        pos.``with`` (character = charPos)

module Cursor =
    let moveToLineEnd (editor: TextEditor, lineIndex: float, endCharPos: float) =
        let newPosition =
            Position.lineStart (lineIndex) |> Position.withCharPos (endCharPos)

        editor.selection <-
            vscode.Selection.Create(
                newPosition,
                newPosition.``with`` (character = newPosition.character - 1.)
            )

module Editor =
    let inline selectOffset (editor: TextEditor, offsetStart: float, offsetEnd: float) =
        let startPos = editor.document.positionAt (offsetStart)
        let endPos = editor.document.positionAt (offsetEnd)
        let selection = vscode.Selection.Create(startPos, endPos)
        // printfn $"selecting: {selection}"
        editor.revealRange (selection)
        editor.selection <- selection

module Path =
    let inline combine (paths) = Node.Api.path.join (paths)

    let inline workspaceRoot () =
        workspace.workspaceFolders
        |> Option.bind (fun folders -> folders |> Seq.tryHead)
        |> Option.map (fun v -> v.uri.fsPath)

module QuickPick =
    let pickFromSeq
        (fn: 't -> #QuickPickItem)
        (data: 't seq)
        : JS.Promise<QuickPickItem option> =
        Promise.create (fun resolve reject ->
            if Seq.isEmpty data then
                window.showErrorMessage "no options were found" |> ignore
            else
                data
                |> Seq.map fn
                |> Seq.map (fun f -> f :> QuickPickItem)
                |> (ResizeArray >> U2.Case1)
                |> window.showQuickPick
                |> (fun f ->
                    f.``then`` (
                        (fun f -> resolve f |> U2.Case1),
                        (fun f -> reject (Failure $"%A{f}"))
                    )
                    |> ignore
                )
        )
