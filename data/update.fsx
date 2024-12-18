
#r "nuget: resharp, 0.1.33"
open System.IO
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let hc = new System.Net.Http.HttpClient()
let nixos_options = "https://nixos.org/manual/nixos/unstable/options.html"
let hm_options = "https://nix-community.github.io/home-manager/options.xhtml"

let wget (url: string) (out: string) =
    task {
        let! html = hc.GetByteArrayAsync(url)
        System.IO.File.WriteAllBytes(out, html)
    }

// let query_htmls =
//     task {
//         do! wget nixos_options "nixos_options.html"
//         do! wget hm_options "hm_options.html"
//     }
//     |> (fun v -> v.Result)


let nixos_options_text = File.ReadAllText "nixos_options.html"

let reg pat =
    Resharp.Regex(pat, Resharp.ResharpOptions.HighThroughputDefaults)

let between left body right = $"(?<={left}){body}(?={right})"

let r_entry = reg "<dt>~(_*<dt>_*)</dd>"
let r_entry_name = reg (between "<a id=\"opt-" "[^\"]*" "\"")
let r_entry_desc = reg (between @"<dd>\s*<p>" "~(_*</_*)" "</")
let r_entry_type = (reg (between @"<em>Type:</em></span>\s*" @"\S~(_*</_*)" "</p>"))

let r_entry_default =
    (reg (between @"<em>Default:</em></span>\s*<code class=""literal"">" @"[^<]*" "</"))

let tryFirst (r: Resharp.Regex) (s: string) fallback =
    r.Matches(s)
    |> Seq.tryHead
    |> Option.map (fun m -> m.Value)
    |> Option.defaultValue fallback

type NixosOption = {
    Name: string
    Desc: string
    Type: string
    Default: string
}

let option_entries = r_entry.Matches(nixos_options_text)

let nixos_options_structured =
    option_entries
    |> Seq.skip 1
    |> Seq.map (fun m -> m.Value)
    |> Seq.mapi (fun i entry ->
        try
            {
                Name = tryFirst r_entry_name entry "none"
                Desc = tryFirst r_entry_desc entry "none"
                Type = tryFirst r_entry_type entry "none"
                Default = tryFirst r_entry_default entry "none"
            }
        with e ->
            failwith $"{i} {e.Message}"
    )
    |> Seq.toArray
    |> Json.serialize ()
    |> File.writeTo "nixos-options.json"

let hm_options_structured =
    r_entry.Matches(File.ReadAllText "hm_options.html")
    |> Seq.skip 1
    |> Seq.map (fun m -> m.Value)
    |> Seq.mapi (fun i entry ->
        try
            {
                Name = tryFirst r_entry_name entry "none"
                Desc = tryFirst r_entry_desc entry "none"
                Type = tryFirst r_entry_type entry "none"
                Default = tryFirst r_entry_default entry "none"
            }
        with e ->
            failwith $"{i} {e.Message}"
    )
    |> Seq.toArray
    |> Json.serialize ()
    |> File.writeTo "hm-options.json"


type NixosPackage = {
    Name: string
    Version: string
}
// #r "nuget: fsharp.data, 6.4.0" 
// type HyprClient = FSharp.Data.JsonProvider<"pkgsource.json">
// let ctx = HyprClient.GetSamples()
// let pkgs2 = ctx |> Seq.map (fun p -> { Name = p.CompletionText; Version = p.ToolTip.Substring("source: nixos\nversion: ".Length) }) |> Seq.toArray
// pkgs2 |> Json.serialize () |> File.writeTo "pkgs2.json"