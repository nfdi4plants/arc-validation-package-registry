let [<Literal>]PACKAGE_METADATA = """(*
---
Name: workflowhub
Summary: Validates if the workflows and runs of an ARC carry the metadata needed to be packaged as a Workflow RO-Crate and deposited on WorkflowHub.
Description: |
  Validates if the workflows and runs of an ARC carry the metadata needed to be packaged as a
  Workflow RO-Crate and deposited on WorkflowHub (https://workflowhub.eu).

  Every workflow in `workflows/` and every run in `runs/` is checked separately and reported
  under its own identifier, so an ARC may contain a mix of depositable and not-yet-depositable
  items. The ARC as a whole passes when it has a LICENSE file with text and at least one workflow
  or run that satisfies every requirement. Without license text the badge counts no item as ready.

  Registration on WorkflowHub additionally requires choosing a WorkflowHub team. The team is not
  part of the ARC and cannot be validated here.

  ARC-wide (critical):
  - The ARC can be loaded
  - The ARC has a LICENSE file
  - At least one workflow or run is ready for WorkflowHub

  Per workflow (reported individually):
  - Title and Description are set
  - The title is at most 255 characters (WorkflowHub's limit)
  - At least one contact, each with a first name
  - A CWL description exists
  - URI and Version are set
  - Every component has a component type with a name
  - ARCtrl can convert it into a Workflow RO-Crate
  - Every file the crate references is in the ARC

  Per run (reported individually):
  - Title and Description are set
  - The title is at most 255 characters (WorkflowHub's limit)
  - At least one performer, each with a first name
  - A CWL description exists
  - Every required CWL input parameter has a value in `run.yml`, unless the CWL declares a default
    for it (an optional parameter needs none)
  - Assigned values match the parameter types, and so does the CWL default that takes the place
    of a missing or null `run.yml` value
  - The first process table has input and output columns with a value in every row
  - ARCtrl can convert it into a Workflow Run RO-Crate
  - Every file the crate references is in the ARC
MajorVersion: 0
MinorVersion: 0
PatchVersion: 2
Publish: true
Authors:
  - FullName: Caroline Ott
    Email: caroline.ott@rptu.de
    Affiliation: DataPLANT
Tags:
  - Name: ARC
  - Name: WorkflowHub
  - Name: RO-Crate
  - Name: workflow
  - Name: CWL
  - Name: data publication
ReleaseNotes: |
  - runs on ARCtrl 3.3.1
  - a CWL input parameter that is optional or has a CWL default no longer needs a value in run.yml
  - value types are compared with CWLType.admits, so a File value is accepted for a File? input
  - a CWL default is type-checked when run.yml has no value or a null value for the parameter
  - an item whose crate references a file that is not in the ARC is not ready
  - the badge and the ARC-wide readiness case count no item as ready when the LICENSE file is missing or empty
  - declares the CQC hook endpoint of the endpoint submission service
CQCHookEndpoint: https://ess.nfdi4plants.org/api/v1/hooks/workflowhub
---
*)"""

#r "nuget: ARCtrl, 3.3.1"
#r "nuget: ARCExpect.Core, 7.0.0-alpha"

open ARCtrl
open ARCtrl.CWL
open ARCExpect
open Expecto
open System.IO
open System.Runtime.ExceptionServices
open System.Text.Json


// Input:
let arcDir = Directory.GetCurrentDirectory()


// Values:

/// Loading may fail on a malformed ARC. Keep the error so it can be surfaced as a test failure
/// instead of aborting the whole script.
let loadedArc =
    try Ok (ARC.load arcDir)
    with e -> Error e.Message

let workflows =
    match loadedArc with
    | Ok arc -> arc.Workflows |> List.ofSeq
    | Error _ -> []

let runs =
    match loadedArc with
    | Ok arc -> arc.Runs |> List.ofSeq
    | Error _ -> []

/// The license ARCtrl would hand to the RO-Crate writer. When the ARC has no LICENSE file,
/// ARCtrl silently substitutes "ALL RIGHTS RESERVED BY THE AUTHORS", which is why the presence
/// of a real license file is checked separately.
let license =
    match loadedArc with
    | Ok arc -> arc.License |> Option.defaultWith License.GetDefaultLicense
    | Error _ -> License.GetDefaultLicense()


// Helper types:

/// The outcome of a single named requirement. It is evaluated once, when the requirement list of a
/// workflow or run is built, and only the outcome is kept: the ARC-wide readiness case and the
/// reported test case both read it, so no requirement is ever checked twice.
type Requirement = {
    Name    : string
    Failure : exn option
}

/// Evaluates `run` immediately. It raises with an explanatory message when the requirement is not
/// met, and that exception is what gets recorded.
let requirement name (run : unit -> unit) =
    try run (); { Name = name; Failure = None }
    with e -> { Name = name; Failure = Some e }

let holds (r : Requirement) = r.Failure.IsNone

/// Replays the recorded outcome. Rethrowing the original exception with its original stack trace
/// keeps Expecto's distinction between a failed requirement and an unexpected error.
let toTestCase (r : Requirement) =
    testCase r.Name (fun () ->
        match r.Failure with
        | Some e -> ExceptionDispatchInfo.Capture(e).Throw()
        | None -> ()
    )

/// Reports a validation failure without an F# stack trace, so the JUnit report and the console
/// show the explanation instead of script internals.
let fail (message : string) : unit = Expecto.Tests.failtestNoStackf "%s" message

let expect condition message =
    if not condition then fail message

/// True when a cell contributes no name to the process input or output it becomes. The first
/// content entry is the naming one for every cell kind, and ARCtrl happily composes a node from an
/// empty one, which lands in the crate as an unnamed input or output.
let cellIsBlank (cell : CompositeCell) =
    match cell.GetContent() with
    | [||] -> true
    | content -> content.[0].Trim() = ""

/// Positions (1-based) of the persons ARCtrl cannot compose. It builds `creator` from the first
/// name and fails with "Person must have a given name" when it is missing.
let personsWithoutFirstName (persons : Person seq) =
    persons
    |> Seq.mapi (fun i p -> i, p)
    |> Seq.filter (fun (_, p) -> p.FirstName |> Option.forall (fun n -> n.Trim() = ""))
    |> Seq.map (fun (i, _) -> string (i + 1))
    |> List.ofSeq

/// The crate JSON of one write, or the message of the exception it raised. It is None when the
/// ARC did not load, which the ARC-wide case reports. The crate file check below reads the JSON.
let writeCrate (write : ARC -> string) =
    match loadedArc with
    | Error _ -> None
    | Ok arc ->
        try Some (Ok (write arc))
        with e -> Some (Error e.Message)

// Port of WfHubClient `Pack.referencedFiles` and of the path resolution in
// `Pack.assembleZipWithLimit` (endpoint-submission src/WfHubClient/Pack.fs). The service packs an
// item only when every file the crate references resolves inside the ARC directory, so the same
// rules decide here whether an item is ready. The LFS pointer check and the size limit stay with
// the service. Both depend on the service's own checkout, which downloads LFS content only under
// `workflows/`, and the checkout this package runs on can differ from it.

// Two or more scheme characters, so a Windows drive such as `C:/x` is a path.
let uriScheme = System.Text.RegularExpressions.Regex @"^[A-Za-z][A-Za-z0-9+.-]+:"

/// The paths, relative to the ARC root, that the crate's root dataset references via `hasPart`,
/// deduplicated. Fragment ids and ids with a URI scheme name no file, and neither does the root.
/// `ro-crate-metadata.json` is added by the service and not listed.
let referencedFiles (crateJson : string) =
    use document = JsonDocument.Parse crateJson

    let ids (element : JsonElement) =
        match element.ValueKind with
        | JsonValueKind.Array -> element.EnumerateArray() |> Seq.toList
        | _ -> [ element ]
        |> List.choose (fun part ->
            match part.TryGetProperty "@id" with
            | true, id -> Option.ofObj (id.GetString())
            | _ -> None)

    let root =
        document.RootElement.GetProperty("@graph").EnumerateArray()
        |> Seq.tryFind (fun entity ->
            match entity.TryGetProperty "@id" with
            | true, id -> id.GetString() = "./"
            | _ -> false)

    match root with
    | None -> []
    | Some rootDataset ->
        match rootDataset.TryGetProperty "hasPart" with
        | true, parts ->
            ids parts
            |> List.filter (fun id -> not (id.StartsWith "#") && not (uriScheme.IsMatch id))
            |> List.map (fun id -> if id.StartsWith "./" then id.Substring 2 else id)
            |> List.filter (fun path ->
                not (List.contains path [ ""; "."; "./"; "ro-crate-metadata.json" ]))
            |> List.distinct
        | _ -> []

/// The referenced paths that do not resolve to a file or folder the service would pack. The
/// service rejects anything outside the ARC root, the root itself, a link anywhere on the path or
/// below a folder, a path under `.git` and any `:`. A rejected path counts as missing.
let missingFiles (arcDirectory : string) (files : string list) =
    let root = Path.GetFullPath arcDirectory
    // A filesystem root such as `C:\` or `/` already ends with a separator.
    let rootPrefix =
        if Path.EndsInDirectorySeparator root then
            root
        else
            root + string Path.DirectorySeparatorChar

    let isLink (path : string) =
        let info : FileSystemInfo =
            if Directory.Exists path then DirectoryInfo path else FileInfo path

        not (isNull info.LinkTarget) || info.Attributes.HasFlag FileAttributes.ReparsePoint

    let entryName (full : string) = Path.GetRelativePath(root, full).Replace('\\', '/')

    let resolveOne (relative : string) =
        let full = Path.GetFullPath(Path.Combine(root, relative))

        let throughLink () =
            let parts =
                Path.GetRelativePath(root, full).Split(
                    [| Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar |]
                )

            parts
            |> Array.mapi (fun i _ -> Path.Combine(root, System.String.Join(string Path.DirectorySeparatorChar, parts[..i])))
            |> Array.exists isLink

        let inGit () =
            let first =
                Path.GetRelativePath(root, full).Split(
                    [| Path.DirectorySeparatorChar; Path.AltDirectorySeparatorChar |]
                )[0]

            System.String.Equals(first, ".git", System.StringComparison.OrdinalIgnoreCase)

        // None means a link was found, so the whole referenced path is rejected.
        let walk (start : string) =
            let options =
                EnumerationOptions(
                    RecurseSubdirectories = false,
                    IgnoreInaccessible = false,
                    AttributesToSkip = FileAttributes.None
                )

            let found = ResizeArray()

            // Seq.forall stops at the first link, which rejects the whole folder.
            let rec visit (dir : string) =
                Directory.EnumerateFileSystemEntries(dir, "*", options)
                |> Seq.forall (fun entry ->
                    if isLink entry then
                        false
                    elif Directory.Exists entry then
                        System.String.Equals(Path.GetFileName entry, ".git", System.StringComparison.OrdinalIgnoreCase)
                        || visit entry
                    else
                        found.Add(entryName entry, entry)
                        true)

            if visit start then Some(List.ofSeq found) else None

        if not (full.StartsWith(rootPrefix, System.StringComparison.Ordinal)) then
            None
        elif Path.TrimEndingDirectorySeparator full = Path.TrimEndingDirectorySeparator root then
            None
        elif Path.GetRelativePath(root, full).Contains ':' then
            None
        elif not (File.Exists full || Directory.Exists full) || inGit () || throughLink () then
            None
        elif File.Exists full then
            Some [ (entryName full, full) ]
        else
            walk full

    // The raw path comes first because ARCtrl writes ids unencoded. The decoded path covers
    // writers that percent-encode.
    let resolve (relative : string) =
        let exists (path : string) =
            let full = Path.GetFullPath(Path.Combine(root, path))
            File.Exists full || Directory.Exists full

        let decoded = System.Uri.UnescapeDataString relative

        if not (exists relative) && decoded <> relative then
            resolveOne decoded
        else
            resolveOne relative

    files |> List.filter (fun file -> (resolve file).IsNone)

/// The failure text of the crate file check, or None when every referenced file resolves.
/// The wording is the one WfHubClient reports when it cannot pack the crate.
let crateFileFailure (arcDirectory : string) (crateJson : string) =
    match missingFiles arcDirectory (referencedFiles crateJson) with
    | [] -> None
    | missing ->
        Some $"""The crate references file(s) that are missing from the ARC directory or not allowed in a crate (outside the ARC, the ARC root itself, a link, under .git, or containing ':'): {String.concat ", " missing}"""


// Requirements per workflow.
let workflowRequirements (workflow : ArcWorkflow) =
    let id = workflow.Identifier
    let crate = writeCrate (fun arc -> ARCtrl.Json.ARC.ROCrate.writeWorkflowAsCrate(workflow, arc.FileSystem, license))
    [
        requirement "Title is set" (fun () ->
            expect
                (workflow.Title |> Option.exists (fun t -> t.Trim() <> ""))
                $"Workflow '{id}' has no title. ARCtrl writes it as `name` of the ARC Workflow dataset and of the crate root, and WorkflowHub uses `name` as the workflow title, which is mandatory on registration."
        )

        requirement "Title is at most 255 characters" (fun () ->
            match workflow.Title with
            | Some t when t.Length > 255 ->
                fail $"The title of workflow '{id}' is {t.Length} characters long. WorkflowHub rejects titles longer than 255 characters."
            | _ -> ()
        )

        requirement "Description is set" (fun () ->
            expect
                (workflow.Description |> Option.exists (fun d -> d.Trim() <> ""))
                $"Workflow '{id}' has no description. ARCtrl requires `description` on the ARC Workflow dataset before writing the crate, and WorkflowHub shows it as the workflow description."
        )

        requirement "At least one contact" (fun () ->
            expect
                (workflow.Contacts.Count > 0)
                $"Workflow '{id}' has no contacts. ARCtrl requires `creator` both on the ARC Workflow dataset and on the workflow protocol, and WorkflowHub reads it as the workflow creators."
        )

        requirement "Every contact has a first name" (fun () ->
            personsWithoutFirstName workflow.Contacts
            |> function
                | [] -> ()
                | positions ->
                    fail $"""Contact(s) at position {String.concat ", " positions} of workflow '{id}' have no first name. ARCtrl composes `creator` from it and fails with "Person must have a given name"."""
        )

        requirement "CWL description exists" (fun () ->
            expect
                workflow.CWLDescription.IsSome
                $"Workflow '{id}' has no CWL description. ARCtrl cannot compose the workflow protocol without one, and it becomes the crate's `mainEntity`, which the Workflow RO-Crate profile requires. Add a `workflows/{id}/workflow.cwl`."
        )

        requirement "URI is set" (fun () ->
            expect
                (workflow.URI |> Option.exists (fun u -> u.Trim() <> ""))
                $"Workflow '{id}' has no URI. ARCtrl only writes `url` on the workflow protocol when the URI is set and rejects the crate without it."
        )

        requirement "Version is set" (fun () ->
            expect
                (workflow.Version |> Option.exists (fun v -> v.Trim() <> ""))
                $"Workflow '{id}' has no version. ARCtrl only writes `version` on the workflow protocol when the version is set and rejects the crate without it."
        )

        requirement "All components have a component type with a name" (fun () ->
            workflow.Components
            |> Seq.mapi (fun i c -> i, c)
            |> Seq.filter (fun (_, c) ->
                match c.ComponentType with
                | Some ct -> ct.Name.IsNone
                | None -> true
            )
            |> Seq.map (fun (i, _) -> string i)
            |> List.ofSeq
            |> function
                | [] -> ()
                | positions ->
                    fail $"""Workflow '{id}' has components without a named component type at position(s) {String.concat ", " positions}. ARCtrl needs the name to compose the computational tool PropertyValue and fails otherwise."""
        )

        requirement "ARCtrl can write the Workflow RO-Crate" (fun () ->
            match crate with
            | Some (Error message) ->
                fail $"ARCtrl could not write workflow '{id}' as a Workflow RO-Crate: {message}"
            | _ -> ()
        )

        requirement "Every file the crate references is in the ARC" (fun () ->
            match crate with
            | Some (Ok crateJson) ->
                crateFileFailure arcDir crateJson
                |> Option.iter (fun message -> fail $"Workflow '{id}': {message}")
            | _ -> ()
        )
    ]


// Requirements per run.
let runRequirements (run : ArcRun) =
    let id = run.Identifier
    let crate = writeCrate (fun arc -> ARCtrl.Json.ARC.ROCrate.writeRunAsCrate(run, arc.FileSystem, license))
    [
        requirement "Title is set" (fun () ->
            expect
                (run.Title |> Option.exists (fun t -> t.Trim() <> ""))
                $"Run '{id}' has no title. ARCtrl writes it as `name` of the ARC Run dataset and of the crate root, and WorkflowHub uses `name` as the title, which is mandatory on registration."
        )

        requirement "Title is at most 255 characters" (fun () ->
            match run.Title with
            | Some t when t.Length > 255 ->
                fail $"The title of run '{id}' is {t.Length} characters long. WorkflowHub rejects titles longer than 255 characters."
            | _ -> ()
        )

        requirement "Description is set" (fun () ->
            expect
                (run.Description |> Option.exists (fun d -> d.Trim() <> ""))
                $"Run '{id}' has no description. ARCtrl requires `description` on the ARC Run dataset before writing the crate, and WorkflowHub shows it as the description."
        )

        requirement "At least one performer" (fun () ->
            expect
                (run.Performers.Count > 0)
                $"Run '{id}' has no performers. ARCtrl requires `creator` on the ARC Run dataset, and WorkflowHub reads it as the creators."
        )

        requirement "Every performer has a first name" (fun () ->
            personsWithoutFirstName run.Performers
            |> function
                | [] -> ()
                | positions ->
                    fail $"""Performer(s) at position {String.concat ", " positions} of run '{id}' have no first name. ARCtrl composes `creator` from it and fails with "Person must have a given name"."""
        )

        requirement "CWL description exists" (fun () ->
            expect
                run.CWLDescription.IsSome
                $"Run '{id}' has no CWL description. ARCtrl cannot compose the workflow protocol without one, and it becomes the crate's `mainEntity`, which the Workflow RO-Crate profile requires. Add a `runs/{id}/run.cwl`."
        )

        requirement "Every CWL input parameter has an assigned value" (fun () ->
            match run.CWLDescription with
            | None -> ()
            | Some pu ->
                let assigned = run.CWLInput |> Seq.map (fun i -> i.Key) |> Set.ofSeq
                CWLProcessingUnit.getInputValues pu
                |> Seq.filter (fun i -> not (assigned.Contains i.Name || i.Optional || i.DefaultValue.IsSome))
                |> Seq.map (fun i -> i.Name)
                |> List.ofSeq
                |> function
                    | [] -> ()
                    | unassigned ->
                        fail $"""Run '{id}' has no value in `runs/{id}/run.yml` for the required CWL input parameter(s) {String.concat ", " unassigned}, and the CWL declares no default for them. ARCtrl builds the workflow invocation from these values and fails when a required parameter has none. Add the values to `runs/{id}/run.yml` or give the parameters a default in the CWL."""
        )

        requirement "Assigned values match the parameter types" (fun () ->
            match run.CWLDescription with
            | None -> ()
            | Some pu ->
                let assigned = run.CWLInput |> Seq.map (fun v -> v.Key, v) |> Map.ofSeq
                CWLProcessingUnit.getInputValues pu
                |> Seq.choose (fun i ->
                    i.Type_
                    |> Option.bind (fun paramType ->
                        // ARCtrl substitutes the default only for a missing or null run.yml value,
                        // so the rule skips a run.yml value without an inferable type.
                        let effectiveType =
                            match assigned.TryFind i.Name with
                            | Some v when v.Value.IsSome && v.Value.Value <> CWLParameterValue.Null ->
                                v.Type |> Option.map (fun t -> t, "")
                            | _ ->
                                i.DefaultValue
                                |> Option.bind (fun d ->
                                    DecodeParameters.decodeParameterValue d.Node
                                    |> CWLParameterValue.tryInferType
                                    |> Option.map (fun t -> t, " from the CWL default"))

                        effectiveType
                        |> Option.filter (fun (valueType, _) -> not (CWLType.admits paramType valueType))
                        |> Option.map (fun (valueType, origin) ->
                            $"{i.Name}{origin} ({Encode.formatCWLType valueType} vs. {Encode.formatCWLType paramType})"))
                )
                |> List.ofSeq
                |> function
                    | [] -> ()
                    | mismatches ->
                        fail $"""Run '{id}' assigns values whose type does not match the CWL input parameter: {String.concat ", " mismatches}. ARCtrl rejects the conversion on such a mismatch."""
        )

        // ARCtrl validates only the first workflow invocation of a run, so convertibility depends
        // on process table order and a run without process tables never converts. Requiring a
        // fully described first process, with an input and an output column, is stricter than
        // ARCtrl needs.
        requirement "First process table has filled input and output columns" (fun () ->
            match run.Tables |> Seq.tryHead with
            | None ->
                fail $"Run '{id}' has no process table. ARCtrl requires `result` on the workflow invocation, and it is only filled from the outputs of a process table; the invocation composed from the CWL inputs alone carries none."
            | Some table ->
                // A missing column leaves the invocation without `object`/`result`; a present but
                // empty cell is worse, because ARCtrl composes an unnamed node from it and writes
                // the crate as if the process were described.
                let column (label : string) (isColumn : CompositeHeader -> bool) =
                    match table.Headers |> Seq.tryFindIndex isColumn with
                    | None -> Some $"has no {label} column"
                    | Some col ->
                        [ 0 .. table.RowCount - 1 ]
                        |> List.filter (fun row ->
                            match table.TryGetCellAt(col, row) with
                            | Some cell -> cellIsBlank cell
                            | None -> true
                        )
                        |> function
                            | [] -> None
                            | blank ->
                                let shown = blank |> List.truncate 5 |> List.map (fun r -> string (r + 1))
                                let more = if blank.Length > 5 then ", ..." else ""
                                Some $"""has an empty {label} cell in row(s) {String.concat ", " shown}{more}"""
                [
                    (if table.RowCount = 0 then Some "has no rows" else None)
                    column "input" (fun h -> h.isInput)
                    column "output" (fun h -> h.isOutput)
                ]
                |> List.choose (fun problem -> problem)
                |> function
                    | [] -> ()
                    | problems ->
                        fail $"""The first process table '{table.Name}' of run '{id}' {String.concat " and " problems}. ARCtrl composes the run's workflow invocation from this table's rows and validates only the first one, so the table has to name the process inputs (`object`) and outputs (`result`); later tables are not considered."""
        )

        requirement "ARCtrl can write the Workflow Run RO-Crate" (fun () ->
            match crate with
            | Some (Error message) ->
                fail $"ARCtrl could not write run '{id}' as a Workflow Run RO-Crate: {message}"
            | _ -> ()
        )

        requirement "Every file the crate references is in the ARC" (fun () ->
            match crate with
            | Some (Ok crateJson) ->
                crateFileFailure arcDir crateJson
                |> Option.iter (fun message -> fail $"Run '{id}': {message}")
            | _ -> ()
        )
    ]


// Building the lists is what evaluates the requirements, once. The badge, the ARC-wide readiness
// case and the reported test cases all read these same outcomes.
let workflowResults = workflows |> List.map (fun w -> w, workflowRequirements w)
let runResults = runs |> List.map (fun r -> r, runRequirements r)

let readyWorkflows = workflowResults |> List.filter (fun (_, rs) -> rs |> List.forall holds)
let readyRuns = runResults |> List.filter (fun (_, rs) -> rs |> List.forall holds)

let readyCount = readyWorkflows.Length + readyRuns.Length
let totalCount = workflowResults.Length + runResults.Length

/// The crate must specify a license, so a missing or empty LICENSE file makes every item not
/// ready. The badge and the ARC-wide readiness case below both count with `badgeReadyCount`,
/// which is zero without license text.
let hasLicenseText =
    match loadedArc with
    | Ok arc -> arc.License |> Option.exists (fun lic -> lic.Content.Trim() <> "")
    | Error _ -> false

let badgeReadyCount = if hasLicenseText then readyCount else 0


// Validation Cases:

let arcCases =
    testList "ARC" [
        testCase "ARC can be loaded" (fun () ->
            match loadedArc with
            | Ok _ -> ()
            | Error message -> fail $"The ARC at '{arcDir}' could not be loaded: {message}"
        )

        // Workflow RO-Crate profile 1.0: "The Crate MUST specify a license."
        // ARCtrl does not enforce this: without a LICENSE file it substitutes the placeholder
        // "ALL RIGHTS RESERVED BY THE AUTHORS", which is not one of the licenses WorkflowHub accepts.
        testCase "ARC has a LICENSE file" (fun () ->
            match loadedArc with
            | Error _ -> ()
            | Ok arc ->
                match arc.License with
                | None ->
                    fail """The ARC has no LICENSE file. The Workflow RO-Crate profile 1.0 requires the crate to specify a license, and without one ARCtrl writes the placeholder "ALL RIGHTS RESERVED BY THE AUTHORS" instead. Add a LICENSE, LICENSE.txt, LICENSE.md or LICENSE.rst at the ARC root."""
                | Some lic when lic.Content.Trim() = "" ->
                    fail $"The LICENSE file '{lic.Path}' of this ARC is empty. Its text is what ARCtrl writes into the crate as the license, so it has to state the license the workflow is published under."
                | Some _ -> ()
        )

        testCase "At least one workflow or run is ready for WorkflowHub" (fun () ->
            match loadedArc with
            | Error _ -> ()
            | Ok _ ->
                if totalCount = 0 then
                    fail "The ARC contains neither a workflow in `workflows/` nor a run in `runs/`, so there is nothing to deposit on WorkflowHub."
                else
                    expect
                        (badgeReadyCount > 0)
                        $"None of the {totalCount} workflow(s)/run(s) of this ARC meets all requirements for a WorkflowHub deposition. A missing or empty LICENSE file also makes every item not ready. See the individual results below for what is missing."
        )
    ]

let workflowCases =
    workflowResults
    |> List.map (fun (workflow, requirements) ->
        testList $"workflows/{workflow.Identifier}" (requirements |> List.map toTestCase)
    )

let runCases =
    runResults
    |> List.map (fun (run, requirements) ->
        testList $"runs/{run.Identifier}" (requirements |> List.map toTestCase)
    )


// Execution:

Setup.ValidationPackage(
    metadata = Setup.Metadata(PACKAGE_METADATA, AVPRIndex.Frontmatter.FSharpFrontmatter),
    CriticalValidationCases = [arcCases],
    NonCriticalValidationCases = (workflowCases @ runCases)
)
|> Execute.ValidationPipeline(
    basePath = arcDir,
    BadgeLabelText = $"{badgeReadyCount}/{totalCount} ready for WorkflowHub"
)
