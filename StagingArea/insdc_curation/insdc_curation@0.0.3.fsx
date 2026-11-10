let [<Literal>]PACKAGE_METADATA = """(*
---
Name: insdc_curation
Description: Validates certain INSDC record curation metadata.
Summary: |
  Validates certain INSDC record curation metadata.
    - fastqc files
      - experiment accession annotation is present and correct
      - descends from a correctly named sample
    - Sample Provenance I/O:
      - Validates presence and formatting of input headers
      - Validates presence and formatting of output headers
MajorVersion: 0
MinorVersion: 0
PatchVersion: 3
Publish: true
Authors:
  - FullName: Emre Filiz
    Affiliation: DataPLANT
Tags:
  - Name: INSDC
  - Name: Metadata curation
ReleaseNotes: |
  - previous fail in ´´Experiment accession annotation is missing´´ fixed
---
*)"""

#r "nuget: ARCtrl, 3.0.5"
#r "nuget: ARCtrl.QueryModel, 3.0.0-alpha.4"
#r "nuget: ARCExpect.Core, 7.0.0-alpha"
#r "nuget: FsHttp"
#r "nuget: FSharpAux"
#r "nuget: FSharpAux.IO"

open ARCtrl
open ARCtrl.QueryModel
open ARCExpect
open FsHttp
open FSharpAux.IO.SchemaReader
open Expecto
open System.IO
open System

let arcDir = Directory.GetCurrentDirectory()

let arc =
    try ARC.load arcDir with
    | _ -> ARC(identifier = "unable to load arc from this dir")

let project_accession = arc.Identifier

module Domain =

    open FSharpAux.IO.SchemaReader.Attribute
    open System.Collections.Generic

    /// represents the relations provided by ENA portal API in TSV form
    type INSDC_Relations = {
        [<FieldAttribute("study_accession")>]
        study_accession: string
        [<FieldAttribute("sample_accession")>]
        sample_accession: string
        [<FieldAttribute("experiment_accession")>]
        experiment_accession: string
        [<FieldAttribute("run_accession")>]
        run_accession: string
        [<FieldAttribute("fastq_ftp")>]
        fastq_ftp: string
    }

    /// Some records map from sample to multiple fastq files in a single line (e.g., paired end reads). This function splits such records into multiple records, one for each fastq file.
    let splitByFastq (r: INSDC_Relations) =
        match r.fastq_ftp.Split(';') |> Array.filter (fun s -> s <> "") with
        | [||]  -> [ r ]                                        // no fastq → keep row as-is
        | files -> [ for f in files -> { r with fastq_ftp = f } ]

    /// lookup index for retrieving INSDC_Relations by any of the accession numbers or fastq file name. Last-wins in case of duplicates.
    let buildIndex (records: INSDC_Relations seq) =
        let d = Dictionary<string, INSDC_Relations>(StringComparer.OrdinalIgnoreCase)
        for r in records do
            for v in [ 
                r.study_accession
                r.sample_accession
                r.experiment_accession
                r.run_accession
                r.fastq_ftp 
            ] do
                if not (String.IsNullOrEmpty v) then d[v] <- r   // last-wins
        d

    // IO-Curation
    let checkFirstTableIO (table: ArcTable) =
        let hasSourceInput =
            table.Headers
            |> Seq.exists (function
                | CompositeHeader.Input IOType.Source -> true
                | _ -> false
            )

        let hasOutput =
            table.Headers
            |> Seq.exists (function 
                | CompositeHeader.Output _ -> true
                | _ -> false
            )
        hasSourceInput && (not hasOutput)

    let checkTableIO (table: ArcTable) =
        let hasSourceInput =
            table.Headers
            |> Seq.exists (function
                | CompositeHeader.Input IOType.Source -> true
                | _ -> false
            )

        let hasSourceOutput =
            table.Headers
            |> Seq.exists (function
                | CompositeHeader.Output IOType.Source -> true
                | _ -> false
            )

        hasSourceInput && hasSourceOutput

    let checkLastTableIO (table: ArcTable) =
        let hasSourceInput =
            table.Headers
            |> Seq.exists (function
                | CompositeHeader.Input IOType.Source -> true
                | _ -> false
            )

        let hasSampleOutput = 
            table.Headers
            |> Seq.exists (function
                | CompositeHeader.Output IOType.Sample -> true
                | _ -> false
            )

        hasSourceInput && hasSampleOutput

    module OntologyTerms = 
        let experiment_accession_term =
            OntologyAnnotation.create(name="Experiment Accession Number", tsr="NCIT", tan="NCIT:C175892")

module ExpectedData =

    let reader = new Csv.CsvReader<Domain.INSDC_Relations>(SchemaMode=Csv.Fill)

    let relations = 
        try
            http {
                GET $"https://www.ebi.ac.uk/ena/portal/api/filereport?accession={project_accession}&result=read_run&fields=study_accession,sample_accession,experiment_accession,run_accession,tax_id,scientific_name,fastq_ftp,submitted_ftp,bam_ftp&format=tsv&download=true&limit=0"
            }
            |> Request.send
            |> Response.toText
            |> fun response -> reader.ReadFromString(response, '\t', firstLineHasHeader = true)
            |> Seq.collect Domain.splitByFastq
            |> List.ofSeq
        with
            | _ -> []

    let relations_index = Domain.buildIndex relations

    let tryFindRelation (s: string) =
        match relations_index.TryGetValue s with
        | true, r  -> Some r
        | false, _ -> None


let create_insdc_relation_validation_cases_for_fastq_file (node: QNode) =

    let tableHasOutput (t: ArcTable) =
        t.Headers |> Seq.exists (function 
            CompositeHeader.Output _ -> true 
            | _ -> false
        )

    let filteredArcTables =
        let validTables =
            arc.ArcTables.Tables
            |> Seq.filter tableHasOutput
            |> ResizeArray
        ArcTables(validTables)

    let samples =
        try
            filteredArcTables.SamplesOf(node)
            |> Seq.map (fun n -> n.Name)
            |> Array.ofSeq
        with
        | _ -> [||]

    let experiment_accession_actual =
        try 
            Some (filteredArcTables.PreviousParametersOf(node)).[Domain.OntologyTerms.experiment_accession_term]
        with
            | _ -> None


    testList node.Name [

        let expected_relations = ExpectedData.tryFindRelation node.Name

        test $"""has correct experiment accession annotation (expected: {(expected_relations |> Option.map (fun r -> r.experiment_accession) |> Option.defaultValue "N/A")})""" {
            let expected = (Expect.wantSome expected_relations $"No relations for {node.Name} in {project_accession} relations").experiment_accession
            let actual = Expect.wantSome experiment_accession_actual "Experiment accession annotation is missing"
            Expect.equal actual.ValueText expected "Experiment accession annotation does not match node name"
        }

        test $"""descends from correct sample (expected: {(expected_relations |> Option.map (fun r -> r.sample_accession) |> Option.defaultValue "N/A")})""" {
            let expected = (Expect.wantSome expected_relations $"No relations for {node.Name} in {project_accession} relations").sample_accession
            Expect.contains samples expected $"Sample accession {expected} not found in ARC"
        }
    ]

let fastqc_cases =
    testList "fastqc" (
        arc.TryGetAssay("sequencing")
        |> Option.map (fun assay ->
            assay.LastData
            |> Seq.map (fun node -> create_insdc_relation_validation_cases_for_fastq_file node)
            |> Seq.toList
        ) 
        |> Option.defaultValue []
    )

let io_cases =
    arc.Studies
    |> Seq.map (fun study -> 
        let tables = study.Tables |> Seq.toList

        let tableTests =
            if tables.Length = 0 then
                [
                    test "Has atleast 2 tables" {
                        Expect.isTrue false $"Study {study.Identifier} has no tables for validation."
                    }
                ]
            elif tables.Length = 1 then
                [
                    test "Has atleast 2 tables" {
                        Expect.isTrue false $"Study {study.Identifier} has only 1 table for validation."
                    }
                ]
            else
                let firstTable = tables.Head
                let lastTable = tables |> List.last
                let middleTables = tables |> List.skip 1 |> List.take (tables.Length - 2)

                let firstTest = 
                    test $"First table '{firstTable.Name}'" {
                        Expect.isTrue (Domain.checkFirstTableIO firstTable) "First table must not have an output, but a Source-Input."
                    }

                let lastTest = 
                    test $"Last table '{lastTable.Name}'" {
                        Expect.isTrue (Domain.checkLastTableIO lastTable) "Last table must have a Source-Input and a Sample-Output."
                    }

                let midTests =
                    middleTables |> List.map (fun t -> 
                        test $"mid tables '{t.Name}'" {
                            Expect.isTrue (Domain.checkTableIO t) "Mid tables must have Source-Input and Source-Output."
                        }
                    )

                [firstTest] @ midTests @ [lastTest]
        testList $"Study: {study.Identifier}" tableTests
    )
    |> Seq.toList
    |> testList "IO Validation for Studies"


Setup.ValidationPackage(
    metadata = Setup.Metadata(PACKAGE_METADATA, AVPRIndex.Frontmatter.FSharpFrontmatter),
    CriticalValidationCases = [
        test "has sequencing assay" {
            Expect.isSome (arc.TryGetAssay("sequencing")) "No sequencing assay found"
        }
        fastqc_cases
        io_cases
    ]
)
|> Execute.ValidationPipeline(basePath = arcDir)

ExpectedData.relations
