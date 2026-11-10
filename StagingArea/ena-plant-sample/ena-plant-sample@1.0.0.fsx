let [<Literal>]PACKAGE_METADATA = """(*
---
Name: ena-plant-sample
Summary: Validates if the ARC contains the necessary metadata to be publishable via ENA as a plant sample.
Description: |
  Validates if the ARC contains the necessary metadata to be publishable via ENA as a plant sample.
  The following metadata is required:
  - Study has plant developmental stage, collection date, geographic location (country and/or sea), geographic location (latitude), geographical location (longitude), plant growth medium, and isolation and growth condition in correct format
MajorVersion: 1
MinorVersion: 0
PatchVersion: 0
Publish: true
Authors:
  - FullName: Oliver Maus
    Affiliation: DataPLANT
Tags:
  - Name: ARC
  - Name: data publication
  - Name: validation
  - Name: ena-plant-sample
ReleaseNotes: |
  - Initial commit
---
*)"""


#r "nuget: ARCExpect, 5.0.1"


open ControlledVocabulary
open Expecto
open ARCExpect
open ARCTokenization
open ARCTokenization.StructuralOntology
open System.IO


// Input:
let arcDir = Directory.GetCurrentDirectory()


// Values:
let absoluteDirectoryPaths = FileSystem.parseARCFileSystem arcDir

let studyProcessGraphTokens = 
    try 
        absoluteDirectoryPaths
        |> Study.parseProcessGraphColumnsFromTokens arcDir
        |> Seq.collect Map.values
        |> List.concat
    with
        | _ -> List.empty

let pdsTerm = CvTerm.create("ENACL:1011014","plant developmental stage","ENACL")

let pdsTokens =
    studyProcessGraphTokens
    |> List.tryFind (fun cvpList -> cvpList.Head |> Param.getValueAsTerm = pdsTerm)
    |> Option.defaultValue []

let cdTerm = CvTerm.create("ENACL:1011029","collection date","ENACL")

let cdTokens =
    studyProcessGraphTokens
    |> List.tryFind (fun cvpList -> cvpList.Head |> Param.getValueAsTerm = cdTerm)
    |> Option.defaultValue []

let glcaosTerm = CvTerm.create("ENACL:1011031","geographic location (country and/or sea)","ENACL")

let glcaosTokens =
    studyProcessGraphTokens
    |> List.tryFind (fun cvpList -> cvpList.Head |> Param.getValueAsTerm = glcaosTerm)
    |> Option.defaultValue []

let gllaTerm = CvTerm.create("ENACL:1011032","geographic location (latitude)","ENACL")

let gllaTokens =
    studyProcessGraphTokens
    |> List.tryFind (fun cvpList -> cvpList.Head |> Param.getValueAsTerm = gllaTerm)
    |> Option.defaultValue []

let glloTerm = CvTerm.create("ENACL:1011033","geographic location (longitude)","ENACL")

let glloTokens =
    studyProcessGraphTokens
    |> List.tryFind (fun cvpList -> cvpList.Head |> Param.getValueAsTerm = glloTerm)
    |> Option.defaultValue []

let pgmTerm = CvTerm.create("ENACL:1011061","plant growth medium","ENACL")

let pgmTokens =
    studyProcessGraphTokens
    |> List.tryFind (fun cvpList -> cvpList.Head |> Param.getValueAsTerm = pgmTerm)
    |> Option.defaultValue []

let iagcTerm = CvTerm.create("ENACL:1011071","isolation and growth condition","ENACL")

let iagcTokens =
    studyProcessGraphTokens
    |> List.tryFind (fun cvpList -> cvpList.Head |> Param.getValueAsTerm = iagcTerm)
    |> Option.defaultValue []


// Helpers:

open System.Text
open System.Text.RegularExpressions


let cdRegex = Regex(@"^(?:[12][0-9]{3}(?:-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12][0-9]|3[01])(?:T[0-9]{2}:[0-9]{2}(?::[0-9]{2})?Z?(?:[+-][0-9]{1,2})?)?)?)?(?:/[0-9]{4}(?:-[0-9]{2}(?:-[0-9]{2}(?:T[0-9]{2}:[0-9]{2}(?::[0-9]{2})?Z?(?:[+-][0-9]{1,2})?)?)?)?)?|not collected|not provided|restricted access|missing: control sample|missing: sample group|missing: synthetic construct|missing: lab stock|missing: third party data|missing: data agreement established pre-2023|missing: endangered species|missing: human-identifiable)$")

let gllRegex = Regex(@"(^[+-]?[0-9]+.?[0-9]{0,8}$)|(^not collected$)|(^not provided$)|(^restricted access$)|(^missing: control sample$)|(^missing: sample group$)|(^missing: synthetic construct$)|(^missing: lab stock$)|(^missing: third party data$)|(^missing: data agreement established pre-2023$)|(^missing: endangered species$)|(^missing: human-identifiable$)")

let glcaosValues = [|
    "Afghanistan"
    "Albania"
    "Algeria"
    "American Samoa"
    "Andorra"
    "Angola"
    "Anguilla"
    "Antarctica"
    "Antigua and Barbuda"
    "Arctic Ocean"
    "Argentina"
    "Armenia"
    "Aruba"
    "Ashmore and Cartier Islands"
    "Atlantic Ocean"
    "Australia"
    "Austria"
    "Azerbaijan"
    "Bahamas"
    "Bahrain"
    "Baker Island"
    "Baltic Sea"
    "Bangladesh"
    "Barbados"
    "Bassas da India"
    "Belarus"
    "Belgium"
    "Belize"
    "Benin"
    "Bermuda"
    "Bhutan"
    "Bolivia"
    "Borneo"
    "Bosnia and Herzegovina"
    "Botswana"
    "Bouvet Island"
    "Brazil"
    "British Virgin Islands"
    "Brunei"
    "Bulgaria"
    "Burkina Faso"
    "Burundi"
    "Cambodia"
    "Cameroon"
    "Canada"
    "Cape Verde"
    "Cayman Islands"
    "Central African Republic"
    "Chad"
    "Chile"
    "China"
    "Christmas Island"
    "Clipperton Island"
    "Cocos Islands"
    "Colombia"
    "Comoros"
    "Cook Islands"
    "Coral Sea Islands"
    "Costa Rica"
    "Cote d'Ivoire"
    "Croatia"
    "Cuba"
    "Curacao"
    "Cyprus"
    "Czechia                <SYNONYM>Czech Republic</SYNONYM>"
    "Democratic Republic of the Congo"
    "Denmark"
    "Djibouti"
    "Dominica"
    "Dominican Republic"
    "East Timor"
    "Ecuador"
    "Egypt"
    "El Salvador"
    "Equatorial Guinea"
    "Eritrea"
    "Estonia"
    "Ethiopia"
    "Europa Island"
    "Falkland Islands (Islas Malvinas)"
    "Faroe Islands"
    "Fiji"
    "Finland"
    "France"
    "French Guiana"
    "French Polynesia"
    "French Southern and Antarctic Lands"
    "Gabon"
    "Gambia"
    "Gaza Strip"
    "Georgia"
    "Germany"
    "Ghana"
    "Gibraltar"
    "Glorioso Islands"
    "Greece"
    "Greenland"
    "Grenada"
    "Guadeloupe"
    "Guam"
    "Guatemala"
    "Guernsey"
    "Guinea"
    "Guinea-Bissau"
    "Guyana"
    "Haiti"
    "Heard Island and McDonald Islands"
    "Honduras"
    "Hong Kong"
    "Howland Island"
    "Hungary"
    "Iceland"
    "India"
    "Indian Ocean"
    "Indonesia"
    "Iran"
    "Iraq"
    "Ireland"
    "Isle of Man"
    "Israel"
    "Italy"
    "Jamaica"
    "Jan Mayen"
    "Japan"
    "Jarvis Island"
    "Jersey"
    "Johnston Atoll"
    "Jordan"
    "Juan de Nova Island"
    "Kazakhstan"
    "Kenya"
    "Kerguelen Archipelago"
    "Kingman Reef"
    "Kiribati"
    "Kosovo"
    "Kuwait"
    "Kyrgyzstan"
    "Laos"
    "Latvia"
    "Lebanon"
    "Lesotho"
    "Liberia"
    "Libya"
    "Liechtenstein"
    "Lithuania"
    "Luxembourg"
    "Macau"
    "Macedonia"
    "Madagascar"
    "Malawi"
    "Malaysia"
    "Maldives"
    "Mali"
    "Malta"
    "Marshall Islands"
    "Martinique"
    "Mauritania"
    "Mauritius"
    "Mayotte"
    "Mediterranean Sea"
    "Mexico"
    "Micronesia"
    "Midway Islands"
    "Moldova"
    "Monaco"
    "Mongolia"
    "Montenegro"
    "Montserrat"
    "Morocco"
    "Mozambique"
    "Myanmar"
    "Namibia"
    "Nauru"
    "Navassa Island"
    "Nepal"
    "Netherlands"
    "New Caledonia"
    "New Zealand"
    "Nicaragua"
    "Niger"
    "Nigeria"
    "Niue"
    "Norfolk Island"
    "North Korea"
    "North Sea"
    "Northern Mariana Islands"
    "Norway"
    "Oman"
    "Pacific Ocean"
    "Pakistan"
    "Palau"
    "Palmyra Atoll"
    "Panama"
    "Papua New Guinea"
    "Paracel Islands"
    "Paraguay"
    "Peru"
    "Philippines"
    "Pitcairn Islands"
    "Poland"
    "Portugal"
    "Puerto Rico"
    "Qatar"
    "Republic of the Congo"
    "Reunion"
    "Romania"
    "Ross Sea"
    "Russia"
    "Rwanda"
    "Saint Helena"
    "Saint Kitts and Nevis"
    "Saint Lucia"
    "Saint Pierre and Miquelon"
    "Saint Vincent and the Grenadines"
    "Samoa"
    "San Marino"
    "Sao Tome and Principe"
    "Saudi Arabia"
    "Senegal"
    "Serbia"
    "Seychelles"
    "Sierra Leone"
    "Singapore"
    "Sint Maarten"
    "Slovakia"
    "Slovenia"
    "Solomon Islands"
    "Somalia"
    "South Africa"
    "South Georgia and the South Sandwich Islands"
    "South Korea"
    "Southern Ocean"
    "Spain"
    "Spratly Islands"
    "Sri Lanka"
    "Sudan"
    "Suriname"
    "Svalbard"
    "Swaziland"
    "Sweden"
    "Switzerland"
    "Syria"
    "Taiwan"
    "Tajikistan"
    "Tanzania"
    "Tasman Sea"
    "Thailand"
    "Togo"
    "Tokelau"
    "Tonga"
    "Trinidad and Tobago"
    "Tromelin Island"
    "Tunisia"
    "Turkey"
    "Turkmenistan"
    "Turks and Caicos Islands"
    "Tuvalu"
    "USA"
    "Uganda"
    "Ukraine"
    "United Arab Emirates"
    "United Kingdom"
    "Uruguay"
    "Uzbekistan"
    "Vanuatu"
    "Venezuela"
    "Viet Nam"
    "Virgin Islands"
    "Wake Island"
    "Wallis and Futuna"
    "West Bank"
    "Western Sahara"
    "Yemen"
    "Zambia"
    "Zimbabwe"
|]


type ErrorMessage with

    static member ofIParamCollection error iParamCollection =

        let iParam = Seq.head iParamCollection

        let str = new StringBuilder()    
        str.AppendFormat("['{0}', ..] {1}\n",  Param.getCvName iParam, error) |> ignore 

        match Param.tryGetValueOfCvParamAttr "FilePath" iParam with
        | Some path ->
            str.AppendFormat(" > filePath '{0}'\n", path) |> ignore         
        | None -> ()

        match Param.tryGetValueOfCvParamAttr "Worksheet" iParam with
        | Some sheet ->
            str.AppendFormat(" > sheet '{0}'", sheet) |> ignore         
        | None -> ()

        match Param.tryGetValueOfCvParamAttr "Row" iParam with
        | Some row -> 
            str.AppendFormat(" > row '{0}'", row) |> ignore
        | None -> ()

        match Param.tryGetValueOfCvParamAttr "Column" iParam with
        | Some column -> 
            str.AppendFormat(" > column '{0}'", column) |> ignore
        | None -> ()        
                
        match Param.tryGetValueOfCvParamAttr "Line" iParam with
        | Some line ->
            str.AppendFormat(" > line '{0}'", line) |> ignore
        | None -> ()

        match Param.tryGetValueOfCvParamAttr "Position" iParam with
        | Some position -> 
            str.AppendFormat(" > position '{0}'", position) |> ignore
        | None -> ()
        str.ToString()


type Validate.ParamCollection with

    static member AllTermsSatisfyPredicate (projection : #IParam -> bool) (paramCollection : #seq<#IParam>) =
        match Seq.forall projection paramCollection with
        | true  -> ()
        | false ->
            ErrorMessage.ofIParamCollection $"does not satisfy the requirements" paramCollection
            |> Expecto.Tests.failtestNoStackf "%s"

    /// <summary>
    /// Validates if at least one Param with the expected term as value in the given collection exists.
    /// </summary>
    /// <param name="expectedTerm">the term expected to occur in at least 1 Param in the given collection.</param>
    /// <param name="paramCollection">The param collection to validate.</param>
    static member ContainsParamWithValueTerm (expectedTerm : CvTerm) (paramCollection : #seq<#IParam>) =
        if Seq.exists (fun p -> p |> Param.getValueAsTerm = expectedTerm) paramCollection then 
            ()
        else
            expectedTerm
            |> ErrorMessage.ofCvTerm $"value does not exist"
            |> Expecto.Tests.failtestNoStackf "%s"


// Validation Cases:
let studyCases =
    testList STDMSO.``Study Metadata``.STUDY.key.Name [
        // Study has plant developmental stage header in process graph
        ARCExpect.validationCase (TestID.Name "plant developmental stage header exists") {
            pdsTokens
            |> Validate.ParamCollection.ContainsParamWithValueTerm pdsTerm
        }

        // TO DO: Study has plant developmental stage with valid values in process graph. Values are valid when they are part of the PO entology and child of http://purl.obolibrary.org/obo/PO_0009012

        // Study has collection date header in process graph
        ARCExpect.validationCase (TestID.Name "collection date header exists") {
            cdTokens
            |> Validate.ParamCollection.ContainsParamWithValueTerm cdTerm
        }

        // Study has collection date with valid values in process graph. Values are valid when they match the collection date Regex above
        ARCExpect.validationCase (TestID.Name "collection date values are valid") {
            cdTokens[1 ..]
            |> Validate.ParamCollection.AllTermsSatisfyPredicate (
                Param.getValueAsString
                >> fun v -> cdRegex.Match(v).Success
            )
        }

        // Study has geographic location (country and/or sea) header in process graph
        ARCExpect.validationCase (TestID.Name "geographic location (country and/or sea) header exists") {
            glcaosTokens
            |> Validate.ParamCollection.ContainsParamWithValueTerm glcaosTerm
        }

        // Study has geographic location (country and/or sea) with valid values in process graph. Values are valid when they match one of the given values above
        ARCExpect.validationCase (TestID.Name "geographic location (country and/or sea) values are valid") {
            glcaosTokens[1 ..]
            |> Validate.ParamCollection.AllTermsSatisfyPredicate (
                Param.getValueAsString
                >> fun v -> Array.contains v glcaosValues
            )
        }

        // Study has geographic location (latitude) header in process graph
        ARCExpect.validationCase (TestID.Name "geographic location (latitude) header exists") {
            gllaTokens
            |> Validate.ParamCollection.ContainsParamWithValueTerm gllaTerm
        }

        // Study has geographic location (latitude) with valid values in process graph. Values are valid when they match the geographic location Regex for longitude and latitude above
        ARCExpect.validationCase (TestID.Name "geographic location (latitude) values are valid") {
            gllaTokens[1 ..]
            |> Validate.ParamCollection.AllTermsSatisfyPredicate (
                Param.getValueAsString
                >> fun v -> gllRegex.Match(v).Success
            )
        }

        // Study has geographic location (longitude) header in process graph
        ARCExpect.validationCase (TestID.Name "geographic location (longitude) header exists") {
            glloTokens
            |> Validate.ParamCollection.ContainsParamWithValueTerm glloTerm
        }

        // Study has geographic location (longitude) with valid values in process graph. Values are valid when they match the geographic location Regex for longitude and latitude above
        ARCExpect.validationCase (TestID.Name "geographic location (longitude) values are valid") {
            glloTokens[1 ..]
            |> Validate.ParamCollection.AllTermsSatisfyPredicate (
                Param.getValueAsString
                >> fun v -> gllRegex.Match(v).Success
            )
        }

        // Study has plant growth medium header in process graph
        ARCExpect.validationCase (TestID.Name "plant growth medium header exists") {
            pgmTokens
            |> Validate.ParamCollection.ContainsParamWithValueTerm pgmTerm
        }

        // Study has plant growth medium with not-empty values in process graph
        ARCExpect.validationCase (TestID.Name "plant growth medium values are not empty") {
            pgmTokens[1 ..]
            |> Validate.ParamCollection.AllTermsSatisfyPredicate (
                Param.getValueAsString
                >> System.String.IsNullOrEmpty
                >> not
            )
        }

        // Study has isolation and growth condition header in process graph
        ARCExpect.validationCase (TestID.Name "isolation and growth condition header exists") {
            iagcTokens
            |> Validate.ParamCollection.ContainsParamWithValueTerm iagcTerm
        }

        // Study has isolation and growth condition with not-empty values in process graph
        ARCExpect.validationCase (TestID.Name "isolation and growth condition values are not empty") {
            iagcTokens[1 ..]
            |> Validate.ParamCollection.AllTermsSatisfyPredicate (
                Param.getValueAsString
                >> System.String.IsNullOrEmpty
                >> not
            )
        }
    ]


// Execution:

Setup.ValidationPackage(
    metadata = Setup.Metadata(PACKAGE_METADATA),
    CriticalValidationCases = [studyCases]
)
|> Execute.ValidationPipeline(
    basePath = arcDir
)