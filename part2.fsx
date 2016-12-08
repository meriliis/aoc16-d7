open System
open System.Text.RegularExpressions

let path = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"input.txt")
let input = System.IO.File.ReadAllLines path

let findHypernets (IP : string)=  
    Regex.Matches(IP, "(?<=\[)\w*(?=\])")
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> List.ofSeq

let findSupernets (IP : string) =
    Regex.Matches(IP, "((?<=\])|^)\w+((?=\[)|$)")
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> List.ofSeq

let rec findABAs (s : string) (acc : string list) =
    match s with
    | s when s.Length < 3 -> acc
    | s                   -> if s.Chars(0) = s.Chars(2) && s.Chars(0) <> s.Chars(1) then 
                                findABAs (s.Substring(1)) (s.Substring(0, 3) :: acc)
                             else 
                                findABAs (s.Substring(1)) acc
                           
let isValid (IP : string) =
    let ABAsInSupernets = IP |> findSupernets |> List.map (fun s -> findABAs s []) |> List.concat |> Set.ofList
    let ABAsInHypernets = IP |> findHypernets |> List.map (fun s -> findABAs s []) |> List.concat
    let BABsInHypernets = ABAsInHypernets |> List.map (fun aba -> aba.Substring(1, 1) + aba.Substring(0, 1) + aba.Substring(1, 1)) |> Set.ofList
    
    if Set.intersect ABAsInSupernets BABsInHypernets |> Set.isEmpty then false else true

input
|> Array.filter (fun IP -> isValid IP)
|> Array.length