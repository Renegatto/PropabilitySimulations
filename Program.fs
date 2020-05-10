// Learn more about F# at https://fsharp.org
// See the 'F# Tutorial' project for more help.
open PropabilityCalculations
open K_from_N_in_F_has_size
open Infrastructure

let F x y = y
let check () =
    prepareData 20 5 7 ()
    |> fun x -> F (printfn "%A" x) x
    |> IO.map (isHypothesisRight 1)


[<EntryPoint>]
let main argv =
    printfn "check %A" (check ())
    printfn "calc %A" <| calculate 20. 5. 7. 3
    printfn "sim %A" <| simul_K_from_N_in_M_from_N_has_size 1 ()
    System.Console.ReadLine () |> ignore
    0 // return an integer exit code
