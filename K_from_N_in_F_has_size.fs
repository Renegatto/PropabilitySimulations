module K_from_N_in_F_has_size
open Infrastructure
//propability estimation
let simulate (times:int) (result_generator: unit -> bool IO): (int * float) IO =
    List.map (fun _ -> result_generator ()) [1..times]
    |> List.map (IO.map (fun x -> if x then 1 else 0))
    |> List.reduce (IO.map2 (+))
    |> IO.map (fun successes -> successes, (float successes / float times) )  
//randoms
let shuffle (rng: System.Random) (xs: 'a list): list<'a>*System.Random = 
    List.sortBy (fun x -> rng.Next (0, List.length xs)) xs, rng
let sample (count:int) (xs:'a list) (rng: System.Random): (list<'a>*System.Random) IO =
    shuffle rng xs
    |> fun (shuffled, rng') -> List.take (count) shuffled, rng'
    |> IO
//logic
let isHypothesisRight (size:int) (k:int Set, m: int Set): bool =
    Set.intersect k m
    |> Set.count
    |> fun x -> x > size

let prepareData (n_length: int) (k_length: int) (m_length: int) ()
    : (Set<int>*Set<int>) IO =

    let rng: System.Random = new System.Random ()

    let n: int list = [1..n_length]

    let k': (list<int>*System.Random) IO = sample k_length n rng
    let m': (list<int>*System.Random) IO = IO.bind (sample m_length n << snd) k'

    let k: Set<int> IO = IO.map (fst>>Set) k'
    let m: Set<int> IO = IO.map (fst>>Set) m'

    IO.map2 (fun x y -> x,y) k m
//usage
let simul_K_from_N_in_M_from_N_has_size (): unit IO =
    (IO.map (isHypothesisRight 3) << prepareData 20 5 7)
    |> simulate 200000
    |> IO.map (fun (successes, propability) -> 
        printfn "Simulation, successes/all = %A/%A, P(x) = %A" successes 200000 propability)
