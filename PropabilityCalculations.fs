module PropabilityCalculations
let sum_independent (a:float) (b:float):float = a+b- (a*b)
let sum_indep_many (times:int) (prop:float): float = List.map (fun _ -> prop) [1..times] |> List.reduce sum_independent

let facs: bigint seq = seq {
    let mutable acc = 1I
    let mutable i = 0I
    while true do 
        i <- (i + 1I)
        acc <- (acc * i)
        yield acc
}

let rec fac n = Seq.take n facs |> Seq.last
let F x y = y
let ffac: float -> float = round >> int >> fac >> float 

let C (k:float) (n:float) = F (printfn "%A %A" k n) (ffac n / ffac (n - k)) |> fun x -> F (printfn "%A" x) x

let calculate n k m s =
    let P_k = k/n
    let P_m = m/k 
    let P_f = P_k/P_m
    let P_s = sum_indep_many s P_f

    printfn "%A and %A" (sum_indep_many s P_f)  <| C (float s) (C k n) / C n n 

    P_s
