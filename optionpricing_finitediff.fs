open System

let callPayoff strike spot = max (spot - strike) 0.0
let putPayoff strike spot = max (strike - spot) 0.0

let triplewise (source: seq<_>) =
  source
    |> Seq.windowed 3 
    |> Seq.map (fun triple -> (triple.[0], triple.[1], triple.[2]))

let calcAssetStep strike numberAssetSteps =
  2.0 * strike / float numberAssetSteps // 'infinity' is twice the strike

let calcTimeStep vol numberAssetSteps expiration =
  let timeStep = 0.9 / (vol * vol) / float (numberAssetSteps * numberAssetSteps) // For stability
  let numberTimeSteps = (int (expiration / timeStep)) + 1
  let dT = (expiration / float numberTimeSteps) // ensure expiration is an integer number of time steps away
  (dT, numberTimeSteps)

let optionValue vol intRate payoff strike expiration numberAssetSteps =
  let payoffForStrike = payoff strike // partially apply
  let dS = calcAssetStep strike numberAssetSteps
  let timeStep = calcTimeStep vol numberAssetSteps expiration
  let dT = fst timeStep
  let numberTimeSteps = snd timeStep
  let S = [|for i in 0 .. numberAssetSteps -> float i * dS|]
  let initialValues = [|for i in 0 .. numberAssetSteps -> payoffForStrike S.[i]|]

  let calc (prev, curr, next) S =
    let delta = (next - prev) / 2.0 / dS; // central difference
    let gamma = (next - 2.0 * curr + prev) / dS / dS // central difference
    let theta = -0.5 * vol * vol * S * S * gamma - intRate * S * delta + intRate * curr // Black-Scholes
    curr - float dT * theta

  let calc (k, prevValues:float[]) =
    if k = (numberTimeSteps + 1) then
      None
    else
      let values = prevValues
                     |> Seq.ofArray
                     |> triplewise
                     |> Seq.mapi (fun idx vals -> calc vals S.[idx + 1])
      let b0 = prevValues.[0] * (1.0 - intRate * float dT) // boundary condition at S=0
      let placeHolder = 0.0
      let valuesSeq = seq {
                        yield b0
                        yield! values
                        yield placeHolder }
      let valuesArray = Seq.toArray valuesSeq
      let b1 = 2.0 * valuesArray.[numberAssetSteps - 1] - valuesArray.[numberAssetSteps - 2] // boundary condition at S=infinity
      valuesArray.[valuesArray.Length - 1] <- b1
      Some(valuesArray, (k+1, valuesArray))

  seq
    { yield initialValues
      yield! Seq.unfold calc (1, initialValues) }

// use gnuplot with the following:
// set zrange [0:120]
// set xlabel "Time"
// set ylabel "Asset"
// set zlabel "Option value"
// splot "out.dat"
let outputVals (filename:string) (dS:float) (dT:float) (vals:seq<float[]>) =
  use textWriter = new System.IO.StreamWriter(filename)
  vals |> Seq.iteri (fun i s ->
    for j = 0 to (s.Length - 1) do
      textWriter.WriteLine("{0} {1} {2}", dT * float i, dS * float j, s.[j]))

[<EntryPoint>]
[<STAThread>]
let main argv = 
    let vol = 0.2 
    let interestRate = 0.05
    let strike = 100.0
    let expiry = 1.0
    let numberAssetSteps = 20
    let vCall = optionValue vol interestRate callPayoff strike expiry numberAssetSteps

    let dS = calcAssetStep strike numberAssetSteps
    let timeStep = calcTimeStep vol numberAssetSteps expiry
    let dT = fst timeStep

    vCall |> outputVals "E:\\temp\\Call.dat" dS dT

    let vPut = optionValue vol interestRate putPayoff strike expiry numberAssetSteps
    vPut |> outputVals "E:\\temp\\Put.dat" dS dT
    0 // return an integer exit code
