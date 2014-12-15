module Medidas

[<Measure>] type kg //define a unit of measure
let myMass = 80.0<kg> //define a mass in kg

[<Measure>] type m //define a unit of measure
[<Measure>] type s //define a unit of measure
let g = 9.8<m/s^2>  //define gravitational acceleration (with units)
let myWeight = myMass * g // result is 784.0<kg m/s ^ 2>

//ERROR: The unit of measure 'm' does not match the unit of measure 'kg m/s ^ 2'
//let stupidError = myWeight + myMass

//[<Measure>] type lb 
let lb = 2.8<kg>



let t = 30.0<s>

let r = myWeight * t



[<Measure>] type degC // temperature, Celsius/Centigrade
[<Measure>] type degF // temperature, Fahrenheit

let convertCtoF ( temp : float<degC> ) = 9.0<degF> / 5.0<degC> * temp + 32.0<degF>
let convertFtoC ( temp: float<degF> ) = 5.0<degC> / 9.0<degF> * ( temp - 32.0<degF>)

// Define conversion functions from dimensionless floating point values.
let degreesFahrenheit temp = temp * 1.0<degF>
let degreesCelsius temp = temp * 1.0<degC>

printfn "Enter a temperature in degrees Fahrenheit."
let input = -40.0<degF>
printfn "That temperature in Celsius is %8.2f degrees C." ((convertFtoC (input))/(1.0<degC>))
