open System.Globalization
open System.Threading

let culture = CultureInfo("en-GB")  
Thread.CurrentThread.CurrentCulture <- culture

Day10_A.run()
Day10_B.run()