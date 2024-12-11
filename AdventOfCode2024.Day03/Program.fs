open System.Globalization
open System.Threading

let culture = CultureInfo("en-GB")  
Thread.CurrentThread.CurrentCulture <- culture

Day03_A.run()
Day03_B.run()