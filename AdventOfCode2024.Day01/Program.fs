open System.Globalization
open System.Threading

let culture = CultureInfo("en-GB")  
Thread.CurrentThread.CurrentCulture <- culture

Day01_A.run()
Day01_B.run()