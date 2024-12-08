open System.Globalization
open System.Threading

let culture = CultureInfo("en-GB")  
Thread.CurrentThread.CurrentCulture <- culture

Day02_A.run()
Day02_B.run()