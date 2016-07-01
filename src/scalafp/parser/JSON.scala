package scalafp.parser

trait JSON
object JSON {
  class JNull extends JSON
  class JObject(get: Map[String, JSON]) extends JSON
  class JArray(get: IndexedSeq[JSON]) extends JSON
  class JBool(get: Boolean) extends JSON
  class JString(get: String) extends JSON
  class JNumber(get: Double) extends JSON

  //  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
  //    import P._
  //    val array = surround(string("["), string("]"))
  //    val boolean = string("true") or string("false")
  //    val str = s"\w+".r
  //    val number = s"\d_".r
  //    val nothing = string("null")
  //    val obj = surround("{", "}")
  //  }

  //  s"{
  //"Company name" : "Microsoft Corporation",
  //"Ticker" : "MSFT","Active" : true,
  //"Price" : 30.66,
  //"Shares outstanding" : 8.38e9,
  //"Related companies" :
  //[ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
  //}"
}

