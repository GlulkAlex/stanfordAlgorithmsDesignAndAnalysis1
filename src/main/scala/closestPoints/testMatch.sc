val strJSON ="""{"coordinates":[
  {"x":"349.26", "y":"241.13"},
  {"x":"205.69", "y":"830.78"},
  {"x":"890.78", "y":"600.81"},
  {"x":"147.82", "y":"956.2"},
  {"x":"468.45", "y":"517.37"},
  {"x":"6.18", "y":"969.61"},
  {"x":"186.22", "y":"176.6"},
  {"x":"683.86", "y":"750.44"},
  {"x":"341.44", "y":"586.26"},
  {"x":"235.79", "y":"295.24"}
  ]}"""

/*extractors*/
val xExtractor =
  """.+(\d+\.\d{2}).+""".r
val yExtractor =
  """y.:.(\d+\.\d{2})""".r
val xyExtractor =
  """\D+(\d+\.\d{2})\D+(\d+\.\d{2}).+""".r
//val arcTailHeadExtractor =
  //"""\D+(\d+)\D+(\d+).+""".r
val digitsExtractor =
  """.+(\d+\.\d{2}).+""".r

def extractX(expr: String): String =
  expr match {
  //case (arcTailHeadExtractor(arcTail,arcHead)) =>
    //s"'arcTail' = $arcTail, 'arcHead' = $arcHead"
  case (xyExtractor(x,y)) =>
    s"'x' = $x, 'y' = $y"
  case (xExtractor(x)) =>
    s"'x' = $x"
  case (yExtractor(y)) =>
    s"'y' = $y"
  case (digitsExtractor(d)) =>
    s"'d' = $d"
  case _ => "no digits Nor 'x' nor 'y' found"
}

//extractX(expr = """ 7  9""")
extractX(expr = """{"x":"6.18", "y":"969.61"},""")
extractX(expr = """{"x":"235.79", "y":"295.24"}""")
extractX(expr = """{"coordinates":[""")
/*filter criteria*/
val partfunc1: PartialFunction[String, String] = {
  case x if x.exists(_ =='x') => x
  // no case for Silver(),
  // because we're only interested in Gold()
}
strJSON.lines.collect(partfunc1).mkString("\n")
val coord = """([x|y])\D+(\d+\.\d{2})""".r
val x =
  """{"x":"349.26", "y":"241.13"}""" match {
    case coord(x, y) => x
    case _ => ""
}
//import scala.util.parsing.json.
val regEx = scala.util.matching.Regex
val regExMatch = scala.util.matching.Regex.Match
val regExGroups = scala.util.matching.Regex.Groups
//JSON.parseFull(strJSON)
/*'key' -> before ':' in double quotes*/
/*'value' ->
after 'key' & ':'
before ',' or '}' in double quotes*/
case class Coordinate(
                       dimension: String,
                       position: Double//String
                       )
case class Coordinates(
                        x: Coordinate,
                        y: Coordinate
                        )
List("US$", "CAN$")
.map(scala.util.matching.Regex.quote)
.mkString("|").r
val input: String =
"CURRENCY 123\n" +
  "CURRENCY 456"
"CURRENCY".r.replaceAllIn(input, regEx quoteReplacement "US$")
"""\w+""".r replaceAllIn ("A simple example.", _ match {
  case regExMatch(s) => s.toUpperCase })
val date = """(\d\d\d\d)-(\d\d)-(\d\d)""".r
val text = "The doc spree happened on 2011-07-15."
val day = date replaceAllIn(text, _ match {
  case regExGroups(_, month, day) => s"$month/$day" })
