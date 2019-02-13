package library

import java.io.FileWriter

object Main extends App
{
   
  val expression = traductiontotale(ExpressionParser.readExp)
  
  val urlHead = "https://search.vivastreet.com/annonces/fr?lb=new&search=1&start_field=1&keywords="
  val urlTail = "&cat_1=&geosearch_text=&searchGeoId=0"
  
  println(expression)
  
  val requests = listRequests(expression)
  
  println(requests)
  
  val analyse = new MyAnalysePage()
  
  var titreUrl = List[(String, String)]()
  
  for(request <- requests) {
    titreUrl = titreUrl ++ analyse.resultats(request, expression)
  }
  
  val production = new MyProductionResultat()
  
  val resultatHtml = production.resultat2html(titreUrl)
  
  val html2String = new MyHtml2String()
  
  val resultatString = html2String.process(resultatHtml.asInstanceOf[Tag])
  
  var file = new FileWriter("resultat.html")
  try {
    file.write(resultatString)
    println("File created")
  } finally file.close()
  
  def traduction(e : Expression) : Expression = {
    e match {
      case And(e,Or(f,g)) => Or(And(traduction(e),traduction(f)),And(traduction(e),traduction(g)))
      case And(Or(e,f),g) => Or(And(traduction(e),traduction(g)),And(traduction(f),traduction(g)))
      case And(e,f) => And(traduction(e),traduction(f))
      case Or(e,f) => Or(traduction(e),traduction(f))
      case Word(s) => Word(s)
    }
  }
  
  def traductiontotale(e : Expression) : Expression = {
    var resultat = traduction(e)
    while (traduction(resultat)!=resultat) {
      resultat=traduction(resultat)
      
    }
    resultat
  }
  def listRequests(exp : Expression) : List[String] = {
    exp match {
      case And(e,f)=> {
        val keywordList = processAnd(exp)
        var result = ""
        for(keyword <- keywordList) {
          result += keyword + '+'
        }
        
        urlHead+result+urlTail::Nil
      }
      case Or(e, f) => listRequests(e)++listRequests(f)
      case Word(s) => urlHead+s+urlTail::Nil
    }
  }
  
  def processAnd(exp : Expression) : List[String] = {
    exp match {
      case And(e, f) => processAnd(e) ++ processAnd(f)
      case Word(e) => List(e)
    }

  }
}