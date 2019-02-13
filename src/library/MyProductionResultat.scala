
package library

class MyProductionResultat {
   def resultat2html(l:List[(String,String)]):Html = {
    Tag("html",List(), List(Tag("head",List(), List(Tag("meta charset=\"UTF-8\"",List(("content","text/html"),("charset","iso-8859-1")),List() ),
                                    Tag("title",List(),List(Text("Résultats de la recherche"))))),
                        Tag("body",List(),tolist(l))))
  }
 
  def tolist(l: List[(String,String)]): List[Html] = {
    var ex: List[Html] = List()
    if (l.isEmpty) {
      ex = ex.::(Tag("h4",List(),List(Text("pas de resultats"))))
      return ex
    }
   
    for (c <- l) {
      ex = ex.::(Tag("br", List(), List()))
      ex = ex.::(Tag("a",List(("href",c._2)),List(Text(c._1))))
     
    }
   
    ex = ex.::(Tag("h4",List(),List(Text("Nombre de résulats : "+l.length))))
   
    ex
  }
}