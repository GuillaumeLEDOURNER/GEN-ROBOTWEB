package library

class MyAnalysePage 
{
  
  val myFiltrageURL:MyFiltrageURL = new MyFiltrageURL()
  val myFiltrageHtml:MyFiltrageHtml = new MyFiltrageHtml()
  
  def resultats(url:String, exp:Expression):List[(String,String)] = 
    {
      val html:Html = UrlProcessor.fetch(url)
      
      val links:List[String] = myFiltrageURL.filtreAnnonce(html)
      
       
      var results:List[(String,Html)] = Nil
      
      val linksLeft = links.length
      var counter = 0
      var percentage = 0.0
      var currentBars = 0
      print("Progress : ")
      
    /*  for (i <- 1 to results.length){
         print("Progress link "+i+" : ")*/
      for (link <- links)
      {
           results ::= (link,UrlProcessor.fetch(link))
           counter += 1
           
           percentage = counter.asInstanceOf[Float] / linksLeft.asInstanceOf[Float] * 100

           val barsToPrint = percentage.asInstanceOf[Int] - currentBars

           for(i <- 0 to barsToPrint) {
             print("|")
             currentBars += 1
           }
      }

      if(currentBars == 0) {
        for(i <- 0 to 100) {
          print("|")
        }
      }

      println(" 100%")

      var matches:List[(String,Html)] = Nil
      
      for (result <- results)
      {
           if (myFiltrageHtml.filtreHtml(result._2, exp) && !doublons(result, matches))
             matches ::= result
      }
      
      var r:List[(String,String)] = Nil
      
      for (m <- matches)
      {
          r ::= (getTitleFromUrl(m._1),m._1)
      }
  
       r
    }
  
    private def doublons(s : (String, Html), m : List[(String, Html)]) : Boolean = {
      
      if (s._1.contains("new&search=1&start_field=")){
        return true
      }
      
      for(a <- m) {
        if(s._1.equals(a._1)) {
          return true
        }
      }
      return false
    }

    private def getTitleFromUrl(url:String) : String = 
    {
      val s:Array[String] =  url.split("/")
      s(5).replace("-", " ")
    }
    
    private def getTitle(html: Html): String = 
    {
       
        html match
        {
          case Tag(name,attributes,children)  =>  ""

          case Text(t) => ""  
        }
       
    }
    
    private def endWithNumber(value:String) : Boolean = 
    {
        return value.matches("^.*\\d$")
    }
}