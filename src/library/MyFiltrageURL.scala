package library

class MyFiltrageURL {
  var aTags = List[Tag]()
  
  def filtreAnnonce(h : Html): List[(String)] = 
  {
      var res: List[String] = Nil
       
        h match
        {
          case Tag(name,attributes,children) 
          
          => attributes match
          {
               case x :: y =>
                 
                 
                   for (n <- children)
                   {
                    
                        if (x._1 == "href" && endWithNumber(x._2))
                        {
                             res = res ++  List((x._2)) ++ filtreAnnonce(n)
                        }
                        else 
                        {
                          res = res ++ filtreAnnonce(n)
                        }  
                        
                   } 
            case Nil => Nil
          }
          
          case Text(t) => Nil
            
        }
        res
  }
    private def endWithNumber(value:String) : Boolean = 
    {
        return value.matches("^.*\\d$")
    }
  def findAllATags(tag : Tag) {
    for(child <- tag.children) {
      if(child.isInstanceOf[Tag]) {
        aTags ::= child.asInstanceOf[Tag]
        findAllATags(child.asInstanceOf[Tag])
      }
    }
  }
  
  def extraireHref(l:List[(String,String)]): List[(String, String)] = {
    var l2 = List[(String, String)]()
    for (ident <- l) {
      if (ident._1=="href") {
        val tmp = (ident._1, ident._2)
        l2 = l2 :+ tmp
      }
    }
    
    return l2
  }
}