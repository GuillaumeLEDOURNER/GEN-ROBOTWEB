package library

class MyFiltrageHtml extends FiltrageHtml {
 

  /** A partir d'un document Html h et d'une requête e, dit si le document
   satisfait l'expression e 
   @param h le document Html
   @param e l'expression 
   @return true si le document satisfait l'expression e
  */
  def filtreHtml(h:Html,e:Expression):Boolean ={
	  filtreHtmlexp(h,e)
    }

  /**
   * Decompose l'expression e en supprimmant tous les and et or
   */
  def filtreHtmlexp(h:Html,e:Expression):Boolean ={
    e match {
      case And(a,b) => filtreHtmlexp(h,a) && filtreHtmlexp(h,b)
      case Or(a,b) => filtreHtmlexp(h,a)||filtreHtmlexp(h,b)
      case Word(a) => filtreHtml2(h,a)
      
    }
   }

  /** La fonction regarde si l'expression e est dans le document html 
   * @param h le document Html  
   * @param e le string
   * @return vrai si l'expression se trouve dans le document Html
   */
  def filtreHtml2(h:Html,e:String): Boolean ={
    var page = new MyHtml2String().process(h)
    page.contains(e)
  }                           				
}