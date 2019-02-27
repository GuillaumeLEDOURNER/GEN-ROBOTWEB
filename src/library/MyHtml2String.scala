package library

class MyHtml2String {

  var htmlCast: Tag = _

  def process(html : Html): String = {

    /*try {
      htmlCast = html.asInstanceOf[Tag]
    } catch {
      case _: Throwable => println("Only Tag objects should be passed to MyHtml2String.process() !")
    }*/

    var opening = '<' + htmlCast.name
    
    for(a <- htmlCast.attributes) {
      opening += (" " + a._1 + "=" + a._2)
    }
    
    opening += '>'
    
    val closing = "</" + htmlCast.name + '>'
    
    var parsedChildren = List[String]()
    
    for(child <- htmlCast.children) {
      child match {
        case Tag(_, _, _) => parsedChildren ::= process(child.asInstanceOf[Tag])
        case Text(_) => parsedChildren ::= child.asInstanceOf[Text].content
      }
    }
    
    var result = opening
    for(element <- parsedChildren.reverse) {
      result += element
    }
    result += closing
    
    result
  }
}