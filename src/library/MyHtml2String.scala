package library

class MyHtml2String {
  def process(html : Tag): String = {
    var opening = '<' + html.name
    
    for(a <- html.attributes) {
      opening += (" " + a._1 + "=" + a._2)
    }
    
    // Test commit
    
    opening += '>'
    
    val closing = "</" + html.name + '>'
    
    var parsedChildren = List[String]()
    
    for(child <- html.children) {
      if(child.isInstanceOf[Tag]) {
        parsedChildren ::= process(child.asInstanceOf[Tag])
      } else {
        parsedChildren ::= child.asInstanceOf[Text].content
      }
    }
    
    var result = opening
    for(element <- parsedChildren.reverse) {
      result += element
    }
    result += closing
    
    return result
  }
}