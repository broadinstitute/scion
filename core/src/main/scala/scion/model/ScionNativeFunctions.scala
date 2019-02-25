package scion.model

import scion.util.ResultWithIssues

object ScionNativeFunctions {

  val load: ScionFunction = new ScionFunction {
    override val signature: ScionFunctionSignature =
      ScionFunctionSignature("load", ScionType.content,
        Map("file" -> ScionType.file),
        Set(Set("file"))
      )

    override def apply(arguments: Map[String, ScionValue]): ScionValue = {
      val fileValue = arguments("file").asInstanceOf[ScionValue.FileValue]
      ScionValue.ContentValue(fileValue.value.contentAsString)
    }
  }

  val insert: ScionFunction = new ScionFunction {
    override val signature: ScionFunctionSignature =
      ScionFunctionSignature("insert", ScionType.content,
        Map(
          "template" -> ScionType.content,
          "insert" -> ScionType.content,
          "before" -> ScionType.string,
          "after" -> ScionType.string
        ),
        Set(Set("template"), Set("insert"), Set("before", "after"))
      )

    override def apply(arguments: Map[String, ScionValue]): ScionValue = {
      val templateValue = arguments("template").asInstanceOf[ScionValue.ContentValue]
      val insertValue = arguments("insert").asInstanceOf[ScionValue.ContentValue]
      val beforeValue = arguments("before").asInstanceOf[ScionValue.StringValue]
//      val afterValue = arguments("after").asInstanceOf[ScionValue.ContentValue]
      val template = templateValue.value
      val insert = insertValue.value
      val before = beforeValue.value
      val insertionPos = template.indexOf(before) + before.size
      val content = if(insertionPos < 0) {
        template
      } else {
        template.substring(0, insertionPos) + insert + template.substring(insertionPos)
      }
      ScionValue.ContentValue(content)
    }
  }

  val save: ScionFunction = new ScionFunction {
    override val signature: ScionFunctionSignature = ScionFunctionSignature("save", ScionType.unit,
      Map(
        "file" -> ScionType.file,
        "content" -> ScionType.content
      ),
      Set(Set("file"), Set("content"))
    )

    override def apply(arguments: Map[String, ScionValue]): ScionValue = {
      val fileValue = arguments("file").asInstanceOf[ScionValue.FileValue]
      val contentValue = arguments("content").asInstanceOf[ScionValue.ContentValue]
      val file = fileValue.value
      val content = contentValue.value
      file.write(content)
      ScionValue.UnitValue
    }
  }
  val dir: Map[String, ScionFunction] = Map("load" -> load, "insert" -> insert, "save" -> save, "insert" -> insert)

  def get(name: String): ResultWithIssues[ScionFunction] = {
    dir.get(name) match {
      case Some(function) => ResultWithIssues.forValue(function)
      case None => ResultWithIssues.forErrorMessage(s"$name is not a recognized function.")
    }
  }

}
