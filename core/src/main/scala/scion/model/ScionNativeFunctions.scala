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
    }
  }

  val save: ScionFunctionSignature =
    ScionFunctionSignature("save", ScionType.unit,
      Map(
        "file" -> ScionType.file,
        "content" -> ScionType.content
      ),
      Set(Set("file"), Set("content"))
    )

  val dir: Map[String, ScionFunctionSignature] = Map("load" -> load, "insert" -> insert, "save" -> save, "insert" -> insert)

  def get(name: String): ResultWithIssues[ScionFunctionSignature] = {
    dir.get(name) match {
      case Some(function) => ResultWithIssues.forValue(function)
      case None => ResultWithIssues.forErrorMessage(s"$name is not a recognized function.")
    }
  }

}
