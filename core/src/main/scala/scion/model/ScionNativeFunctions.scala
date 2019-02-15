package scion.model

object ScionNativeFunctions {

  val load: ScionFunction =
    ScionFunction("load", ScionType.content,
      Map("file" -> ScionType.file),
      Set(Set("file"))
    )

  val insert: ScionFunction =
    ScionFunction("insert", ScionType.content,
      Map(
        "template" -> ScionType.content,
        "insert" -> ScionType.content,
        "before" -> ScionType.string,
        "after" -> ScionType.string
      ),
      Set(Set("template"), Set("insert"), Set("before", "after"))
    )

  val save: ScionFunction =
    ScionFunction("save", ScionType.unit,
      Map(
        "file" -> ScionType.file,
        "content" -> ScionType.content
      ),
      Set(Set("file"), Set("content"))
    )

  val dir: Map[String, ScionFunction] = Map("load" -> load, "insert" -> insert, "save" -> save, "insert" -> insert)

}
