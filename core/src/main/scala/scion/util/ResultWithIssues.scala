package scion.util

import scion.util.ResultWithIssues.{Error, Failure, Issue, SimpleError, SimpleWarning, ThrowableError, Warning}

trait ResultWithIssues[+V] {
  def wasSuccess: Boolean

  def wasFailure: Boolean

  def get: V = valueOpt.get

  def valueOpt: Option[V]

  def issues: Seq[Issue]

  def warnings: Seq[Warning] = issues.collect({ case warning: Warning => warning })

  def errors: Seq[Error] = issues.collect({ case error: Error => error })

  def withWarning(warning: Warning): ResultWithIssues[V]

  def withWarning(message: String): ResultWithIssues[V] = withWarning(SimpleWarning(message))

  def failWithError(error: Error): Failure = Failure(issues :+ error)

  def failWithError(message: String): Failure = failWithError(SimpleError(message))

  def failWithError(throwable: Throwable): Failure = failWithError(ThrowableError(throwable))

  def map[R](function: V => R): ResultWithIssues[R]

  def flatMap[R](function: V => ResultWithIssues[R]): ResultWithIssues[R]

  def func2[V2, R](result2: ResultWithIssues[V2])(function2: (V, V2) => R): ResultWithIssues[R]
}

object ResultWithIssues {

  def forValue[V](value: V): Success[V] = Success[V](value)

  def forErrorMessage(message: String): Failure = Failure(Seq(SimpleError(message)))

  def forThrowable(throwable: Throwable): Failure = Failure(Seq(ThrowableError(throwable)))

  class Box[V](var result: ResultWithIssues[V]) {
    def update(updater: V => V): Unit = {
      result = result.map(updater)
    }
    def flatUpdate(updater: V => ResultWithIssues[V]): Unit = {
      result = result.flatMap(updater)
    }
    def insert[V2](result2: ResultWithIssues[V2])(inserter: (V, V2) => V): Unit = {
      result = result.func2[V2, V](result2)(inserter)
    }
    def insertOptional[V2](result2: ResultWithIssues[Option[V2]])(inserter: (V, V2) => V): Unit = {
      val optionalInserter: (V, Option[V2]) => V = {
        case (value, Some(value2)) => inserter(value, value2)
        case (value, None) => value
      }
      result = result.func2[Option[V2], V](result2)(optionalInserter)
    }
  }

  object Box {
    def forValue[V](value: V): Box[V] = new Box(ResultWithIssues.forValue(value))
  }

  case class Success[+V](value: V, issues: Seq[Issue] = Seq.empty) extends ResultWithIssues[V] {
    override def wasSuccess: Boolean = true

    override def wasFailure: Boolean = false

    override def valueOpt: Option[V] = Some(value)

    override def withWarning(warning: Warning): ResultWithIssues[V] = copy(issues = issues :+ warning)

    override def map[R](function: V => R): ResultWithIssues[R] = Success(function(value), issues)

    override def flatMap[R](function: V => ResultWithIssues[R]): ResultWithIssues[R] = {
      function(value) match {
        case Success(mappedValue, moreIssues) => Success(mappedValue, issues ++ moreIssues)
        case Failure(moreIssues) => Failure(issues ++ moreIssues)
      }
    }

    override def func2[V2, R](result2: ResultWithIssues[V2])(function2: (V, V2) => R): ResultWithIssues[R] = {
      result2 match {
        case Success(value2, issues2) => Success(function2(value, value2), issues ++ issues2)
        case Failure(issues2) => Failure(issues ++ issues2)
      }
    }
  }

  case class Failure(issues: Seq[Issue]) extends ResultWithIssues[Nothing] {
    override def wasSuccess: Boolean = false

    override def wasFailure: Boolean = true

    override def valueOpt: Option[Nothing] = None

    override def withWarning(warning: Warning): ResultWithIssues[Nothing] = copy(issues = issues :+ warning)

    override def map[R](function: Nothing => R): ResultWithIssues[R] = this

    override def flatMap[R](function: Nothing => ResultWithIssues[R]): ResultWithIssues[R] = this

    override def func2[V2, R](result2: ResultWithIssues[V2])(function2: (Nothing, V2) => R): ResultWithIssues[R] =
      Failure(issues ++ result2.issues)
  }

  trait Issue {
    def message: String

    def isError: Boolean
  }

  trait Warning extends Issue {
    override def isError: Boolean = false
  }

  case class SimpleWarning(message: String) extends Warning

  trait Error extends Issue {
    override def isError: Boolean = true
  }

  case class SimpleError(message: String) extends Error

  case class ThrowableError(throwable: Throwable) extends Error {
    override def message: String = throwable.getMessage
  }

  def fromTrying[V](thunk: =>V): ResultWithIssues[V] = {
    try {
      ResultWithIssues.forValue(thunk)
    } catch {
      case exception: Exception => ResultWithIssues.forThrowable(exception)
    }
  }

  def consolidateMap[K, V](map: Map[K, ResultWithIssues[V]]): ResultWithIssues[Map[K, V]] = {
    val issues = map.values.toSeq.flatMap(_.issues)
    if(map.values.forall(_.wasSuccess)) {
      val simpleMap = map.mapValues(_.get).view.force
      ResultWithIssues.Success(simpleMap, issues)
    } else {
      ResultWithIssues.Failure(issues)
    }
  }

}
