package scion.util

import scion.util.Result.{Error, Failure, Issue, SimpleError, SimpleWarning, ThrowableError, Warning}

trait Result[+V] {

  def get: V = valueOpt.get

  def valueOpt: Option[V]

  def issues: Seq[Issue]

  def warnings: Seq[Warning] = issues.collect({ case warning: Warning => warning })

  def errors: Seq[Error] = issues.collect({ case error: Error => error })

  def withWarning(warning: Warning): Result[V]

  def withWarning(message: String): Result[V] = withWarning(SimpleWarning(message))

  def failWithError(error: Error): Failure = Failure(issues :+ error)

  def failWithError(message: String): Failure = failWithError(SimpleError(message))

  def failWithError(throwable: Throwable): Failure = failWithError(ThrowableError(throwable))

  def map[R](function: V => R): Result[R]

  def flatMap[R](function: V => Result[R]): Result[R]

  def func2[V2, R](result2: Result[V2])(function2: (V, V2) => R): Result[R]
}

object Result {

  def forValue[V](value: V): Success[V] = Success[V](value)

  def forErrorMessage(message: String): Failure = Failure(Seq(SimpleError(message)))

  def forThrowable(throwable: Throwable): Failure = Failure(Seq(ThrowableError(throwable)))

  case class Success[+V](value: V, issues: Seq[Issue] = Seq.empty) extends Result[V] {
    override def valueOpt: Option[V] = Some(value)

    override def withWarning(warning: Warning): Result[V] = copy(issues = issues :+ warning)

    override def map[R](function: V => R): Result[R] = Success(function(value), issues)

    override def flatMap[R](function: V => Result[R]): Result[R] = {
      function(value) match {
        case Success(mappedValue, moreIssues) => Success(mappedValue, issues ++ moreIssues)
        case Failure(moreIssues) => Failure(issues ++ moreIssues)
      }
    }

    override def func2[V2, R](result2: Result[V2])(function2: (V, V2) => R): Result[R] = {
      result2 match {
        case Success(value2, issues2) => Success(function2(value, value2), issues ++ issues2)
        case Failure(issues2) => Failure(issues ++ issues2)
      }
    }
  }

  case class Failure(issues: Seq[Issue]) extends Result[Nothing] {
    override def valueOpt: Option[Nothing] = None

    override def withWarning(warning: Warning): Result[Nothing] = copy(issues = issues :+ warning)

    override def map[R](function: Nothing => R): Result[R] = this

    override def flatMap[R](function: Nothing => Result[R]): Result[R] = this

    override def func2[V2, R](result2: Result[V2])(function2: (Nothing, V2) => R): Result[R] =
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

}
