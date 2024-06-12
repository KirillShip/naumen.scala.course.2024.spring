import cats._
import cats.implicits._

/*
  Задание №2
  Всё просто, для каждого кейс класса необходимо описать логику его сложения.
  Радиус-вектор должен складываться, как и любой другой вектор.
  GradeAngle всегда выражает угол [0, 360).
  SquareMatrix просто сложение квадратных матриц
 */
object Task2 extends App {
  case class RadiusVector(x: Int, y: Int)
  object RadiusVector {
    implicit val monoid: Monoid[RadiusVector] = ???
    implicit val monoid: Monoid[RadiusVector] = new Monoid[RadiusVector] {
      override def empty: RadiusVector = RadiusVector(0, 0)

      override def combine(vec1: RadiusVector, vec2: RadiusVector): RadiusVector =
        RadiusVector(vec1.x + vec2.x, vec1.y + vec2.y)
    }
  }
  case class DegreeAngle(angel: Double)
  object DegreeAngle {
    implicit val monoid: Monoid[DegreeAngle] = ???
    implicit val monoid: Monoid[DegreeAngle] = new Monoid[DegreeAngle] {
      override def empty: DegreeAngle = DegreeAngle(0)

      override def combine(a1: DegreeAngle, a2: DegreeAngle): DegreeAngle = DegreeAngle((a1.angel + a2.angel) % 360)
    }
  }

  case class SquareMatrix[A : Monoid](values: ((A, A, A), (A, A, A), (A, A, A)))
  object SquareMatrix {
    implicit def monoid[A: Monoid]: Monoid[SquareMatrix[A]] = ???
    implicit def monoid[A: Monoid]: Monoid[SquareMatrix[A]] = new Monoid[SquareMatrix[A]] {
      override def empty: SquareMatrix[A] = {
        val row = (Monoid[A].empty, Monoid[A].empty, Monoid[A].empty)
        SquareMatrix(row.copy(), row.copy(), row.copy())
      }

      override def combine(m1: SquareMatrix[A], m2: SquareMatrix[A]): SquareMatrix[A] = {
        new SquareMatrix[A](m1.values.combine(m2.values))
      }
    }
  }

  val radiusVectors = Vector(RadiusVector(0, 0), RadiusVector(0, 1), RadiusVector(-1, 1))
  Monoid[RadiusVector].combineAll(radiusVectors) // RadiusVector(-1, 2)

  val gradeAngles = Vector(DegreeAngle(380), DegreeAngle(60), DegreeAngle(30))
  Monoid[DegreeAngle].combineAll(gradeAngles) // GradeAngle(90)

  val matrixes = Vector(
    SquareMatrix(
      (
        (1, 2, 3),
        (4, 5, 6),
        (7, 8, 9)
      )
    ),
    SquareMatrix(
      (
        (-1, -2, -3),
        (-3, -4, -5),
        (-7, -8, -9)
      )
    )
  )
  Monoid[SquareMatrix[Int]].combineAll(matrixes)
  //  [0, 0, 0]
  //  |1, 1, 1|
  //  [0, 0, 0]
}
