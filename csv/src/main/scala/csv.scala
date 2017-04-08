import shapeless._


trait CsvEncoder[A] {
  def encode(value: A): List[String]
}


object CsvEncoder {
  def pure[A](func: A => List[String]): CsvEncoder[A] =
    new CsvEncoder[A] {
      def encode(value: A): List[String] =
        func(value)
    }

  implicit val stringEnc: CsvEncoder[String] =
    pure(str => List(str))

  implicit val intEnc: CsvEncoder[Int] =
    pure(num => List(num.toString))

  implicit val booleanEnc: CsvEncoder[Boolean] =
    pure(bool => List(if (bool) "yes" else "no"))

  implicit val hnilEnc: CsvEncoder[HNil] =
    pure(hnil => Nil)

  implicit def hconsEnc[H, T <: HList](
                                        implicit
                                        hEnc: CsvEncoder[H],
                                        tEnc: CsvEncoder[T]
                                      ): CsvEncoder[H :: T] =
    pure {
      case head :: tail =>
        hEnc.encode(head) ++ tEnc.encode(tail)
    }

  implicit def genericEnc[A, R](
                                 implicit
                                 generic: Generic.Aux[A, R], //Generic[R] = {type Rept = R}
                                 encoder: Lazy[CsvEncoder[R]]
                               ): CsvEncoder[A] =
    pure(a => encoder.value.encode(generic.to(a)))

  implicit val iceCreamEnc: CsvEncoder[IceCream] =
    pure(iceCream => List(iceCream.name, iceCream.numCherries.toString, iceCream.inCone.toString))
}

object Main extends Demo {
  def encodeCsv[A](value: A)(implicit enc: CsvEncoder[A]): List[String] =
    enc.encode(value)

  println(encodeCsv("Dave"))
  println(encodeCsv(123))
  println(encodeCsv(true))

  println(encodeCsv(IceCream("Cornetto", 0, true)))

  val hlist = "Magnum" :: 0 :: true :: HNil
  println(encodeCsv(hlist))

}


final case class Employee(
                           name: String,
                           number: Int,
                           manager: Boolean
                         )

final case class IceCream(
                           name: String,
                           numCherries: Int,
                           inCone: Boolean
                         )

