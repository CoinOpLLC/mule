// Our configuration class hierarchy


sealed trait Shape
case class Circle(radius: Double) extends Shape
// #TODO: what is Case Classy buying here? It can't work with `refined`
type PositiveDouble = Double Refined Positive
case class Rectangle(length: Double, width: PositiveDouble) extends Shape
// case class Rectangle(length: Double, width: Double /* Refined Positive */ ) extends Shape

case class MyConfig(someString: Option[String], shapes: List[Shape])

val decoder1 = deriveDecoder[Config, MyConfig]
val shapes   = decoder1 fromString """shapes = []"""
// res4: Either[classy.DecodeError,MyConfig] = Right(MyConfig(None,List()))

val cfgClassy = decoder1 fromString """
  someString = "hello"
  shapes     = []"""
// res5: Either[classy.DecodeError,MyConfig] = Right(MyConfig(Some(hello),List()))

val moarShapes = decoder1 fromString """shapes = [
  { circle    { radius: 200.0 } },
  { rectangle { length: 10.0, width: 20.0 } }
]"""
// res6: Either[classy.DecodeError,MyConfig] = Right(MyConfig(None,List(Circle(200.0), Rectangle(10.0,20.0))))

// mismatched config
val badCfg = decoder1 fromString """shapes = [
  { rectangle { radius: 200.0 } },
  { circle    { length: 10.0, width: -20.0 } }
]"""
// res: Either[classy.DecodeError,MyConfig] = Left(AtPath(shapes,And(AtIndex(0,Or(AtPath(circle,Missing),List(AtPath(rectangle,And(AtPath(length,Missing),List(AtPath(width,Missing))))))),List(AtIndex(1,Or(AtPath(circle,AtPath(radius,Missing)),List(AtPath(rectangle,Missing))))))))

// error pretty printing
val sinisterOutcome = badCfg fold (
  error => error.toPrettyString,
  conf => s"success: $conf"
)
