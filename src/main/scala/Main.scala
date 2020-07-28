import java.lang.{Long, Integer, Float, Double}
import java.math.{BigDecimal, BigInteger, RoundingMode}
import com.google.common.math._
import scala.util._
import scala.{math => _}

//this seems to be an overhead but is nice to have for testing
trait Ops {
  def countBigInteger(number: BigInteger): Try[scala.Long]
  def countBigDecimal(number: BigDecimal): Try[scala.Long]
  def countFloating(number: Double): Try[scala.Long]
  def countLong(number: Long): Try[scala.Long]
}

trait ImplOps extends Ops {

  //this is known formula, I am not aware of another way that would be safe "enough"
  //i.e. transfrom to String and count it would allocate a lot of memory for big numbers
  //and I am not sure that 2GB of string is big enough to hold a BigInteger, however I didn't went into this
  //also dividing by 10 is not safe or fast enough either for sufficiently big numbers
  //still, we take the formula for granted :), I am not actually count them myself
  //I couldn't find a proper "log" function for BigIntegers in Scala/Java library so I took it from Guava
  def countBigInteger(number: BigInteger): Try[scala.Long] =
    Try(BigIntegerMath.log10(number.abs(), RoundingMode.FLOOR) + 1)

  def countBigDecimal(number: BigDecimal): Try[scala.Long] =
    countBigInteger(number.toBigInteger())

  def countFloating(number: Double): Try[scala.Long] =
    countBigDecimal(BigDecimal.valueOf(number))

  def countLong(number: Long): Try[scala.Long] =
    countBigDecimal(BigDecimal.valueOf(number))
}

object ImplOps extends ImplOps

object Ops {
  def count(number: Number)(ops: Ops): Try[scala.Long] = {
    number match {
      case n: BigInteger =>
        //use the "biggest" type we know that implements Number atm to calculate the actual size
        ops.countBigInteger(n)
      case n: BigDecimal =>
        //this should be safe because BigDecimals in Java are BigIntegers
        //with extra knowledge about the position of decimal point
        ops.countBigDecimal(n)
      case n
          if (n.isInstanceOf[Double] || n
            .isInstanceOf[Float]) =>
        //because of precision, which will be lost the float cases should be handled in a separate case from whole
        //numers, the convertion from float -> double should not loose any precision therefore this should be correct
        ops.countFloating(number.doubleValue())
      case n
          if (n.isInstanceOf[Long] ||
            n.isInstanceOf[Integer] ||
            n.isInstanceOf[Short] ||
            n.isInstanceOf[Byte]) =>
        //do the same for whole numbers, nothing should be lost, when converting byte,short or integers to long'
        //nor when converting the number further to BigDecimal
        ops.countLong(n.longValue())
      case _ =>
        //there are more known types implementing Number, however this is not a "sealed" type
        //by design this function is wrong, therefore the requirments to build such a function,
        //it shouldn't accept "Number" because it can be extended indefinetly
        //to define this function correctly it should only accept only the types that we need using a Sum type
        Failure(new RuntimeException("Don't know the type of this one"))
    }
  }
}

import zio.test._
import zio.duration._
import zio.test.Assertion._

//this is basically the main of our App, it will run when you run the project
//the requirements don't say that the application should accept a string from arguments
//only to implement the function with java.lang.Number as argument, hence,
//I didn't implement String -> Number because that adds to the complexity,
object Spec extends DefaultRunnableSpec {

  def genStringifiedNumber(max: Int = 100, isDecimal: Boolean = false) =
    for {
      //gen a random number which will contains the digit count
      count <- Gen.int(1, max)
      //generate a stream of digits
      number <- Gen.chunkOfN(count)(Gen.int(1, 9))
      //generate a random sign
      sign <- Gen.elements("", "-")
      //generate decimals
      decimals <- Gen.int(1, 10)
      //generate the stringified number based on arguments with the random sign
      strNumber =
        if (isDecimal) s"$sign${number.mkString}.${decimals}"
        else
          s"$sign${number.mkString}"
    } yield (count.toLong, strNumber)
  //return the random size for the number and the stringified number itself

  def sampleNo(
      max: Int = 100,
      isDecimal: Boolean = false
  ) =
    genStringifiedNumber(max, isDecimal).sample
      .map(_.value)
      .runHead
      .get //sample one random value from above

  //this is the entry point
  override def spec =
    suite("Test")(
      suite("test that default implementation calculates number of digits for")(
        testM("positive/negative BigInteger value") {
          check(genStringifiedNumber()) {
            case (c, s) =>
              val number = new BigInteger(s)
              val result = ImplOps.countBigInteger(number)
              assert(result)(isSuccess(equalTo(c)))
          }
        },
        testM("positive/negative BigDecimal value") {
          check(genStringifiedNumber(isDecimal = true)) {
            case (c, s) =>
              val number = new BigDecimal(s)
              val result = ImplOps.countBigDecimal(number)
              assert(result)(isSuccess(equalTo(c)))
          }
        },
        testM("positive/negative float value") {
          check(genStringifiedNumber(max = 8, isDecimal = true)) {
            case (c, s) =>
              val number = Float.parseFloat(s)
              val result = ImplOps.countFloating(number)
              assert(result)(isSuccess(equalTo(c)))
          }
        },
        testM("positive/negative double value") {
          check(genStringifiedNumber(40, isDecimal = true)) {
            case (c, s) =>
              val number = Double.parseDouble(s)
              val result = ImplOps.countFloating(number)
              assert(result)(isSuccess(equalTo(c)))
          }
        },
        testM("positive/negative int value") {
          check(genStringifiedNumber(9)) {
            case (c, s) =>
              val number = Integer.parseInt(s)
              val result = ImplOps.countLong(number)
              assert(result)(isSuccess(equalTo(c)))
          }
        },
        testM("positive/negative long value") {
          check(genStringifiedNumber(15)) {
            case (c, s) =>
              val number = Long.parseLong(s)
              val result = ImplOps.countLong(number)
              assert(result)(isSuccess(equalTo(c)))
          }
        }
      ),
      suite("test that it calls")(
        //not using a mocking framework to verify functions are called here,
        //just a simple mutabale var, I wouldn't do this with real code
        //in real code I usually use stubs and *still avoid mocking frameworks*
        //but instead of vars I usually use Promises/Refs, the pure ones(Cats,ZIO), not the scala Promise
        testM("countBigDecimal when counting BigDecimal") {
          sampleNo(isDecimal = true) map {
            case (_, s) =>
              val number = new BigDecimal(s)
              var called = 0
              val testOps = new ImplOps {
                override def countBigDecimal(number: BigDecimal) = {
                  called = called + 1
                  Success(0L)
                }
              }
              Ops.count(number)(testOps)
              assert(called)(equalTo(1))
          }
        },
        testM("countFloating when counting Float") {
          sampleNo(max = 10, isDecimal = true) map {
            case (_, s) =>
              val number = Float.parseFloat(s)
              var called = 0
              val testOps = new ImplOps {
                override def countFloating(number: Double) = {
                  called = called + 1
                  Success(0L)
                }
              }
              Ops.count(number)(testOps)
              assert(called)(equalTo(1))
          }
        },
        testM("countFloating when counting Double") {
          sampleNo(max = 15, isDecimal = true) map {
            case (_, s) =>
              val number = Double.parseDouble(s)
              var called = 0
              val testOps = new ImplOps {
                override def countFloating(number: Double) = {
                  called = called + 1
                  Success(0L)
                }
              }
              Ops.count(number)(testOps)
              assert(called)(equalTo(1))
          }
        },
        testM("countLong when counting Integer") {
          sampleNo(max = 9) map {
            case (_, s) =>
              val number = Integer.parseInt(s)
              var called = 0
              val testOps = new ImplOps {
                override def countLong(number: Long) = {
                  called = called + 1
                  Success(0L)
                }
              }
              Ops.count(number)(testOps)
              assert(called)(equalTo(1))
          }
        },
        testM("countLong when counting Long") {
          sampleNo(max = 15) map {
            case (_, s) =>
              val number = Long.parseLong(s)
              var called = 0
              val testOps = new ImplOps {
                override def countLong(number: Long) = {
                  called = called + 1
                  Success(0L)
                }
              }
              Ops.count(number)(testOps)
              assert(called)(equalTo(1))
          }
        }
      )
    ) @@ TestAspect.timeout(30.seconds)
}
