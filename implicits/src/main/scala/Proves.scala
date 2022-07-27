import scala.language.postfixOps

object Proves extends App {

  trait Coffee {
    def cost: Int
  }

  case class Cappuccino(quantity: Int) extends Coffee {
    val BASE_PRICE = 300

    def cost: Int = BASE_PRICE * quantity
    def +(that: Coffee) = Order(List(this, that))
  }

  case class Latte(quantity: Int) extends Coffee {
    def cost: Int = quantity * 350
  }

  case class Order(items: List[Coffee]) {
    def cost: Int = items.map(item => item.cost).sum
    override def toString: String = s"${this.cost}"
    def +(coffee: Coffee) = Order(items ::: List(coffee))
  }

  case class Request(quantity: Int) {
    def cappuccino = Cappuccino(quantity)
    def latte = Latte(quantity)
  }

  implicit def fromQuantityToConverter(quantity: Int) = Request(quantity)

  val orderV1 = Order(List(
    Cappuccino(2),
    Latte(2)
  ))
  val orderV2 = (1 cappuccino) + (1 cappuccino) + (2 latte)

  println(s"The cost of the order is ${orderV1.cost}")
  println(s"The cost of the order MVP is ${orderV2}")
}
