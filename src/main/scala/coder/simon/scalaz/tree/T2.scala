package coder.simon.scalaz.tree

import scalaz._
import Scalaz._
import spray.json._
import scala.annotation.tailrec

trait Config {
  type A
  def tags: List[String]
  val default: A

  implicit def showValue: Show[A]
  implicit def formatValue: JsonFormat[A]

  case class Name(tag: String, value: String)
  private def createName(pair: (String, String)) = Name(pair._1, pair._2)

  case class Value(name: Name, value: A)

  implicit val valueShow = new Show[Value] {
    override def show(v: Value) = s"${v.name.tag}#${v.name.value}-(${showValue.show(v.value)})"
  }

  val root = Tree(Value(Name("root", "default"), default))

  type ConfigTree = Tree[Value]

  private def locNames(loc: TreeLoc[Value], names: List[Name]): Option[TreeLoc[Value]] = {
    @tailrec
    def step(opt: Option[TreeLoc[Value]], path: List[Name]): Option[TreeLoc[Value]] = (opt, path) match {
      case (None, _)         => None
      case (path, Nil)       => path
      case (Some(l), h :: t) => step(l.findChild { _.rootLabel.name == h }, t)
    }
    step(Some(loc), names)
  }

  private object JsonProtocol extends DefaultJsonProtocol {
    implicit val nameFormat = jsonFormat2(Name)
    implicit val valueFormat = jsonFormat2(Value)

    val TAG = "tag"
    val NAME = "name"
    val VALUE = "value"
    val DETAILS = "details"

    implicit object configTreeFormat extends RootJsonFormat[ConfigTree] {

      def read(json: JsValue): ConfigTree = json.asJsObject.getFields(TAG, NAME, VALUE, DETAILS) match {
        case Seq(JsString(tag), JsString(name), value, JsArray(details)) =>
          Tree.node(Value(Name(tag, name), formatValue.read(value)), details.map { js => js.convertTo[ConfigTree] }.toStream)
        case Seq(JsString(tag), JsString(name), value) =>
          Tree.leaf(Value(Name(tag, name), formatValue.read(value)))
      }

      def write(obj: ConfigTree): JsValue = obj match {
        case Tree.Node(Value(Name(tag, name), value), forest) =>
          if (forest.isEmpty) {
            JsObject(
              TAG -> JsString(tag),
              NAME -> JsString(name),
              VALUE -> formatValue.write(value))
          } else {
            JsObject(
              TAG -> JsString(tag),
              NAME -> JsString(name),
              VALUE -> formatValue.write(value),
              DETAILS -> JsArray(forest.map(_.toJson).toVector))
          }
      }
    }
  }

  private def addOrUpdateImpl(tree: ConfigTree, names: List[String], value: A) = {
    val (inits, last) = { val path = tags.zip(names); (path.init, path.last) }
    val lastName = createName(last)
    val cp = Value(lastName, value)
    val levelMinusOne = locNames(tree.loc, inits.map(createName))
    val levelFull = levelMinusOne.flatMap(l => l.findChild { _.rootLabel.name == lastName })
    (levelMinusOne, levelFull) match {
      case (None, _)           => throw new IllegalStateException(s"default ${inits.mkString("/")} not set yet")
      case (Some(l1), None)    => l1.modifyTree { t => Tree.node(t.rootLabel, Tree.leaf(cp) #:: t.subForest) }.toTree
      case (Some(_), Some(l2)) => l2.modifyLabel { n => n.copy(value = value) }.toTree
    }
  }

  private def getConfigImpl(tree: ConfigTree, names: List[String]): Option[Value] = {
    @tailrec
    def go(tree: ConfigTree, path: List[Name]): Option[Value] = {
      val r = locNames(tree.loc, path).map(r => r.getLabel)
      (path, r) match {
        case (Nil, _)  => r
        case (_, None) => go(tree, path.init)
        case (_, _)    => r
      }
    }
    go(tree, tags.zip(names).map(createName))
  }

  var tree = root
  def addOrUpdate(names: List[String], value: A) = {
    tree = addOrUpdateImpl(tree, names, value)
    tree
  }
  def getConfig(names: List[String]): Option[Value] = getConfigImpl(tree, names)

  import JsonProtocol._
  def fromJson(js: JsValue) = js.convertTo[ConfigTree]

  def toJson() = tree.toJson

  def drawTree = tree.drawTree
}

object RTPConfig extends Config {
  type A = BigDecimal
  val tags = List("operator", "game", "currency", "bettype")
  val showValue = implicitly[Show[BigDecimal]]
  object valueFormat extends RootJsonFormat[BigDecimal] {
    def read(json: JsValue): BigDecimal = json match {
      case JsNumber(rtp) => rtp
      case _             => throw new IllegalStateException
    }
    def write(obj: BigDecimal): JsValue = JsNumber(obj)
  }
  val formatValue = valueFormat
  val default: BigDecimal = 0.96
}

object T2 {

  def main(args: Array[String]) {

    val config = RTPConfig

    val t1 = config.addOrUpdate(List("simon"), 0.2)
    println(t1.drawTree)

    val t2 = config.addOrUpdate(List("valor"), 0.1)
    val t3 = config.addOrUpdate(List("simon"), 0.7)

    println(t2.drawTree)
    println(t3.drawTree)

    val t2c = config.addOrUpdate(List("simon", "ilotto5"), 0.8)

    val t2c1 = config.addOrUpdate(List("simon", "ilotto5", "RMB"), 0.8)
    println(t2c1.drawTree)

    val x = config.toJson
    println(x)

    val y = config.fromJson(x)
    println(y.drawTree)
  }

}
