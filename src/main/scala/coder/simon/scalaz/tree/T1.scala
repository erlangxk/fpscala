package coder.simon.scalaz.tree

import scalaz._
import Scalaz._
import spray.json._

object T1 {

  case class Value(name: String, active: Boolean)

  implicit val valueShow = new Show[Value] {
    override def show(v: Value) = s"name:${v.name},active:${v.active}"
  }

  object ValueJsonProtocol extends DefaultJsonProtocol {
    implicit val valueFormat = jsonFormat2(Value)

    implicit object valueTreeFormat extends RootJsonFormat[CatTree] {
      def read(json: JsValue): CatTree = json.asJsObject.getFields("name", "active", "children") match {
        case Seq(JsString(name), JsBoolean(active), JsArray(children)) =>
          Tree.node(Value(name, active), children.map { js => js.convertTo[CatTree] }.toStream)
        case Seq(JsString(name), JsBoolean(active)) =>
          Tree.leaf(Value(name, active))
      }

      def write(obj: CatTree): JsValue = obj match {
        case Tree.Node(Value(name, active), forest) =>
          if (forest.isEmpty) {
            JsObject(
              "name" -> JsString(name),
              "active" -> JsBoolean(active))
          } else {
            JsObject(
              "name" -> JsString(name),
              "active" -> JsBoolean(active),
              "children" -> JsArray(forest.map(_.toJson).toVector))
          }
      }
    }
  }

  type CatTree = Tree[Value]

  def catLevelOf(tree: CatTree, path: List[String]): Option[CatTree] = {
    def step(opt: Option[CatTree], p: List[String]): Option[CatTree] = (opt, p) match {
      case (None, _)               => None
      case (p, Nil)                => p
      case (Some(t), head :: tail) => step(t.subForest.find { x => x.rootLabel.name == head }, tail)
    }
    step(Some(tree), path)
  }

  private def locOfPath(loc: TreeLoc[Value], path: List[String]): Option[TreeLoc[Value]] = {
    def step(opt: Option[TreeLoc[Value]], p: List[String]): Option[TreeLoc[Value]] = (opt, p) match {
      case (None, _)         => None
      case (p, Nil)          => p
      case (Some(l), h :: t) => step(l.findChild { _.rootLabel.name == h }, t)
    }
    step(Some(loc), path)
  }

  def modifyLabel(tree: CatTree, path: List[String], f: Value => Value) = {
    locOfPath(tree.loc, path).map(l => l modifyLabel f).map(_.toTree)
  }

  def main(args: Array[String]) {

    val g1 = Value("Root", true).node(
      Value("C10", true).node(
        Value("C101", true).node(
          Value("C1010", false).leaf,
          Value("C1011", true).leaf,
          Value("C1012", false).leaf),
        Value("C102", false).node(Value(
          "C1020", true).leaf,
          Value("C1021", true).leaf)),
      Value("C11", true).node(
        Value("C111", false).node(
          Value("C1110", false).leaf,
          Value("C1111", true).leaf),
        Value("C112", true).node(
          Value("C1120", true).leaf)))

    val kk1 = modifyLabel(g1, List("C10", "C101", "C1012"), v => v.copy(active = true))
    println(kk1.get.drawTree)

    import ValueJsonProtocol._

    val x = kk1.get.toJson
    println(kk1.get.toJson)

    println("************")
    val jt = x.convertTo[CatTree]
    println(jt.drawTree)
  }

}