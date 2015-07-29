package coder.simon.types.phantom

/**
 * @author simon
 */
object P2 {

  sealed private[phantom] class SelectQuery[State <: SelectQuery[_]](qs: String) {
    def query: String = qs

    def where[T >: State <: BaseSelect](whereStatement: String): SelectQuery[WhereSelect] =
      new SelectQuery[WhereSelect](query + " where " + whereStatement)

    def and[T >: State <: WhereSelect](whereStatement: String): SelectQuery[WhereSelect] =
      new SelectQuery[WhereSelect](query + " and " + whereStatement)
  }

  sealed private[phantom] class BaseSelect(qs: String) extends SelectQuery[BaseSelect](qs)
  sealed private[phantom] class WhereSelect(qs: String) extends SelectQuery[WhereSelect](qs)

  object SelectQuery {
    def create(qs: String) = new SelectQuery[BaseSelect](qs)
  }
  
 
}