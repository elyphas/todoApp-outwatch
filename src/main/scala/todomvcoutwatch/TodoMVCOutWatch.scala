package todomvcoutwatch

import cats.effect.IO
import monix.execution.Scheduler.Implicits.global
import org.scalajs.dom
import outwatch.dom._
import outwatch.dom.dsl._
import todomvcoutwatch.StoreApp._
import todomvcoutwatch.Menu._
import todomvcoutwatch.Components._

object TodoMVCOutWatch {

  def main ( args: Array [ String ] ): Unit = {

    val localStore = (key: String) => (data: String) => IO(dom.window.localStorage.setItem(key, data))

    val app = for {
      initialState <- initialState
      writeLocal = localStore(LocalStorageKey)
      store <- storeApp(initialState)
      _ = store.mapEvalF(st => writeLocal(upickle.default.write(st.todoList))).foreachL(identity).runAsync(_ => ())
      router <- hashRouter
      _ <- IO(router.map(s => UpdateFilter ( s ) ).subscribe(store))
      focusHandler <- Handler.create [ Int ]
      result = div(
        h1 ( "todos" ),
        todoInput ( store, store ),
        store.map(state =>
          state.todoList match {
            case Nil => Seq.empty
            case _ => Seq ( todoListSection ( state, store, focusHandler ), todoFooter ( state, store ) )
          }
        )
      )
      _ <- OutWatch.renderInto(".todoapp", result )
    } yield ( )

    app.unsafeRunSync ( )

  }

}