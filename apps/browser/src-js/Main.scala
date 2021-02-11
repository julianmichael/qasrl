package qasrl.apps.browser

import scala.concurrent.Future

import org.scalajs.dom

import cats.~>
import cats.Id

import jjm.DotMap
import jjm.LowerCaseString
import jjm.OrWrapped
import jjm.io.HttpUtil

import qasrl.bank.DataIndex
import qasrl.bank.Document
import qasrl.bank.DocumentId
import qasrl.bank.service.DocumentService
import qasrl.bank.service.Search

import qasrl.data.Dataset

import japgolly.scalajs.react.AsyncCallback
import scalacss.DevDefaults._

object Main {
  def main(args: Array[String]): Unit = {
    BrowserStyles.addToDocument()

    val dataIndex = {
      import scala.scalajs.js
      import io.circe.scalajs.decodeJs
      import io.circe.syntax._
      import collection.mutable
      import cats.Order.catsKernelOrderingForOrder
      decodeJs[DataIndex](
        js.Dynamic.global.dataMetaIndex.asInstanceOf[js.Any]
      ) match {
        case Right(index) => index
        case Left(err) =>
          System.err.println(err)
          null: DataIndex
      }
    }

    val apiUrl: String = dom.document
      .getElementById(SharedConstants.apiUrlElementId)
      .getAttribute("value")

    val initialCache = DotMap.empty[Id, DocumentService.Request]
      .put(DocumentService.GetDataIndex)(dataIndex)
      .put(DocumentService.SearchDocuments(Search.Query(None, Set())))(dataIndex.allDocumentIds)

    import scala.concurrent.ExecutionContext.Implicits.global

    type DelayedFuture[A] = () => Future[A]
    val wrapCallback = λ[DelayedFuture ~> AsyncCallback](f =>
      AsyncCallback.fromFuture(f())
    )

    val documentService = DocumentService(
      jjm.Memo.memoizeDotFuture(
        HttpUtil.makeHttpPostClient[DocumentService.Request](apiUrl),
        initialCache
      ).andThenK(OrWrapped.mapK(wrapCallback))
    )

    Browser.Component(Browser.Props(documentService)).renderIntoDOM(
      dom.document.getElementById("browser")
    )
  }
}
