package qasrl.crowd.util

import org.scalajs.dom.html

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import cats.implicits._

// allows you to easily use refs inline in DOM creation, if, for example,
// you need to set the location of some element (e.g., a dropdown menu)
// on the basis of the location of another.
object InstructionsPanel {

  val dataToggle = VdomAttr("data-toggle")

  case class Props(
    instructionsId: String,
    collapseCookieId: String,
    tabs: List[(String, VdomTag)]
  )

  class Backend(scope: BackendScope[Props, Unit]) {

    def render(props: Props) =
      <.div(
        ^.classSet1("card"),
        <.div(
          ^.classSet1("card-header card-inverse card-secondary"),
          <.h4(
            <.a(
              dataToggle := "collapse",
              ^.href := s"#${props.instructionsId}",
              "Instructions ",
              <.span(^.id := "collapse-text")
            )
          )
        ),
        <.div(
          ^.id := props.instructionsId,
          ^.classSet1("collapse"),
          <.div( // header
            ^.classSet1("card-header"),
            <.ul(
              ^.classSet1("nav nav-tabs card-header-tabs"),
              props.tabs.zipWithIndex.toVdomArray {
                case ((title, _), index) =>
                  <.li(
                    ^.classSet1("nav-item"),
                    ^.key := s"nav-item-$index",
                    <.a(
                      ^.classSet1("nav-link", "active" -> (index == 0)),
                      ^.href := s"#tab-$index-body",
                      dataToggle := "tab",
                      title
                    )
                  )
              }
            )
          ),
          <.div( // content
            ^.classSet1("card-block"),
            <.div(
              ^.classSet1("tab-content"),
              props.tabs.zipWithIndex.toVdomArray {
                case ((_, content), index) =>
                  <.div(
                    ^.key := s"tab-$index-body",
                    ^.id := s"tab-$index-body",
                    ^.classSet1("tab-pane"),
                    content
                  )
              }
            )
          )
        )
      )
  }

  val Component = ScalaComponent
    .builder[Props]("Instructions")
    .renderBackend[Backend]
    .componentDidMount(
      context =>
        Callback {
          import scala.scalajs.js.Dynamic
          import org.scalajs.jquery.jQuery
          val cookieOpt = Dynamic.global.$.cookie(context.props.collapseCookieId)
            .asInstanceOf[scalajs.js.UndefOr[String]]
            .toOption
          if (cookieOpt.nonEmpty) {
            jQuery(s"#collapse-text").text("(Click to expand)")
          } else {
            Dynamic.global.$(s"#${context.props.instructionsId}").collapse("show")
            jQuery(s"#collapse-text").text("(Click to collapse)")
          }

          Dynamic.global.$("#tab-0-body").tab("show")

          Dynamic.global
            .$(s"#${context.props.instructionsId}")
            .on(
              "hide.bs.collapse",
              () => {
                jQuery(s"#collapse-text").text("(Click to expand)")
                Dynamic.global.$.cookie(
                  context.props.collapseCookieId,
                  "1",
                  Dynamic.literal(expires = 5, path = "/")
                )
              }
            )

          Dynamic.global
            .$(s"#${context.props.instructionsId}")
            .on(
              "show.bs.collapse",
              () => {
                jQuery(s"#collapse-text").text("(Click to collapse)")
                Dynamic.global.$.removeCookie(
                  context.props.collapseCookieId,
                  Dynamic.literal(path = "/")
                )
              }
            )

      }
    )
    .build
}
