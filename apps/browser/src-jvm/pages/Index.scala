package qasrl.apps.browser.pages
import qasrl.apps.browser._
import scalatags.Text.all._

object Index {
  def apply(config: PageConfig): scalatags.Text.all.Frag =
    html(lang := "en")(
      head(
        meta(charset := "utf-8"),
        meta(
          name := "viewport",
          content := "width=device-width, initial-scale=1, shrink-to-fit=no"
        ),
        config.bootstrapLink,
        tag("title")("QA-SRL | Browse Data")
      ),
      body(
        div(id := SharedConstants.mainBrowserDivElementId),
        input(
          `type` := "hidden",
          value := config.apiUrl,
          id := SharedConstants.apiUrlElementId
        ),
        config.bootstrapScripts,
        script(`type` := "text/javascript")(raw(config.dataMetaIndexContent)),
        script(src := config.jsDepsLocation),
        script(src := config.jsLocation)
      )
    )
}
