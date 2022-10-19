import mill._
// import ammonite.
// import ammonite.ops._

trait SimpleJSDeps extends Module {
  def jsDeps = T { Agg.empty[String] }
  def downloadedJSDeps = T {
    for(url <- jsDeps()) yield {
      val filename = url.substring(url.lastIndexOf("/") + 1)
        os.proc("curl", "-o", filename, url).call(cwd = T.ctx().dest)
      T.ctx().dest / filename
    }
  }
  def aggregatedJSDeps = T {
    val targetPath = T.ctx().dest / "jsdeps.js"
    os.write.append(targetPath, "")
    downloadedJSDeps().foreach { path =>
      os.write.append(targetPath, os.read(path))
      os.write.append(targetPath, "\n")
    }
    PathRef(targetPath)
  }
}
