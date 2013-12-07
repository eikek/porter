import sbt._
import sbt.classpath.ClasspathUtilities

object Distribution extends Plugin {

  object DistributionKeys {
    val dist = TaskKey[File]("dist", "Creates a zip file with porter in it.")
  }

  private val libraries = Keys.dependencyClasspath.in(Runtime)
  private val apiJar = Keys.packageBin.in(Api.module).in(Compile)
  private val appJar = Keys.packageBin.in(App.module).in(Compile)
  private val distJar = Keys.packageBin.in(Compile)

  val distSettings = super.projectSettings ++ Seq(
    DistributionKeys.dist <<=
      (libraries, apiJar, appJar, distJar, Keys.target, Keys.sourceDirectory, Keys.version) map {
      (libs, api, app, distr, target, srcdir, version) =>

      val (files, dirs) = libs.map(_.data).toVector.partition(ClasspathUtilities.isArchive)
      val outdir = target / s"porter-$version"
      val zipped = target / s"porter-$version.zip"
      IO.delete(outdir)
      val libdir = outdir / "lib"
      (files :+ api :+ app).foreach(f => IO.copyFile(f, libdir / f.getName))

      IO.createDirectories(Seq(libdir, outdir / "bin", outdir / "etc"))
      IO.copyFile(distr, outdir / "bin" / "porter.jar")
      IO.listFiles(srcdir / "main" / "dist" / "bin").map(f => IO.copyFile(f, outdir / "bin" / f.getName))
      IO.listFiles(srcdir / "main" / "dist" / "etc").map(f => IO.copyFile(f, outdir / "etc" / f.getName))

      IO.zip(entries(outdir).map(d => (d, d.getAbsolutePath.substring(outdir.getParent.length +1))), zipped)
      zipped
    }
  )

  def entries(f: File) :List[File] =
    f :: (if (f.isDirectory) IO.listFiles(f).toList.flatMap(entries) else Nil)
}