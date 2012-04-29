resolvers += "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
 
addSbtPlugin("org.ensime" % "ensime-sbt-cmd" % "0.0.7")


addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.7.+")

resolvers += Resolver.url("sbt-plugin-releases",new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns)

