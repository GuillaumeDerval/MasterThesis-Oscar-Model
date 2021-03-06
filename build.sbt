name := "Oscar-EPS"

version := "0.1"

scalaVersion := "2.11.8"

resolvers += "Artifactory-UCL" at "http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot-local/"

libraryDependencies += "jaxen" % "jaxen" % "1.1.6"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.8"
libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5"

libraryDependencies += "org.graphstream" % "gs-core" % "1.3"
libraryDependencies += "org.graphstream" % "gs-algo" % "1.3"
libraryDependencies += "org.graphstream" % "gs-ui" % "1.3"

libraryDependencies += "org.jfree" % "jfreechart" % "1.0.19"

libraryDependencies += "org.rogach" % "scallop_2.11" % "1.0.0"

libraryDependencies += "org.scala-lang.modules" % "scala-swing_2.11" % "2.0.0-M2"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.4.3"
libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.4.3"
libraryDependencies += "com.twitter" % "chill-akka_2.11" % "0.8.0"
libraryDependencies += "org.scala-lang.modules" %% "spores-core" % "0.2.1"

libraryDependencies += "oscar" % "oscar-algo_2.11" % "3.1.0-SNAPSHOT"
libraryDependencies += "oscar" % "oscar-cp_2.11" % "3.1.0-SNAPSHOT"
libraryDependencies += "oscar" % "oscar-util_2.11" % "3.1.0-SNAPSHOT"