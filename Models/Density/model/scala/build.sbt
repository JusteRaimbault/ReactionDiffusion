name := "density"

version := "1.0"

scalaVersion := "2.11.8"

enablePlugins(SbtOsgi)

OsgiKeys.exportPackage := Seq("density.*")

OsgiKeys.importPackage := Seq("*;resolution:=optional")

OsgiKeys.privatePackage := Seq("*")

OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.5"