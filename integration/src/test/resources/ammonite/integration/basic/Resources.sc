interp.load.ivy("org.apache.jackrabbit" % "oak-core" % "1.3.16")
@
val path = "org/apache/jackrabbit/oak/plugins/blob/blobstore.properties"
val stream = Thread.currentThread().getContextClassLoader.getResourceAsStream(path)
println(new String(stream.readAllBytes(), "UTF-8").length) // Should work