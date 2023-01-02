plugins {
    // https://plugins.jetbrains.com/docs/intellij/tools-gradle-intellij-plugin.html
    id("org.jetbrains.intellij") version "1.11.0"
    java
}

group = "org.example"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
    maven("https://maven.stardog.com")
}

dependencies {
    implementation(files("/home/jbalint/sw/java-sw/jcfl/build/libs/jcfl-0.1.jar"))
    implementation("com.complexible.stardog:client-http:7.2.1") {
        // conflicts with IntelliJ's version
        exclude(group = "org.slf4j")
        // contains a non-shadowed slf4j (WTF!)
        exclude(group = "org.apache.directory.server")
        // DatatypeFactory issues w/j11. originated in Stark's `Values` static initializer
        exclude(group = "xerces")
        exclude(group = "xml-apis")
    }
    testImplementation("junit", "junit", "4.12")
}

// See https://github.com/JetBrains/gradle-intellij-plugin/
intellij {
    version.set("2022.2.1")
    type.set("IC")
    plugins.set(listOf("java"))
}

tasks {
    // Set the JVM compatibility versions
    withType<JavaCompile> {
        sourceCompatibility = "11"
        targetCompatibility = "11"
    }
}
tasks.getByName<org.jetbrains.intellij.tasks.PatchPluginXmlTask>("patchPluginXml") {
//    changeNotes("""
//      Add change notes here.<br>
//      <em>most HTML tags may be used</em>""")
}
//
//// TODO : already set by default in RunIdeBase?
//tasks.getByName<org.jetbrains.intellij.tasks.RunIdeTask>("runIde") {
//    systemProperty("idea.auto.reload.plugins", "true")
//}