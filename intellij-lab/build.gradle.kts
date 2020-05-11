plugins {
    id("org.jetbrains.intellij") version "0.4.20"
    java
    kotlin("jvm") version "1.3.71"
}

group = "org.example"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
    maven("http://maven.stardog.com")
}

dependencies {
    implementation(kotlin("stdlib-jdk8"))
    implementation(files("/home/jbalint/sw/java-sw/jcfl/build/libs/jcfl-0.1.jar"))
//    implementation("com.stardog.stark:client-http:7.2.1")
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
    version = "2020.1.1"
//    pluginName = ""
    setPlugins("java")
}
configure<JavaPluginConvention> {
    sourceCompatibility = JavaVersion.VERSION_1_8
}
tasks {
    compileKotlin {
        kotlinOptions.jvmTarget = "1.8"
    }
    compileTestKotlin {
        kotlinOptions.jvmTarget = "1.8"
    }
}
tasks.getByName<org.jetbrains.intellij.tasks.PatchPluginXmlTask>("patchPluginXml") {
    changeNotes("""
      Add change notes here.<br>
      <em>most HTML tags may be used</em>""")
}
//
//// TODO : already set by default in RunIdeBase?
//tasks.getByName<org.jetbrains.intellij.tasks.RunIdeTask>("runIde") {
//    systemProperty("idea.auto.reload.plugins", "true")
//}