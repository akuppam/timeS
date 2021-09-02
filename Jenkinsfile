#!/usr/bin/env groovy
@Library('devops/pipeline-shared-library@v1-latest' ) _

def slackChannel = "#datascience-cicd"
def buildInfo = "_time-series-multireg <${env.BUILD_URL}/console|${env.BUILD_DISPLAY_NAME}> (${env.BRANCH_NAME})_"
def color = "good"
def commitHash = "unset"

properties([
        buildDiscarder(logRotator(numToKeepStr: '5')),
        disableConcurrentBuilds(),
])

node("dind") {
    ws(UUID.randomUUID().toString()) {
        try {
            stage('Checkout') {
                deleteDir()
                checkoutProject()
            }

            stage('Setup') {
                sh("make agent-stop")
                sh("make agent-running")
            }

            stage('Install Module') {
                sh("make install")
            }

            stage('Test') {
                def testStatus = sh(
                    script: "make test",
                    returnStdout: true
                )
                notifySlack message: "${buildInfo}: *Tests complete!* 👩‍🍳 ${testStatus}", channel: slackChannel 
                echo testStatus
            }

            def tag = sh(
                returnStdout: true,
                script: "git tag --contains | head -1"
            ).trim()

            if (tag) {
                stage("Release") {
                    sh(script: "make publish_module")
                }
            }

        } catch (Exception ex) {
            notifySlack message: "${buildInfo}: 😫  *Exception During Build*: ${ex}", channel: slackChannel, color: "danger", icon: ":warning:"
            color = "danger"
            error("Build failed.")
        } finally {
            if(color == "good")
                notifySlack message: "${buildInfo}: *Done*: 👍", channel: slackChannel, color: color
            else
                notifySlack message: "${buildInfo}: *Done*: 👎", channel: slackChannel, color: color
        }
    }
}
