# Gradle

## How It Works

You'll clone a GitHub repository that contains a project template and you'll build the project using the Gradle Wrapper script. The script will install Gradle for you and Gradle will install Eta and Etlas for you.

## Requirements

You must have the following installed on your system:

- [JDK 8 or above](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)
- [Git](https://git-scm.com/book/en/v1/Getting-Started-Installing-Git)

## Installation

1.  Fire up your terminal and type the following commands:

    ```sh
    $ git clone https://github.com/typelead/eta-init
    $ cd eta-init
    ```

    **Linux/OS X**

    ```sh
    $ ./gradlew run
    ```

    **Windows**

    ```sh
    $ gradlew.bat run
    ```

2.  If this is your first time using Eta on your system, you will get prompted about your preference for telemetry.

    If you are fine with sending metrics, you can run the following commands:

    **Linux/OS X**

    ```sh
    $ ./gradlew run -PetaSendMetrics=true
    ```

    **Windows**

    ```sh
    $ gradlew.bat run -PetaSendMetrics=true
    ```

    If you are **not** fine with sending metrics, you can replace **true** with **false** in the commands listed above.

    **NOTE:** You only need to send the `-P` argument the first time you express your preference.

## Jump to Module

Click [here](/docs/user-guides/eta-user-guide/basics/quick-start) to jump to the basics module.
