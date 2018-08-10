# Eta Proguard Config

# Requirements

- [ProGuard](https://www.guardsquare.com/en/proguard)

# Steps

1. Once you have compiled your Eta project with `--enable-uberjar-mode`, copy the
   generated JAR file to this directory.
2. Edit the config file and replace the values of `-injars` and `-outjars` with the
   name of your input file and desired output file. 
3. Download the proguard file from https://github.com/typelead/gradle-eta/blob/master/src/main/resources/com/typelead/gradle/eta/api/eta-rules.pro.
4. Run `proguard @eta-rules.pro`.

