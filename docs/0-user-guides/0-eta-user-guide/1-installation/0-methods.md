# Methods

## Overview

In this section, we'll setup a working environment for Eta development so that you can follow along with the rest of the modules.

There are different ways to do this so we'll give you a quick overview of the benefits of each method.

* **Gradle** - This is the easiest way to get started and also the recommended way of installing Eta on your system. You should use this method if you are working on mixed-language projects.
* **Etlas** - Etlas is the build tool for Eta and it forms the backbone of any plugin to existing build tools (e.g. Gradle, Bazel, Maven). This method is preferred if you want to write projects in Eta entirely.
* **Source** - Feel like contributing? Enjoy building software from source!
* **Docker** - Like keeping your development environment containerized? Every commit to the `master` branch of the Eta repository builds a docker image allowing you to conveniently try out new features.
* **Nix** - Fan of the Nix package manager? We have a Nix expressions ready for you that will set you up with a reproducible build environment.
* **sbt** - An avid Scala developer? You may prefer the sbt-eta plugin to integrate Eta into your Scala projects.

## Choose an Installation

<style>
.install-circle {
    height: 150px;
    width: 150px;
    background-color: #2cd4d9;
    border-radius: 50%;
    margin: auto;
    text-align: center;
    line-height: 150px;
    position: relative;
}
.install-circle img {
  vertical-align: middle;
  height:50px;
  position: absolute;
  top: 50%;
  margin-top: -30px;
  margin-left: -35px;
}
div.install-circle:hover{
    background-color: #15686b;
}
a{
  text-decoration: none;
}
img.star {
  position: absolute;
  -webkit-filter: drop-shadow(2px 2px 2px #696969);
  filter: drop-shadow(2px 2px 2px #696969);
  top: 5%;
  left: 90%;
}
</style>

<div class="row mt--40">
  <div class='col-lg-4 col-sm-6 mb--xs--60 mt--xs--20'>
    <a href="/docs/user-guides/eta-user-guide/installation/gradle">
      <div class="install-circle" style="border: 5px solid #efce4a;">
        <img alt="Gradle" src="/images/gradle-logo.svg">
        <img class="star" src="/images/star.svg">
      </div>
      <h3 class='heading--small center-xs mt--20'>Gradle</h3>
    </a>  
  </div>
  <div class='col-lg-4 col-sm-6 mb--xs--60 mt--xs--20'>
    <a href="/docs/user-guides/eta-user-guide/installation/etlas">
      <div class="install-circle">
        <img alt="Etlas" src="/images/etlas-logo.svg" style="margin-left: -30px; margin-top: -35px; height: 70px;">
      </div>
      <h3 class='heading--small center-xs mt--20'>Etlas</h3>
    </a>  
  </div>
  <div class='col-lg-4 col-sm-6 mb--xs--60 mt--xs--20'>
    <a href="/docs/user-guides/eta-user-guide/installation/source">
      <div class="install-circle">
        <img alt="GitHub" src="/images/github-logo.svg" style="margin-left: -35px; margin-top: -35px; height: 70px;">
      </div>
      <h3 class='heading--small center-xs mt--20'>Source</h3>
    </a>  
  </div>
</div>

<div class="row mt--40">
  <div class='col-lg-4 col-sm-6 mb--xs--60 mt--xs--20'>
    <a href="/docs/user-guides/eta-user-guide/installation/docker">
      <div class="install-circle">
        <img alt="Docker" src="/images/docker-logo.svg" style="margin-left:-30px; margin-top:-25px">
      </div>
      <h3 class='heading--small center-xs mt--20'>Docker</h3>
    </a>  
  </div>
  <div class='col-lg-4 col-sm-6 mb--xs--60 mt--xs--20'>
    <a href="/docs/user-guides/eta-user-guide/installation/nix">
      <div class="install-circle">
        <img alt="Etlas" src="/images/nixos-logo.svg" style="margin-left: -30px; margin-top: -35px; height: 70px;">
      </div>
      <h3 class='heading--small center-xs mt--20'>Nix</h3>
    </a>  
  </div>
  <div class='col-lg-4 col-sm-6 mb--xs--60 mt--xs--20'>
    <a href="/docs/user-guides/eta-user-guide/installation/sbt">
      <div class="install-circle">
        <img alt="GitHub" src="/images/sbt-logo.svg" style="margin-left: -45px;">
      </div>
      <h3 class='heading--small center-xs mt--20'>sbt</h3>
    </a>  
  </div>
</div>

## Jump to Module

Click [here](/docs/user-guides/eta-user-guide/basics/quick-start) to jump to the basics module.
