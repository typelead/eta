@echo off
git pull
git submodule update --recursive
cleaninstall
etlas update
