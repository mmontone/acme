#!/bin/sh

CONFIGURATOR_HOME=/usr/local/lib/configurator

rm -rf $CONFIGURATOR_HOME
mkdir $CONFIGURATOR_HOME
cp * $CONFIGURATOR_HOME -r
rm /usr/local/bin/configurator
ln $CONFIGURATOR_HOME/configurator -s /usr/local/bin/configurator 
