#!/bin/bash
mkdir temp && \
cd temp && \
wget http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && \
sudo dpkg -i erlang-solutions_1.0_all.deb && \
sudo apt-get update && \
sudo apt-get install -qq erlang && \
sudo apt-get install -qq tomcat7 && \
sudo apt-get install -qq maven2 && \
cd ../example/freq_server && \
mvn compile && \
mvn war:war && \
cd ../.. && \
sudo cp example/freq_server/target/*war /var/lib/tomcat7/webapps/freq_server.war
