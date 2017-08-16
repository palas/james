#!/bin/bash
mkdir temp && \
cd temp && \
wget http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && \
sudo dpkg -i erlang-solutions_1.0_all.deb && \
sudo apt-get update && \
sudo apt-get install -qq emacs && \
sudo apt-get install -qq erlang && \
sudo apt-get install -qq tomcat7 && \
sudo apt-get install -qq maven2 && \
sudo apt-get install -qq autotools-dev && \
sudo apt-get install -qq autoconf && \
sudo apt-get install -qq automake && \
sudo apt-get install -qq g++ && \
sudo apt-get install -qq libboost-system-dev && \
cd ../example/freq_server && \
mvn compile && \
mvn war:war && \
cd ../.. && \
sudo cp example/freq_server/target/*war /var/lib/tomcat7/webapps/freq_server.war && \
cd agent && \
./autogen.sh && \
./configure && \
make && \
cd ../server && \
cp ../travis/test.erl ./ && \
erl -make && \
cd ..
