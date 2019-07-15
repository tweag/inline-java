export JAVA_HOME="$(dirname "$(java -XshowSettings:properties -version 2>&1 > /dev/null | grep 'java.home' | sed 's/[[:space:]]*java\.home = //')")"
