#!/bin/sh

sed -i 's/<code>/`/g' $1
sed -i 's/<\/code>/`/g' $1
sed -i 's/<p>//g' $1
sed -i 's/<\/p>//g' $1

sed -i 's/\#ifdef HsColour/~~~~ \{.haskell\}/g' $1
sed -i 's/\#endif/~~~~/g' $1

sed -i 's/<div class=\"code\">//g' $1
sed -i 's/<\/div>//g' $1