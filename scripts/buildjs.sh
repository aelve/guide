#!/bin/bash -xe

cd guidejs
echo "PWD is $PWD"
npm run build
rm -rf ../static/js
cp -r ./dist/ ../static/js
cd ..
