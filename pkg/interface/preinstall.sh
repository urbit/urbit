#!/bin/sh
cd ../npm

for i in $(find . -type d -maxdepth 1) ; do
    packageJson="${i}/package.json"
    if [ -f "${packageJson}" ]; then
        echo "installing ${i}..."
        cd ./${i}
        npm ci
        cd ..
    fi
done