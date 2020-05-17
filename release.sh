#!/usr/bin/env bash

if [ $# -ne 1 ]; then
    cat <<EOF
Use: $0 <VERSION>"
- VERSION    version to release (0.1.6 for example)

To install the token, execute the install-marmalade-token.sh.
EOF
    exit 1;
fi

WDIR=$(dirname $0)

VERSION=$1

# launched from the current dev branch

git fetch -p --all

git checkout master

git merge origin/master

git tag -a -s $VERSION

git push origin --tag

make package
