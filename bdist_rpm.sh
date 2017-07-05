#!/bin/bash

VERSION=$(stack query | grep version| cut -d"'" -f2)
TARBALL_DIR=hnormalise-${VERSION}

mkdir -p ~/rpmbuild/SOURCES
mkdir -p ~/rpmbuild/RPMS
mkdir -p ~/rpmbuild/SPECS
mkdir -p ~/rpmbuild/SRPMS

function test_and_exit() {
    msg=$1
    exitcode=$?
    if [ $exitcode -ne 0 ]; then
            echo "$msg"
            exit $exitcode
    fi
}

rm -rf "${TARBALL_DIR}"

stack build; test_and_exit "stack build failed"
stack install; test_and_exit "stack install failed"

mkdir -p "${TARBALL_DIR}"/usr/bin;
mkdir -p "${TARBALL_DIR}"/etc/hnormalise

cp ~/.local/bin/hnormalise "${TARBALL_DIR}"/usr/bin/
cp data/hnormalise.yaml "${TARBALL_DIR}"/etc/hnormalise/

tar zcvf hnormalise-"${VERSION}".tar.gz "${TARBALL_DIR}"; test_and_exit "creating tarball failed"
cp hnormalise-"${VERSION}".tar.gz ~/rpmbuild/SOURCES; test_and_exit "copying tarball to SOURCES failed"
cp hnormalise.spec ~/rpmbuild/SPECS

ls -l ~/rpmbuild/SPECS
ls -l ~/rpmbuild/SOURCES

rpmbuild -ba ~/rpmbuild/SPECS/hnormalise.spec; test_and_exit "rpmbuild failed"
