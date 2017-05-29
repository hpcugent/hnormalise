#!/bin/bash

VERSION=$(stack query | grep version| cut -d"'" -f2)
TARBALL_DIR=hnormalise-${VERSION}

mkdir -p ~/rpmbuild/SOURCES
mkdir -p ~/rpmbuild/RPMS
mkdir -p ~/rpmbuild/SPECS
mkdir -p ~/rpmbuild/SRPMS



rm -rf ${TARBALL_DIR}

stack build
stack install

mkdir -p ${TARBALL_DIR}/usr/bin
mkdir -p ${TARBALL_DIR}/etc/hnormalise

cp ~/.local/bin/hnormalise ${TARBALL_DIR}/usr/bin/
cp data/hnormalise.yaml ${TARBALL_DIR}/etc/hnormalise/

tar zcvf hnormalise-${VERSION}.tar.gz ${TARBALL_DIR}
cp hnormalise-${VERSION}.tar.gz ~/rpmbuild/SOURCES
cp hnormalise.spec ~/rpmbuild/SPECS

ls -l ~/rpmbuild/SPECS
ls -l ~/rpmbuild/SOURCES

rpmbuild -ba ~/rpmbuild/SPECS/hnormalise.spec
