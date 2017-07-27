# hnormalise - a log normalisation library
#
# Copyright Andy Georges (c) 2017
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above
# copyright notice, this list of conditions and the following
# disclaimer in the documentation and/or other materials provided
# with the distribution.
#
# * Neither the name of Author name here nor the names of other
# contributors may be used to endorse or promote products derived
# from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

Summary: Log normalisation tool
Name: hnormalise
Version: 0.4.4.0
Release: 1

Group: Applications/System
License: BSD3
URL: htts://github.com/itkovian/hnormalise
Source0: %{name}-%{version}.tar.gz

BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot
BuildArch: x86_64

Requires: czmq >= 3.0.2, zeromq >= 4.1.4

%description
hnormalise is a log normalisation tool.

%prep
%setup -q


%build


%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/usr/bin
mkdir -p $RPM_BUILD_ROOT/etc/hnormalise
mkdir -p $RPM_BUILD_ROOT/etc/systemd/system/

#install usr/bin/hnormalise $RPM_BUILD_ROOT/usr/bin/hnormalise
#install etc/hormalise/hnormalise.yaml $RPM_BUILD_ROOT/etc/hnormalise/hnormalise.yaml

cp -a * %{buildroot}

%post
systemctl daemon-reload

%clean
#rm -rf ~/.stack ~/.stack-work

%files
%defattr(750,root,root,-)
/usr/bin/hnormalise

%defattr(640,root,root,-)
/etc/hnormalise/hnormalise.yaml
/etc/systemd/system/hnormalise.service


%changelog
* Mon May 29 2017 Andy Georges <itkovian@gmail.com>
- Created spec file
