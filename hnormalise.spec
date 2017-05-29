Summary: Log normalisation tool
Name: hnormalise		
Version: 0.3.2.0
Release: 1

Group: Applications/System
License: BSD3
URL: htts://github.com/itkovian/hnormalise
Source0: %{name}-%{version}.tar.gz

BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot
BuildArch: x86_64

%description
hnormalise is a log normalisation tool.

%prep
%setup -q


%build


%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/usr/bin
mkdir -p $RPM_BUILD_ROOT/etc/hnormalise

#install usr/bin/hnormalise $RPM_BUILD_ROOT/usr/bin/hnormalise
#install etc/hormalise/hnormalise.yaml $RPM_BUILD_ROOT/etc/hnormalise/hnormalise.yaml

cp -a * %{buildroot}

%clean
#rm -rf ~/.stack ~/.stack-work

%files
%defattr(750,root,root,-)
/usr/bin/hnormalise

%defattr(640,root,root,-)
/etc/hnormalise/hnormalise.yaml


%changelog
* Mon May 29 2017 Andy Georges <itkovian@gmail.com>
- Created spec file
