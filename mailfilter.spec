Summary: Utilities for mh mail filtering
Name: mailfilter
Version: %{version}
Release: %{release}
License: Commerical
Group: Applications/Internet
#URL: 
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

# Don't attempt to make the debuginfo package
%define debug_package %{nil}

%description

mailstatus, folderfilter, and incfilter.  
Utilities for mh mail filtering.

%prep
%setup -q

%build
make

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/usr/fi/mailfilter
for program in mailstatus incfilter folderfilter; do
	cp -rp $program/* $RPM_BUILD_ROOT/usr/fi/mailfilter
	ln -s /usr/fi/mailfilter/$program $RPM_BUILD_ROOT/usr/fi/$program
done



%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
/usr/fi/mailfilter
/usr/fi/mailstatus
/usr/fi/incfilter
/usr/fi/folderfilter
%doc


%changelog
* Mon Sep 25 2006 Ahmon Dancy <dancy@dancy> - 
- Initial build.

