%define name idx-tsunami
%define version 1.0.1
%define release 1

Summary: A distributed multi-protocol load testing tool.
URL: http://tsunami.idealx.org/
Name: %{name}
Version: %{version}
Release: %{release}
Source0: http://tsunami.idealx.org/dist/%{name}-%{version}.tar.gz
License: GPL
Vendor: IDEALX S.A.S.
Packager: Nicolas Niclausse <nicolas@niclux.org>
Group: Development/Tools
BuildArch: noarch
BuildRequires: erlang-otp, xmerl >= 0.19
Requires: erlang-otp >= R9C_0, xmerl >= 0.19
BuildRoot: %{_tmppath}/%{name}-buildroot
Prefix: %{_prefix}

%description
 IDX-Tsunami is a distributed load testing tool.
 It is protocol-independent and can currently be used to stress and
 benchmark HTTP and Jabber servers.
 It simulates user behaviour using an XML description file, reports
 many measurements in real time (statistics can be customized with
 transactions, and graphics generated using gnuplot).
 For HTTP, it supports 1.0 and 1.1, has a proxy mode to record
 sessions, supports GET and POST methods, Cookies, and Basic
 WWW-authentication. It also has support for SSL.
 .
 More information is available at http://tsunami.idealx.org/ .

%prep
%setup
%configure --target=i386-redhat-linux

make

%install
rm -rf $RPM_BUILD_ROOT
export DESTDIR=$RPM_BUILD_ROOT %makeinstall 
install -m644 CONTRIBUTORS $RPM_BUILD_ROOT/usr/share/doc/%{name}/
install -m644 README $RPM_BUILD_ROOT/usr/share/doc/%{name}/
install -m644 TODO $RPM_BUILD_ROOT/usr/share/doc/%{name}/
install -m644 COPYING $RPM_BUILD_ROOT/usr/share/doc/%{name}/
install -m644 CHANGES $RPM_BUILD_ROOT/usr/share/doc/%{name}/

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
/usr/bin/idx-tsunami
/usr/lib/erlang/lib
/usr/lib/idx-tsunami
/usr/share/idx-tsunami
%doc /usr/share/doc/idx-tsunami/*
%doc /usr/share/man/man1/idx-tsunami.1.gz

%changelog
* Thu Aug  18 2004 Nicolas Niclausse <nicolas.niclausse@niclux.org> 1.0.1-1
- new release
* Mon Aug  9 2004 Nicolas Niclausse <nicolas.niclausse@IDEALX.com> 1.0-1
- new release
* Mon Aug  9 2004 Nicolas Niclausse <nicolas.niclausse@IDEALX.com> 1.0.beta7-2
- fix doc 
* Mon Aug  9 2004 Nicolas Niclausse <nicolas.niclausse@IDEALX.com> 1.0.beta7-1
- initial rpm 

# end of file
