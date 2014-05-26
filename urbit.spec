Name:           urbit
Version:        0.2
Release:        1%{?dist}
Summary:        The Urbit environment

License:        Public domain
URL:            http://urbit.org/
Source0:        urbit.tar.gz

BuildRequires:  openssl-devel, ncurses-devel, gmp-devel, libsigsegv-devel
Requires:       openssl, ncurses, gmp, libsigsegv

%description
Urbit is a new operating environment designed from scratch.

%prep
%setup -q

%build
make debbuild LIB=%{_datadir}

%install
rm -rf $RPM_BUILD_ROOT
make debinstall DESTDIR=$RPM_BUILD_ROOT


%files
%{_bindir}/vere
%{_datadir}/urb/*
%doc



%changelog
* Sun May 25 2014 urbit
- Package for Red Hat
