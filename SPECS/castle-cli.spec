Summary: FS cli
Name: castle-cli
Version:        %{buildver}
Release:        %{buildrev}
License: No
Group: Filesystem
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

Provides:       %{name}-%{changesetver}

BuildRequires: ocaml-utils
BuildRequires: ocaml-findlib
BuildRequires: ocaml-castle
BuildRequires: libcastle-devel

%define _use_internal_dependency_generator 0
%define __find_requires /usr/lib/rpm/ocaml-find-requires.sh
%define __find_provides /usr/lib/rpm/ocaml-find-provides.sh

%description
CLI for castle-fs

%prep
%setup -q -n %{name}

%build
make all

%install
rm -rf %{buildroot}
export DESTDIR=%{buildroot}
export OCAMLFIND_DESTDIR=%{buildroot}%{_libdir}/ocaml
export OCAMLFIND_LDCONF=ignore
mkdir -p $OCAMLFIND_DESTDIR
mkdir -p %{buildroot}/opt/acunu/castle/bin
make install BUILD_ROOT=%{buildroot}

%clean
rm -rf %{buildroot}

%files
%defattr(-,root,root,-)
/opt/acunu/castle/bin/objClient
/usr/bin/castle-cli

%changelog
* Wed Sep  8 2010 Andrew Suffield <asuffield@acunu.com> - %{buildver}-%{buildrev}
- Initial package
