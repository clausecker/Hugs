# Requires %defines of `name', `version' and `release'.
# (`make rpm' takes care of these - you aren't expected to
# use this spec directly)

Summary:      Hugs - A Haskell Interpreter
Vendor:       Galois Connections, Inc.
Name:         %{name}
Version:      %{version}
Release:      %{release}
License:      BSDish
Group:        Development/Languages/Haskell
Packager:     jeff@galois.com
URL:          http://www.haskell.org/hugs
Source:       %{name}-%{version}.tar.gz
BuildRoot:    %{_tmppath}/%{name}-buildroot
Provides:     haskell
Requires:     readline

%description
Hugs 98 is an interpreter for Haskell, a lazy functional programming language.

%prep
%setup -q

%build
( cd src/unix
  ./configure --prefix=%{_prefix} --mandir=%{_mandir} ${EXTRA_CONFIGURE_OPTS} )
make -C src
make -C docs/users_guide html

%install
rm -rf $RPM_BUILD_ROOT

make -C src DESTDIR=${RPM_BUILD_ROOT} install_rpm

%files
%defattr(-,root,root)
%doc Credits
%doc License
%doc Readme
%doc docs/ffi-notes.txt
%doc docs/libraries-notes.txt
%doc docs/machugs-notes.txt
%doc docs/packages.txt
%doc docs/server.html
%doc docs/server.tex
%doc docs/winhugs-notes.txt
%doc docs/users_guide/users_guide
%{_mandir}/man1/hugs.1.gz
%{_mandir}/man1/hugs-package.1.gz
%{prefix}/bin/ffihugs
%{prefix}/bin/hugs
%{prefix}/bin/hugs-package
%{prefix}/bin/runhugs
%{prefix}/lib/hugs/demos
%{prefix}/lib/hugs/include
%{prefix}/lib/hugs/libraries
%{prefix}/lib/hugs/oldlib
%{prefix}/lib/hugs/tools
