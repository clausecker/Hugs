%define version    Apr2002
%define patchlevel 1
%define prefix     /usr

Vendor:       Galois Connections, Portland, Oregon.
Name:         hugs98
Version:      %{version}
License:      BSDish
Release:      %{patchlevel}
Group:        Development/Languages/Haskell
Packager:     jeff@galois.com
URL:          http://www.haskell.org/hugs
Source:       http://cvs.haskell.org/Hugs/downloads/hugs98-%{version}.tar.gz
Provides:     haskell
Requires:     readline
BuildRoot:    /tmp/hugs98
Summary:      A Haskell Interpreter

%description
Hugs 98 is an interpreter for Haskell, a lazy functional programming language.

%prep
%setup -n hugs98

%build
cd src/unix
./configure --prefix=%{prefix} --with-readline
cd ..
make

%install
cd src
make prefix=$RPM_BUILD_ROOT%{prefix} install
gzip -f -9 $RPM_BUILD_ROOT%{prefix}/man/man1/hugs.1

%files
%attr(-, root, root) %doc License
%attr(-, root, root) %doc Readme
%attr(-, root, root) %doc Credits
%attr(-, root, root) %doc docs/server.html
%attr(-, root, root) %doc docs/observe-notes.txt
%attr(-, root, root) %doc docs/mdo-notes.txt
%attr(-, root, root) %doc docs/ffi-notes.txt
%attr(-, root, root) %doc docs/zipcomp-notes.txt
%attr(-, root, root) %doc docs/winhugs-notes.txt
%attr(-, root, root) %doc docs/machugs-notes.txt
%attr(-, root, root) %{prefix}/man/man1/hugs.1.gz
%attr(-, root, root) %{prefix}/bin/hugs
%attr(-, root, root) %{prefix}/bin/runhugs
%attr(-, root, root) %{prefix}/share/hugs/demos
%attr(-, root, root) %{prefix}/share/hugs/lib
%attr(-, root, root) %{prefix}/share/hugs/include
