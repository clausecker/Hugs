Vendor:       PacSoft, Portland, Oregon.
Name:         hugs98
Version:      Feb2000
License:      BSDish
Release:      2
Group:        Development/Languages/Haskell
Packager:     jlewis@cse.ogi.edu
URL:          http://www.haskell.org/hugs
Source:       http://www.cse.ogi.edu/PacSoft/projects/Hugs/downloads/hugs98-Feb2000.tar.gz
BuildRoot:    /tmp/hugs98
Summary:      A Haskell Interpreter
%description
Hugs 98 is an interpreter for Haskell, a lazy functional programming
language.  It is mostly compliant with the Haskell 98 standard, differing
mostly in some minor details of the module system.

%prep
%setup

%build
cd src/unix
./configure --prefix=/usr --with-readline
cd ..
make

%install
rm -f $RPM_BUILD_ROOT/usr/man/man1/hugs.1.gz
mkdir -p $RPM_BUILD_ROOT/usr/man/man1
cp docs/hugs.1 $RPM_BUILD_ROOT/usr/man/man1
gzip $RPM_BUILD_ROOT/usr/man/man1/hugs.1
cd src
make prefix=$RPM_BUILD_ROOT/usr install

%files
%attr(-, root, root) %doc License
%attr(-, root, root) %doc Readme
%attr(-, root, root) %doc docs/server.html
%attr(-, root, root) %doc docs/windows-notes.txt
%attr(-, root, root) /usr/man/man1/hugs.1.gz
%attr(-, root, root) /usr/bin/hugs
%attr(-, root, root) /usr/bin/runhugs
%attr(-, root, root) /usr/share/hugs/demos
%attr(-, root, root) /usr/share/hugs/lib
