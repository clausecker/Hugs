Vendor:       PacSoft, Portland, Oregon.
Name:         hugs98
Version:      Nov1999
License:      BSDish
Release:      1
Group:        Development/Languages/Haskell
Packager:     jlewis@cse.ogi.edu
URL:          http://www.haskell.org/hugs
Source:       http://www.cse.ogi.edu/PacSoft/projects/Hugs/downloads/hugs98-Nov1999.tar.gz
BuildRoot:    /tmp/hugs98
Summary:      A Haskell Interpreter
%description
Hugs is a Haskell interpreter and programming environment for
developing cool Haskell programs. This release is largely conformant
with Haskell 98, including monad and record syntax, newtypes,
strictness annotations, and modules.  In addition, it comes packaged
with the libraries defined in the most recent version of the Haskell
Library Report and with extension libraries which are compatible with
GHC 3.0 and later.

Hugs is best used as a Haskell program development system: it boasts
extremely fast compilation, supports incremental compilation, and has
the convenience of an interactive interpreter (within which one can
move from module to module to test different portions of a program).
However, being an interpreter, it does not nearly match the run-time
performance of, for example, GHC or HBC.

Authors:
--------
    Mark P Jones <mpj@cse.ogi.edu>
    Alastair Reid <reid@cs.utah.edu>

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
