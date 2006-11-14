/<?xml/d
/^<!ENTITY %/,/>/d
/^%/d
/^<!ENTITY/ s/\.xml/.sgml/
/^<!DOCTYPE/,/\[/ {
	s/ XML / /
	s/[ 	]*"http:[^"]*"//
	/^$/d
}
s:/>:>:g
