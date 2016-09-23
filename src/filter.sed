# Replace the marker NEWLINE by a '\n' character.
# This will work in Linux and OSX. *Keep it in two lines!*
#
s/NEWLINE/\
/g
#
# This is for removing empty lines
/^[[:space:]]*$/d
#
# Basically the following commands translates
# pre-processors within another preprocessor
# which isn't allowed.
# Hence a small workaround is needed.
#
# This is for removing empty and comment lines
/^$$/d;/^\!.*&/d
#
# This is for removing double hash and too much space
# Strictly not needed
s/[[:space:]]*\#\#[[:space:]]*\([^[:space:]]*\)/\1/g
#
# This is for changing the include statements
# to direct include statements, certain platforms
# requires you to do this kind of trick :(
s/[[:space:]]*\#\([^i][^[:space:]]*\)/"\1"/g
#
# This is for translating direct endif statements to
# preprocessor statements
#s/"endif"/\n\#endif/g
s/"endif"/\
\#endif/g
#
# In fortran one may use the // operator
# But C-preprocessors are hungry and gobbles them...
s:/ /://:g
