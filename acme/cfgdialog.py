from __future__ import unicode_literals
import sys, locale
from dialog import Dialog
import configuration as conf

# This is almost always a good thing to do at the beginning of your programs.
locale.setlocale(locale.LC_ALL, '')

# Initialize a dialog.Dialog instance
d = Dialog(dialog="dialog")
d.set_background_title("A Simple Example")

d.msgbox("""\
This is a very simple example of a program using pythondialog.

Contrary to what is done in demo.py, the Dialog exit code for the Escape key \
is not checked after every call, therefore it is not so easy to exit from \
this program as it is for the demo. The goal here is to show basic \
pythondialog usage in its simplest form.

With not too old versions of dialog, the size of dialog boxes is \
automatically computed when one passes width=0 and height=0 to the \
widget call. This is the method used here in most cases.""",
         width=0, height=0, title="'msgbox' example")
