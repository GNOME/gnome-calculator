#!/bin/python

# Gcalctool Automated Tests
#
# $Header$
#
# Copyright (c) 1987-2006 Sun Microsystems, Inc.
# All Rights Reserved.
#

"""Gcalctool Automated Tests.  This standalone script talks
directly with the AT-SPI Registry via its IDL interfaces.

It's based on the Orca tool play_keystrokes.py.

It will read gcalctool test calculation (read from standard input) 
and generate keyboard events for the gcalctool application.

To perform the gcalctool automated tests, follow these steps:

1) Run the runtests.py script in a terminal window.
   Results are written to standard output. For example:

       runtests.py > output.txt

2) Start the play_keystrokes.py script in a terminal window.
   The input file should be provided on standard input. For example:

       play_keystrokes.py < input.txt

3) Run gcalctool.

4) Give focus to gcalctool. You have five seconds to do this.

That's it! The tests will now be automatically run.

Press F12 to terminate the test run.
"""

import os
import signal
import sys
import time
import bonobo
import ORBit
import gtk

ORBit.load_typelib("Accessibility")
ORBit.CORBA.ORB_init()

import Accessibility
import Accessibility__POA

registry = bonobo.get_object("OAFIID:Accessibility_Registry:1.0",
                             "Accessibility/Registry")

debug = False


def start():
    """Starts event notification with the AT-SPI Registry.  This method
    only returns after 'stop' has been called.
    """

    bonobo.main()


def stop():
    """Stop event notification with the AT-SPI Registry.
    """

    bonobo.main_quit()


def shutdownAndExit(signum=None, frame=None):
    stop()


keycodeCache = {}

def getKeycode(keysym):
    """Converts an XKeysym string (e.g., 'KP_Enter') to a keycode that
    should match the event.hw_code for key events.

    Arguments:
    - keysym: a string that is a valid representation of an XKeysym.

    Returns an integer representing a key code that should match the
    event.hw_code for key events.
    """

    if not keycodeCache.has_key(keysym):
        keymap = gtk.gdk.keymap_get_default()
        entries = keymap.get_entries_for_keyval(
            gtk.gdk.keyval_from_name(keysym))
        if entries:
            keycodeCache[keysym] = entries[0][0]
        else:
            keycodeCache[keysym] = 0
    return keycodeCache[keysym]


def sendKey(d, token):
    """Converts an XKeysym event string to its hardware code and generates
    two keyboard events (pressed and released) for it.

    Arguments:
    - d: a handle to the Registry device event controller
    - token: an XKeysym string containing the event type.
    """

    global debug

    hw_code = getKeycode(token)
    if debug:
        sys.stderr.write("sendKey: token: %s hw_code: %d\n" % (token, hw_code))
    d.generateKeyboardEvent(hw_code, "", 0)
    d.generateKeyboardEvent(hw_code, "", 1)


def readAndSendInput():
    """Keep reading a line of text from standard input (which contains 
    a single gcalctool test), until all lines have been read. For each 
    line, it splits it into tokens, each of which is an XKeysym event 
    type. Two keyboard events are generated from each of these ("pressed" 
    and "released") until a Return event token has been found. After that 
    has been sent, a pair of Delete events are generated to clear the 
    display before the next line is read from standard input.

    Note that this routine throws a EOFError exception when there is no
    more input to read.
    """

    global debug, registry

    d = registry.getDeviceEventController()

    while True:
        line = raw_input()
        tokens = line.split()
        for i in range(0, len(tokens)):
            sendKey(d, tokens[i])
            if debug:
                sys.stderr.write("readAndSendInput: sleeping...\n")

            time.sleep(0.5)
            if debug:
                sys.stderr.write("readAndSendInput: waking...\n")

            if tokens[i] == "Return":
                break


def test():
    print "Give focus to gcalctool."
    print "You have five seconds."
    print
    time.sleep(5.0)
    print "Starting keyboard event generation."

    try:
        readAndSendInput()
    except EOFError:
        pass

    print "Completing keyboard event generation."


if __name__ == "__main__":
    import signal
    signal.signal(signal.SIGINT, shutdownAndExit)
    signal.signal(signal.SIGQUIT, shutdownAndExit)
    test()
